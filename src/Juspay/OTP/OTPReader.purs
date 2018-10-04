module Juspay.OTP.OTPReader (
    OtpRule(..)
  , Sms(..)
  , ProcessedSms(..)
  , Result(..)
  , noProcessed
  , getSmsReadPermission
  , requestSmsReadPermission
  , getOtp
  , smsReceiver
  , smsPoller
  , extractOtp
  , hashSms
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, cons, elem, findMap, length, null, (!!))
import Data.Either (Either(..), hush)
import Data.Foreign (MultipleErrors)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds)
import Juspay.Compat (Aff, Eff, delay, liftEff, makeAff, match, parAff)
import Juspay.Compat (id) as Compat -- To prevent shadow definitions warning for `id` from `Prelude`

-- | Internal type used for encoding from and decoding to `OtpRule`.
newtype OtpRule' = OtpRule' {
  matches :: Matches,
  otp :: String,
  group :: NullOrUndefined Int
}

derive instance newtypeOtpRule' :: Newtype OtpRule' _
derive instance genericOtpRule' :: Generic OtpRule' _

-- | Internal type used for encoding from and decoding to `OtpRule`.
newtype Matches = Matches {
  sender :: Array String,
  message :: String
}

derive instance newtypeMatches :: Newtype Matches _
derive instance genericMatches :: Generic Matches _
instance encodeMatches :: Encode Matches where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeMatches :: Decode Matches where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


-- | Represents a rule for matching and extracting an OTP. An SMS will
-- | successfully match a given `OtpRule` if the following are true:
-- |  1. `message`, `otp` and the strings in the `sender` array are all
-- |    valid regex strings.
-- |  2. The SMS body matches the `message` regex
-- |  3. The SMS 'from' field matches one of the `sender` regexes
-- |  4. The OTP part of the SMS body successfuly matches the `otp` regex
-- |  5. If the `otp` regex contains groups, the `group` value references
-- |    a valid group matched by the `otp` regex. (defaults to 0 if `Nothing`)
newtype OtpRule = OtpRule {
  matches :: {
    sender :: Array String,
    message :: String
  },
  otp :: String,
  group :: Maybe Int
}

derive instance newtypeOtpRule :: Newtype OtpRule _
derive instance genericOtpRule :: Generic OtpRule _
instance encodeOtpRule :: Encode OtpRule where
  encode (OtpRule r) = genericEncode defaultOptions {unwrapSingleConstructors = true}
    (OtpRule' r{
      group = NullOrUndefined r.group,
      matches = wrap r.matches
    })
instance decodeOtpRule :: Decode OtpRule where
  decode f = do
    (OtpRule' r) <- genericDecode defaultOptions{unwrapSingleConstructors = true} f
    pure $ OtpRule r{
      group = unNullOrUndefined r.group,
      matches = unwrap r.matches
    }


-- | Type representing an SMS received using `smsReceiver` or `smsPoller`.
newtype Sms = Sms {
  from :: String,
  body :: String,
  time :: String
}

derive instance eqSms :: Eq Sms
derive instance newtypeSms :: Newtype Sms _
derive instance genericSms :: Generic Sms _
instance encodeSms :: Encode Sms where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeSms :: Decode Sms where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


-- | This type holds hashes of previously processed SMSs, i.e., an SMS
-- | that successfully matched a rule and had an OTP extracted from it.
-- | See `getOtp` for instructions on how to use it.
newtype ProcessedSms = ProcessedSms (Array String)

derive instance newtypeProcessedSms :: Newtype ProcessedSms _
derive instance genericProcessedSms :: Generic ProcessedSms _
instance encodeProcessedSms :: Encode ProcessedSms where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeProcessedSms :: Decode ProcessedSms where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}

-- | Empty list of processed SMSs. This should be passed to `getOtp`
-- | when it is called for the first time.
noProcessed :: ProcessedSms
noProcessed = ProcessedSms []


-- | Type representing the final response from `getOtp`. When an OTP has
-- | been successfully extracted from an SMS, the `MatchedOtp` constructer is
-- | returned. If an error occured, `Error` is returned.
data Result = MatchedOtp String ProcessedSms | Error String

derive instance genericResult :: Generic Result _
instance encodeResult :: Encode Result where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeResult :: Decode Result where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


foreign import getSmsReadPermission' :: forall e. Eff e Boolean
foreign import requestSmsReadPermission' :: forall e. (Boolean -> Eff e Unit) -> Eff e Unit
foreign import startSmsReceiver :: forall e. (String -> Eff e Unit) -> Eff e Unit
foreign import stopSmsReceiver :: forall e. Eff e Unit
foreign import readSms :: forall e. String -> Eff e String
foreign import md5Hash :: String -> String
foreign import trackException :: String -> String -> Unit


-- | Check if the app has been granted SMS Read permissions.
getSmsReadPermission :: forall e. Aff e Boolean
getSmsReadPermission = liftEff $ getSmsReadPermission'


-- | First check if the app has SMS Read permissions and if not, attempt
-- | to ask the user to grant the app SMS Read permissions. Returns false
-- | if the user declines.
requestSmsReadPermission :: forall e. Aff e Boolean
requestSmsReadPermission = do
  isGranted <- getSmsReadPermission
  if isGranted then pure true
    else makeAff requestSmsReadPermission'


-- | Waits until an SMS is received that matches one of the given `OtpRule`s.
-- | It works by starting an Android Broadcast Receiver and polling the
-- | SMS inbox at fixed intervals until an SMS is received that matches one
-- | of the supplied OTP rules.
-- |
-- | The first argument is the list of `OtpRule`s of which a received SMS
-- | should match at least one.
-- |
-- | The second argument should be a UNIX timestamp string in milliseconds
-- | which represents the start time the SMS poller should be interested in
-- | when polling for new SMSs.
-- |
-- | The third argument is the frequency at which the SMS poller should check
-- | the Inbox for new messages
-- |
-- | The fourth argument is a list of previously processed SMS hashes used
-- | to prevent a previously processed OTP from being processed again a second
-- | time. When running this function for the first time, pass `noProcessed`.
-- | When the first OTP is extracted from an SMS, the hash of that SMS will
-- | be returned along with the OTP as `ProcessedSms` in the `Result` value.
-- | The next time `getOtp` is called, the previously returned `ProcessedSms`
-- | value can be passed back to this function so that if the same SMS is
-- | found again by the SMS poller, it will be ignored. Every subsequent call
-- | of `getOtp` can be passed a `ProcessedSms` value from the previous run
getOtp :: forall e. Array OtpRule -> String -> Milliseconds -> ProcessedSms -> Aff e Result
getOtp rules startTime pollFrequency processed = do
  isGranted <- requestSmsReadPermission
  if not isGranted then pure $ Error "SMS Read permission not granted"
    else do
      sms <- parAff [smsReceiver, smsPoller startTime pollFrequency]
      case extractOtp sms rules processed of
        Just result -> pure result
        Nothing -> getOtp rules startTime pollFrequency processed


-- | Starts an Android Broadcast Receiver and waits till an SMS received event
-- | triggers. Returns an array of the new SMSs.
smsReceiver :: forall e. Aff e (Array Sms)
smsReceiver = do
  smsString <- makeAff startSmsReceiver
  liftEff $ stopSmsReceiver
  --TODO track sms receive event
  let sms = (decodeAndTrack >>> hush >>> maybe [] Compat.id) smsString
  if length sms < 0
    then smsReceiver
    else pure sms


-- | Given a start time (first argument), this function will poll the SMS
-- | inbox for messages on or after that start time and return them as an
-- | Array. If no SMSs after the given timestamp are found, it will sleep for
-- | the given time interval (second arguemnt) and try again until it finds at
-- | least 1 new SMS.
smsPoller :: forall e. String -> Milliseconds -> Aff e (Array Sms)
smsPoller startTime pollFrequency = do
  delay pollFrequency
  smsString <- liftEff $ readSms startTime
  --TODO track sms receive event
  let sms = (decodeAndTrack >>> hush >>> maybe [] Compat.id) smsString
  if length sms < 1
    then smsPoller startTime pollFrequency
    else pure sms


-- | Given a list of SMSs and a list of OTP rules, it will return the first OTP
-- | that matches one of the given rules or `Nothing` if none of them match.
extractOtp :: Array Sms -> Array OtpRule -> ProcessedSms -> Maybe (Result)
extractOtp sms rules processed =
  findMap (\rule -> findMap (matchAndExtract rule processed) sms) rules


-- | Match a given SMS against a given rule and attempt to extract the OTP
-- | from the SMS. Returns `Nothing` if it fails. Ignores the SMS if a hash of
-- | it is present in the given `ProcessedSms` object.
matchAndExtract :: OtpRule -> ProcessedSms -> Sms -> Maybe Result
matchAndExtract (OtpRule rule) (ProcessedSms processed) sms =
  matchSender sms >>= matchMessage >>= notProcessed >>= extract
  where
    matchSender :: Sms -> Maybe Sms
    matchSender (Sms sms') =
      let
        senderRules = catMaybes $ makeRegex <$> rule.matches.sender
        matches = not null $ catMaybes $ (\r -> match r sms'.from) <$> senderRules
      in if matches then Just (Sms sms') else Nothing

    matchMessage :: Sms -> Maybe Sms
    matchMessage (Sms sms') =
      let
        matches = isJust $ makeRegex rule.matches.message >>= (\r -> match r sms'.body)
      in if matches then Just (Sms sms') else Nothing

    notProcessed :: Sms -> Maybe Sms
    notProcessed sms' =
      if elem (hashSms sms') processed then Nothing else Just sms

    extract :: Sms -> Maybe Result
    extract (Sms sms') =
      let
        group = fromMaybe 0 rule.group
        otp = join $ makeRegex rule.otp >>= (\r -> match r sms'.body) >>= (\arr -> arr !! group)
      in case otp of
        Just otp' -> Just $ MatchedOtp otp' $ ProcessedSms $ hashSms (Sms sms') `cons` processed
        Nothing -> Nothing

    makeRegex :: String -> Maybe Regex
    makeRegex s = case regex s (ignoreCase) of
      Right r -> Just r
      Left err -> let _ = trackException "otp_reader" ("Regex syntax error \"" <> err <> "\" for rule: " <> encodeJSON (OtpRule rule)) in Nothing


-- | Used for tracking decode errors for values that should never have failed
-- | a decode (such as `Sms` values retreived from Android or `OtpRule`s)
decodeAndTrack :: forall a. Decode a => String -> Either MultipleErrors a
decodeAndTrack s = case runExcept $ decodeJSON s of
  Right a -> Right a
  Left err -> let _ = trackException "otp_reader" ("decode exception: " <> show err) in Left err

-- | Creates a hash value for a given SMS by MD5 hashing the SMS body and time
hashSms :: Sms -> String
hashSms (Sms s) = md5Hash $ s.body <> s.time
