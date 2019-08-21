module Juspay.OTP.Reader (
    Sms(..),
    OtpRule(..),
    getGodelOtpRules,
    SmsReader(..),
    getName,
    smsReceiver,
    smsPoller,
    clipboard,
    getSmsReadPermission,
    requestSmsReadPermission,
    Otp(..),
    OtpError(..),
    OtpListener,
    getOtpListener,
    extractOtp
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Data.Array (catMaybes, elem, filter, filterA, findMap, length, null)
import Data.Array.NonEmpty ((!!))
import Data.Either (Either(..), either, hush)
import Data.Foldable (class Foldable, oneOf)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (fromString)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, delay, effectCanceler, makeAff, nonCanceler)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (F, Foreign, MultipleErrors, readString)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.Index (readProp)


-- | Type representing an SMS received using any `SmsReader`s.
newtype Sms = Sms {
  from :: String,
  body :: String,
  time :: String
}

derive instance eqSms :: Eq Sms
derive instance newtypeSms :: Newtype Sms _
derive instance genericSms :: Generic Sms _

-- | Ordered by SMS time. If conversion of the time string to a Number fails,
-- | regular string ordering is used as a fallback
instance ordSms :: Ord Sms where
  compare (Sms sms1) (Sms sms2) = fromMaybe (compare sms1.time sms2.time) $
    compare <$> (fromString sms1.time) <*> (fromString sms2.time)
instance encodeSms :: Encode Sms where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeSms :: Decode Sms where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}



-- | Internal type used for encoding from and decoding to `OtpRule`.
newtype OtpRule' = OtpRule' {
  matches :: Matches,
  otp :: String,
  group :: Maybe Int
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
      group = r.group,
      matches = wrap r.matches
    })
instance decodeOtpRule :: Decode OtpRule where
  decode f = do
    (OtpRule' r) <- genericDecode defaultOptions{unwrapSingleConstructors = true} f
    pure $ OtpRule r{
      group = r.group,
      matches = unwrap r.matches
    }



foreign import getGodelOtpRules' :: Effect Foreign

-- | Gets bank OTP rules from Godel's config.
getGodelOtpRules :: String -> Effect (F (Array OtpRule))
getGodelOtpRules bank = do
  f <- getGodelOtpRules'
  pure $ do
    rules <- decode f
    bankRules <- filterA (matchesBank) rules
    traverse decode bankRules
  where
    matchesBank :: Foreign -> F Boolean
    matchesBank f = do
      b <- readProp "bank" f >>= readString
      pure $ b == bank




-- | This type represents a method of reading incoming SMSs from the OS. If newer
-- | methods of reading SMSs need to be created, use this type.
-- | The first parameter is the name of the SMS Reader as a String (useful for
-- | differentiating between SmsReaders`s). The second argument is the function
-- | to be used to wait for the next SMS.
data SmsReader = SmsReader String (Aff (Either Error (Array Sms)))

-- | Get the name of an `SmsReader`. Useful for differentiating between `SmsReader`s
getName :: SmsReader -> String
getName (SmsReader name _) = name


foreign import getSmsReadPermission' :: Effect Boolean

-- | Checks if Android SMS Read permission has been granted
getSmsReadPermission :: Effect Boolean
getSmsReadPermission = liftEffect $ getSmsReadPermission'



foreign import requestSmsReadPermission' :: (Boolean -> Effect Unit) -> Effect Unit

-- | Requests Android SMS Read permission from the user
requestSmsReadPermission :: Aff Boolean
requestSmsReadPermission = makeAff (\cb -> requestSmsReadPermission' (Right >>> cb) *> pure nonCanceler)



foreign import startSmsReceiver :: (String -> Effect Unit) -> Effect Unit
foreign import stopSmsReceiver :: Effect Unit

-- | Capture incoming SMSs by registering an Android Broadcast Receiver for
-- | SMS_RECEIVED action. This requires SMS permission to work.
-- | Calling `getName` on this will return the string "SMS_RECEIVER".
smsReceiver :: SmsReader
smsReceiver = SmsReader "SMS_RECEIVER" getNextSms
  where
  getNextSms :: Aff (Either Error (Array Sms))
  getNextSms = do
    smsString <- makeAff (\cb -> startSmsReceiver (Right >>> cb) *> pure (effectCanceler stopSmsReceiver))
    let sms = (decodeAndTrack >>> hush >>> maybe [] identity) smsString
    if length sms < 1
      then getNextSms
      else pure $ Right sms



foreign import readSms :: String -> Effect String
foreign import md5Hash :: String -> String

-- | Capture incoming SMSs by polling the SMS inbox at regular intervals. The
-- | first argument specifies the earliest time from which SMSs should be read
-- | (eg: session start time or time just before OTP trigger). The second
-- | argument specifies the frequency with which the poller should run (suggested
-- | frequency: 2 seconds). This requires SMS permission to work
-- | Calling `getName` on this will return the string "SMS_POLLER".
smsPoller :: Milliseconds -> Milliseconds -> Effect SmsReader
smsPoller startTime frequency = do
  processedRef <- Ref.new []
  pure $ SmsReader "SMS_POLLER" $ getNextSms processedRef
  where
    getNextSms :: Ref (Array String) -> Aff (Either Error (Array Sms))
    getNextSms processedRef = do
      delay frequency
      smsString <- liftEffect $ readSms $ show (unwrap startTime)
      processed <- liftEffect $ Ref.read processedRef
      let sms = filter (notProcessed processed) $ (decodeAndTrack >>> hush >>> fromMaybe []) smsString
      liftEffect $ Ref.write (processed <> (hashSms <$> sms)) processedRef
      if length sms < 1
        then getNextSms processedRef
        else do pure $ Right sms

    getSmsTime :: Sms -> Maybe Milliseconds
    getSmsTime (Sms sms) = Milliseconds <$> fromString sms.time

    hashSms :: Sms -> String
    hashSms (Sms sms) = md5Hash $ sms.body <> sms.time

    notProcessed :: Array String -> Sms -> Boolean
    notProcessed processed sms = not $ elem (hashSms sms) processed


foreign import onClipboardChange :: (String -> Effect Unit) -> Effect Unit
foreign import getCurrentTime :: Effect Number

-- | Capture incoming OTPs by listening for clipboard changes. The body could
-- | either be the the entire SMS body or the OTP itself. In both cases, the OTP
-- | should be extractable by `extractOtp`
-- | Calling `getName` on this will return the string "CLIPBOARD"
clipboard :: SmsReader
clipboard = SmsReader "CLIPBOARD" getNextSms
  where
    getNextSms :: Aff (Either Error (Array Sms))
    getNextSms = do
      smsString <- makeAff (\cb -> onClipboardChange (Right >>> cb) *> pure nonCanceler)
      currentTime <- liftEffect getCurrentTime
      let stringArray = (decodeAndTrack >>> hush >>> maybe [] identity) smsString
          sms = toSms currentTime <$> stringArray
      if length sms < 1
        then getNextSms
        else pure $ Right sms
    toSms :: Number -> String -> Sms
    toSms time body = Sms {
      from: "UNKNOWN_BANK",
      body,
      time: encodeJSON time
    }


-- | Internal type used to represent a received SMS and the `SmsReader` that
-- | captured it
data ReceivedSms = ReceivedSms Sms SmsReader

-- | Internal type used as the monad inside `getNextOtp`
type OtpM a = ExceptT OtpError Aff a



-- | Takes an array of `SmsReader`s and returns functions to get OTPs. It uses the
-- | supplied `SmsReader`s  by running them in parallel to capture any incoming
-- | SMSs and attempts to extract an OTP from them using given OTP rules. Check
-- | the `OtpListener` type for more info on how to get OTPs and set OTP rules.
getOtpListener :: Array SmsReader -> Aff OtpListener
getOtpListener readers = do
  otpRulesVar <- AVar.empty
  unprocessedSmsVar <- AVar.new []

  let
    setOtpRules rules = AVar.tryTake otpRulesVar *> AVar.put rules otpRulesVar

    getNextOtp = do
      otpRulesSet <- isOtpRulesSet otpRulesVar
      smsList <- if otpRulesSet
          then ExceptT $ waitForSms readers
          else ExceptT $ oneOfAff [
            waitForSms readers,
            waitForOtpRules otpRulesVar *> (Right <$> getUnprocessedSms unprocessedSmsVar)
          ]
      otpRules <- getOtpRules otpRulesVar
      case otpRules of
        Nothing -> addToUnprocessed unprocessedSmsVar smsList *> getNextOtp
        Just rules -> maybe getNextOtp pure $ findMap (tryExtract rules) smsList
  pure {
    getNextOtp: (either Error identity) <$> runExceptT getNextOtp,
    setOtpRules
  }
  where
    oneOfAff :: forall f a. (Foldable f) => (Functor f) => f (Aff a) -> Aff a
    oneOfAff affs = sequential $ oneOf $ parallel <$> affs

    waitForSms :: Array SmsReader -> Aff (Either OtpError (Array ReceivedSms))
    waitForSms smsReaders = oneOfAff $ smsReaders <#> (\reader@(SmsReader _ r) -> do
        res <- r
        pure $ case res of
          Left err -> Left (SmsReaderError err reader)
          Right smses -> Right $ (\sms -> ReceivedSms sms reader) <$> smses
      )

    waitForOtpRules :: AVar (Array OtpRule) -> Aff (Array OtpRule)
    waitForOtpRules otpRulesVar = AVar.read otpRulesVar

    getOtpRules :: AVar (Array OtpRule) -> OtpM (Maybe (Array OtpRule))
    getOtpRules otpRulesVar = lift $ AVar.tryRead otpRulesVar

    getUnprocessedSms :: AVar (Array ReceivedSms) -> Aff (Array ReceivedSms)
    getUnprocessedSms unprocessedSmsVar = AVar.read unprocessedSmsVar

    isOtpRulesSet :: AVar (Array OtpRule) -> OtpM Boolean
    isOtpRulesSet otpRulesVar = lift $ AVar.isFilled <$> AVar.status otpRulesVar

    addToUnprocessed :: AVar (Array ReceivedSms) -> Array ReceivedSms -> OtpM Unit
    addToUnprocessed unprocessedSmsVar sms = lift do
      unprocessed <- AVar.take unprocessedSmsVar
      AVar.put (unprocessed <> sms) unprocessedSmsVar

    tryExtract :: Array OtpRule -> ReceivedSms -> Maybe Otp
    tryExtract rules (ReceivedSms sms reader) = (\otp -> Otp otp sms reader) <$> extractOtp sms rules



-- | Return type of `getOtpListener` function.
-- |
-- | The `getNextOtp` function blocks until an OTP is received. It uses the list
-- | of `SmsReader`s passed to `getOtpListener` to caputre incoming SMSs and
-- | returns the first one to match an `OtpRule`.
-- |
-- | The `setOtpRules` function is used to set the OtpRules that should be used
-- | for attempting to exctract an OTP from any incoming SMSs.
-- |
-- | At first, no OTP rules are set and calling `getNextOtp` will not return any
-- | OTPs. Instead any incoming SMSs will be queued until `setOtpRules` is called
-- | for the first time at which point, the queued up SMSs will be validated and
-- | an OTP returned if any of them match any rule.
type OtpListener = {
  getNextOtp :: Aff Otp,
  setOtpRules :: Array OtpRule -> Aff Unit
}


-- | Return type of `OtpListener.getNextOtp`.
-- |
-- | In case of success, it provides the OTP string, the SMS from which that OTP
-- | was extracted and the `SmsReader` that captured that SMS. (You can use
-- | `getName` to get the name of the SMS Reader that caputed the SMS).
-- |
-- | In case of an error, it provides an `OtpError` type.
data Otp = Otp String Sms SmsReader | Error OtpError

-- | Represents an error that occured during `OtpListener.getNextOtp`. It can
-- | either be an `Error` thrown by one of the `SmsReader`s or some other generic
-- | `Error`. In case of an `SmsReader` error, the `SmsReader` that threw the
-- | error is also provided. (You can use `getName` to get the name of the SMS
-- | Reader that threw the error).
data OtpError = SmsReaderError Error SmsReader | OtherError Error

instance showOtpError :: Show OtpError where
  show (SmsReaderError e _) = show e
  show (OtherError e) = show e


-- | Given an SMS and a list of OTP rules, it will return the first OTP
-- | that matches one of the given rules or `Nothing` if none of them match.
extractOtp :: Sms -> Array OtpRule -> Maybe String
extractOtp sms rules =
  findMap (\rule -> matchAndExtract rule sms) rules



-- | Match a given SMS against a given rule and attempt to extract the OTP
-- | from the SMS. Returns `Nothing` if it fails.
matchAndExtract :: OtpRule -> Sms -> Maybe String
matchAndExtract (OtpRule rule) sms =
  matchSender sms >>= matchMessage >>= extract
  where
    matchSender :: Sms -> Maybe Sms
    matchSender (Sms sms') =
      let
        senderRules = catMaybes $ makeRegex <$> rule.matches.sender
        matches = not null $ catMaybes $ (\r -> match r sms'.from) <$> senderRules
        fromRetrieverApi = sms'.from == "UNKNOWN_BANK"
      in if matches || fromRetrieverApi then Just (Sms sms') else Nothing

    matchMessage :: Sms -> Maybe Sms
    matchMessage (Sms sms') =
      let
        matchesBody = isJust $ makeRegex rule.matches.message >>= (\r -> match r sms'.body)
        matchesOtp = isJust $ makeRegex ("^" <> rule.otp <> "$") >>= (\r -> match r sms'.body) -- if the whole message body is the OTP (like from clipboard)
      in if matchesBody || matchesOtp then Just (Sms sms') else Nothing

    extract :: Sms -> Maybe String
    extract (Sms sms') =
      let
        group = fromMaybe 0 rule.group
        otp = join $ makeRegex rule.otp >>= (\r -> match r sms'.body) >>= (\arr -> arr !! group)
      in otp

    makeRegex :: String -> Maybe Regex
    makeRegex s = case regex s (ignoreCase) of
      Right r -> Just r
      Left err -> let _ = trackException "otp_reader" ("Regex syntax error \"" <> err <> "\" for rule: " <> encodeJSON (OtpRule rule)) in Nothing



foreign import trackException :: String -> String -> Unit

-- | Used for tracking decode errors for values that should never have failed
-- | a decode (such as `Sms` values retreived from Android or `OtpRule`s)
decodeAndTrack :: forall a. Decode a => String -> Either MultipleErrors a
decodeAndTrack s = case runExcept $ decodeJSON s of
  Right a -> Right a
  Left err -> let _ = trackException "otp_reader" ("decode exception: " <> show err) in Left err
