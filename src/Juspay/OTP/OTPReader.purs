module Juspay.OTP.OTPReader (
    Sms(..),
    SmsReader(..),
    smsReceiver,
    smsPoller,
    getSmsReadPermission,
    requestSmsReadPermission,
    OtpListener,
    getOtpListener,
    extractOtp
  ) where

import Prelude

import Control.Monad.Aff (Aff, delay, effCanceler, makeAff, nonCanceler)
import Control.Monad.Aff.AVar (AVar, isFilled, makeEmptyVar, makeVar, putVar, readVar, status, takeVar, tryReadVar, tryTakeVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Control.Parallel (parallel, sequential)
import Data.Array (any, catMaybes, elem, filter, findMap, length, null, (!!))
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, and, oneOf)
import Data.Foreign (MultipleErrors)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number (fromString)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..))
import Juspay.OTP.Rule (OtpRule(..))
import Unsafe.Coerce (unsafeCoerce)


-- | Type representing an SMS received using `smsReceiver` or `smsPoller`.
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


foreign import getSmsReadPermission' :: forall e. Eff e Boolean
foreign import requestSmsReadPermission' :: forall e. (Boolean -> Eff e Unit) -> Eff e Unit
foreign import startSmsReceiver :: forall e. (String -> Eff e Unit) -> Eff e Unit
foreign import stopSmsReceiver :: forall e. Eff e Unit
-- foreign import startSmsRetriever :: forall e. (String -> Eff e Unit) -> Eff e Unit
-- foreign import stopSmsRetriever :: forall e. Eff e Unit
foreign import readSms :: forall e. String -> Eff e String
foreign import md5Hash :: String -> String
foreign import trackException :: String -> String -> Unit

-- | This type represents a method of reading incoming SMSs from the OS. If newer
-- | methods of reading SMSs need to be created, use this type
newtype SmsReader = SmsReader (forall e. Aff e (Either Error (Array Sms)))

-- | Checks if Android SMS Read permission has been granted
getSmsReadPermission :: forall e. Eff e Boolean
getSmsReadPermission = liftEff $ getSmsReadPermission'

-- | Requests Android SMS Read permission from the user
requestSmsReadPermission :: forall e. Aff e Boolean
requestSmsReadPermission = makeAff (\cb -> requestSmsReadPermission' (Right >>> cb) *> pure nonCanceler)


-- | Capture incoming SMSs by registering a Broadcast Receiver for SMS_RECEIVED
-- | action. This requires SMS permission to work
smsReceiver :: SmsReader
smsReceiver = SmsReader getNextSms
  where
  getNextSms :: forall e. Aff e (Either Error (Array Sms))
  getNextSms = do
    smsString <- makeAff (\cb -> startSmsReceiver (Right >>> cb) *> pure (effCanceler stopSmsReceiver))
    let sms = (decodeAndTrack >>> hush >>> maybe [] id) smsString
    if length sms < 1
      then getNextSms
      else pure $ Right sms

-- | Capture incoming SMSs by polling the SMS inbox at regular intervals. The
-- | first argument specifies the earliest time from which SMSs should be read
-- | (eg: session start time or time just before OTP trigger). The second
-- | argument specifies the frequency with which the poller should run (suggested
-- | frequency: 2 seconds)
smsPoller :: forall e. Milliseconds -> Milliseconds -> Eff e SmsReader
smsPoller startTime frequency = do
  processedRef <- unsafeCoerceEff $ newRef []
  pure $ SmsReader (unsafeCoerceAff $ getNextSms processedRef)
  where
    getNextSms :: Ref (Array String) -> Aff e (Either Error (Array Sms))
    getNextSms processedRef = do
      delay frequency
      smsString <- liftEff $ readSms $ show (unwrap startTime)
      processed <- unsafeCoerceAff $ liftEff $ readRef processedRef
      let sms = filter (notProcessed processed) $ (decodeAndTrack >>> hush >>> fromMaybe []) smsString
      unsafeCoerceAff $ liftEff $ writeRef processedRef $ processed <> (hashSms <$> sms)
      if length sms < 1
        then getNextSms processedRef
        else do pure $ Right sms

    getSmsTime :: Sms -> Maybe Milliseconds
    getSmsTime (Sms sms) = Milliseconds <$> fromString sms.time

    hashSms :: Sms -> String
    hashSms (Sms sms) = md5Hash $ sms.body <> sms.time

    notProcessed :: Array String -> Sms -> Boolean
    notProcessed processed sms = not $ elem (hashSms sms) processed


-- smsRetriever :: forall e. SmsReader
-- smsRetriever = SmsReader { getNextSms }
--   where
--     getNextSms :: Aff e (Either Error (Array Sms))
--     getNextSms = do
--       s <- makeAff (\cb -> startSmsRetriever (Right >>> cb) *> pure (effCanceler stopSmsRetriever))
--       case runExcept $ decodeJSON s of
--         Right sms -> pure $ Right sms
--         Left _ -> case s of
--           "TIMEOUT" -> getNextSms
--           err -> pure $ Left (error err)


-- | Return type of `getOtpListener` function.
-- |
-- | The `getNextOtp` function blocks until an OTP is received. It uses the list
-- | of SmsReaders passed to `getOtpListener` to caputre incoming SMSs and
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
  getNextOtp :: forall e. Aff e (Either Error String),
  setOtpRules :: forall e. Array OtpRule -> Aff e Unit
}

-- | Takes an array of `SmsReader`s and returns functions to get OTPs. It uses the
-- | supplied `SmsReader`s  by running them in parallel to capture any incoming
-- | SMSs and attempts to extract an OTP from them using given OTP rules. Check
-- | the `OtpListener` type for more info on how to get OTPs and set OTP rules.
getOtpListener :: forall e. Array SmsReader -> Aff e OtpListener
getOtpListener readers = do
  otpRulesVar <- unsafeCoerceAff makeEmptyVar
  unprocessedSmsVar <- unsafeCoerceAff $ makeVar []

  let
    setOtpRules rules = unsafeCoerceAff $ tryTakeVar otpRulesVar *> putVar rules otpRulesVar

    getNextOtp = do
      otpRulesSet <- isOtpRulesSet otpRulesVar
      smsList <- if otpRulesSet
          then waitForSms readers
          else oneOfAff [waitForSms readers, waitForOtpRules otpRulesVar *> getUnprocessedSms unprocessedSmsVar]
      otpRules <- getOtpRules otpRulesVar
      case smsList, otpRules of
        Left err, _ -> pure $ Left err
        Right s , Nothing -> addToUnprocessed unprocessedSmsVar s *> getNextOtp
        Right s, Just rules -> maybe (getNextOtp) (Right >>> pure) $ extractOtp s rules
  -- unsafeCoerce is only used to silence escaped scope issues. Did I mention I really hate effects? (This isn't an issue in purescript v12)
  unsafeCoerce $ unsafeCoerceAff $ pure {
    getNextOtp,
    setOtpRules
  }
  where
    oneOfAff :: forall f a. (Foldable f) => (Functor f) => f (Aff e a) -> Aff e a
    oneOfAff affs = sequential $ oneOf $ parallel <$> affs

    waitForSms :: Array SmsReader -> Aff e (Either Error (Array Sms))
    waitForSms readers = oneOfAff $ (\(SmsReader r) -> r) <$> readers

    waitForOtpRules :: AVar (Array OtpRule) -> Aff e (Array OtpRule)
    waitForOtpRules otpRulesVar = unsafeCoerceAff $ readVar otpRulesVar

    getOtpRules :: AVar (Array OtpRule) -> Aff e (Maybe (Array OtpRule))
    getOtpRules otpRulesVar = unsafeCoerceAff $ tryReadVar otpRulesVar

    getUnprocessedSms :: AVar (Array Sms) -> Aff e (Either Error (Array Sms))
    getUnprocessedSms unprocessedSmsVar = unsafeCoerceAff $ Right <$> readVar unprocessedSmsVar

    isOtpRulesSet :: AVar (Array OtpRule) -> Aff e Boolean
    isOtpRulesSet otpRulesVar = unsafeCoerceAff $ isFilled <$> status otpRulesVar

    addToUnprocessed :: AVar (Array Sms) -> Array Sms -> Aff e Unit
    addToUnprocessed unprocessedSmsVar sms = unsafeCoerceAff do
      unprocessed <- takeVar unprocessedSmsVar
      putVar (unprocessed <> sms) unprocessedSmsVar


-- | Given a list of SMSs and a list of OTP rules, it will return the first OTP
-- | that matches one of the given rules or `Nothing` if none of them match.
extractOtp :: Array Sms -> Array OtpRule -> Maybe String
extractOtp sms rules =
  findMap (\rule -> findMap (matchAndExtract rule) sms) rules


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


-- | Used for tracking decode errors for values that should never have failed
-- | a decode (such as `Sms` values retreived from Android or `OtpRule`s)
decodeAndTrack :: forall a. Decode a => String -> Either MultipleErrors a
decodeAndTrack s = case runExcept $ decodeJSON s of
  Right a -> Right a
  Left err -> let _ = trackException "otp_reader" ("decode exception: " <> show err) in Left err
