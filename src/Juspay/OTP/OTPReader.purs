module Juspay.OTP.OTPReader {-- (
    OtpRule(..)
  , Sms(..)
  , ProcessedSms(..)
  , Result(..)
  , noProcessed
  , getSmsReadPermission
  , requestSmsReadPermission
  , getOtp
  , smsReceiver'
  , smsPoller'
  , extractOtp
  , hashSms
  , unsafeGetOtp
  ) --} where

import Prelude

import Control.Monad.Aff (Aff, delay, effCanceler, makeAff, nonCanceler)
import Control.Monad.Aff.AVar (AVar, isFilled, makeEmptyVar, makeVar, putVar, readVar, status, takeVar, tryReadVar, tryTakeVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Control.Parallel (parallel, sequential)
import Data.Array (catMaybes, elem, filter, findMap, length, null, (!!))
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, oneOf)
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


-- | Type representing an SMS received using `smsReceiver` or `smsPoller`.
newtype Sms = Sms {
  from :: String,
  body :: String,
  time :: String
}

derive instance eqSms :: Eq Sms
derive instance newtypeSms :: Newtype Sms _
derive instance genericSms :: Generic Sms _
instance ordSms :: Ord Sms where
  compare (Sms sms1) (Sms sms2) = fromMaybe (compare sms1.time sms2.time) $
    compare <$> (fromString sms1.time) <*> (fromString sms2.time)
instance encodeSms :: Encode Sms where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeSms :: Decode Sms where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


foreign import getSmsReadPermission' :: forall e. Eff e Boolean
foreign import requestSmsReadPermission' :: forall e. (Boolean -> Eff e Unit) -> Eff e Unit
foreign import startSmsReceiver :: forall e. (String -> Eff e Unit) -> Eff e Unit
foreign import stopSmsReceiver :: forall e. Eff e Unit
foreign import startSmsRetriever :: forall e. (String -> Eff e Unit) -> Eff e Unit
foreign import stopSmsRetriever :: forall e. Eff e Unit
foreign import readSms :: forall e. String -> Eff e String
foreign import md5Hash :: String -> String
foreign import trackException :: String -> String -> Unit


newtype SmsReader e = SmsReader {
  getNextSms :: Aff e (Either Error (Array Sms))
}

derive instance newtypeSmsReader :: Newtype (SmsReader e) _


smsReceiver :: forall e. SmsReader e
smsReceiver = SmsReader { getNextSms }
  where
  getNextSms :: Aff e (Either Error (Array Sms))
  getNextSms = do
    smsString <- makeAff (\cb -> startSmsReceiver (Right >>> cb) *> pure (effCanceler stopSmsReceiver))
    let sms = (decodeAndTrack >>> hush >>> maybe [] id) smsString
    if length sms < 1
      then getNextSms
      else pure $ Right sms


smsPoller :: forall e. Milliseconds -> Milliseconds -> Eff e (SmsReader e)
smsPoller startTime frequency = do
  processedRef <- unsafeCoerceEff $ newRef []
  pure $ SmsReader { getNextSms: getNextSms processedRef }
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


-- | This code is not final. SMS Retriever native code hasn't been merged to
-- | godel-core master as of now. Take care when using it.
smsRetriever :: forall e. SmsReader e
smsRetriever = SmsReader { getNextSms }
  where
    getNextSms :: Aff e (Either Error (Array Sms))
    getNextSms = do
      s <- makeAff (\cb -> startSmsRetriever (Right >>> cb) *> pure (effCanceler stopSmsRetriever))
      case runExcept $ decodeJSON s of
        Right sms -> pure $ Right sms
        Left _ -> case s of
          "TIMEOUT" -> getNextSms
          err -> pure $ Left (error err)



getSmsReadPermission :: forall e. Eff e Boolean
getSmsReadPermission = liftEff $ getSmsReadPermission'

requestSmsReadPermission :: forall e. Aff e Boolean
requestSmsReadPermission = makeAff (\cb -> requestSmsReadPermission' (Right >>> cb) *> pure nonCanceler)


type OtpListener e = {
  getNextOtp :: Aff e (Either Error String),
  setOtpRules :: Array OtpRule -> Aff e Unit
}

getOtpListener :: forall e. Array (SmsReader e) -> Aff e (OtpListener e)
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

  pure { getNextOtp, setOtpRules }
  where
    oneOfAff :: forall f a. (Foldable f) => (Functor f) => f (Aff e a) -> Aff e a
    oneOfAff affs = sequential $ oneOf $ parallel <$> affs

    waitForSms :: Array (SmsReader e) -> Aff e (Either Error (Array Sms))
    waitForSms readers = oneOfAff $ (unwrap >>> _.getNextSms) <$> readers

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
