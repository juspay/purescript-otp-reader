module Juspay.OTP.Reader (
    Sms(..),
    OtpRule(..),
    getGodelOtpRules,
    SmsReader(..),
    getName,
    smsReceiver,
    smsRetriever,
    smsPoller,
    initiateSMSRetriever,
    isConsentAPISupported,
    smsConsentAPI,
    isConsentDeniedError,
    isClipboardSupported,
    clipboard,
    getSmsReadPermission,
    requestSmsReadPermission,
    Otp(..),
    OtpError(..),
    OtpListener,
    getOtpListener,
    extractOtp,
    PermissionResult(..),
    trackAction,
    isSmsPermissionGranted
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Data.Array (catMaybes, elem, filter, filterA, findMap, length, null, singleton)
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Either (Either(..), either, hush, fromRight)
import Data.Foldable (class Foldable, oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Number (fromString)
import Data.Ord (greaterThan)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (ignoreCase, global)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, delay, effectCanceler, error, makeAff, nonCanceler)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Exception (Error, message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (F, Foreign, MultipleErrors, readString, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, decode)
import Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.Index (readProp)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Tracker (trackAction, trackContext, trackException) as Tracker
import Tracker.Labels (Label(..)) as Tracker
import Tracker.Types (Level(..), Action(..), Context(..), ACTION'(..)) as Tracker

-- | Type representing an SMS received using any `SmsReader`s.
newtype Sms = Sms {
  from :: String,
  body :: String,
  time :: String
}

derive instance eqSms :: Eq Sms
derive instance newtypeSms :: Newtype Sms _
derive instance genericSms :: Generic Sms _
instance showSms :: Show Sms where
  show = genericShow

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
instance showOtpRule :: Show OtpRule where
  show = genericShow
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

foreign import trackAction :: ∀ a. a -> String -> Effect Unit
foreign import trackException :: String -> String -> Unit
foreign import getGodelOtpRulesImpl :: Effect Foreign
foreign import replaceDigitWithX :: String -> String

-- | Gets bank OTP rules from Godel's config.
getGodelOtpRules :: String -> Effect (F (Array OtpRule))
getGodelOtpRules bank = do
  f <- getGodelOtpRulesImpl
  pure $ do
    rules <- decode f
    bankRules <- filterA (matchesBank) rules
    let isOtpRulesAvailable = not $ null bankRules
    _ <- pure $ trackAction {is_otp_rules_available: isOtpRulesAvailable, service: "getGodelOtpRules" } "otp"
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
derive instance smsReader :: Generic SmsReader _
instance showSmsReader :: Show SmsReader where
  show (SmsReader x _) = "SmsReader" <> x

-- | Get the name of an `SmsReader`. Useful for differentiating between `SmsReader`s
getName :: SmsReader -> String
getName (SmsReader name _) = name


foreign import getSmsReadPermissionImpl :: Effect Boolean

-- | Checks if Android SMS Read permission has been granted
getSmsReadPermission :: Effect Boolean
getSmsReadPermission = do
  a <- getSmsReadPermissionImpl
  pure a

foreign import isSmsPermissionGranted :: Effect Boolean

getSmsPermissionStatus :: Effect PermissionResult
getSmsPermissionStatus = do
  permissionGranted <- isSmsPermissionGranted
  shouldShowRationale <- toMaybe <$> shouldShowRequestPermissionRationale
  let fnName = "getSmsPermissionStatus"
  case permissionGranted, shouldShowRationale of
    true , _          -> do
      _ <- liftEffect $ trackAction {smsReadPermission: (show Granted), service: fnName } "otp_info"
      pure Granted
    false, Just false -> do
      _ <- liftEffect $ trackAction {smsReadPermission: (show DeniedNeverAskAgain), service: fnName } "otp_info"
      pure DeniedNeverAskAgain
    false, _          -> do
      _ <- liftEffect $ trackAction {smsReadPermission: (show Denied), service: fnName } "otp_info"
      pure Denied


foreign import requestSmsReadPermissionImpl :: (Boolean -> Effect Unit) -> Effect Unit
foreign import shouldShowRequestPermissionRationale :: Effect (Nullable Boolean)

-- | A type representing the result of `requestSmsReadPermission`.
-- |
-- |  * `Granted`: The user has just granted or previously granted SMS read
-- | permission
-- |
-- |  * `Denied`: The user has denied SMS read permission
-- |
-- |  * `DeniedNeverAskAgain`: The user has just denied or previously denied SMS
-- | read permission and chose to never see the dialog again
-- |
-- | _Note: Some specific native functions are used to check if the user chose
-- | "Never Ask Again". If these native functions were not available or an
-- | exception occured while trying to call them, the default response will
-- | be `Denied`. Keep this in mind when building your business logic based on
-- | `Denied` vs `DeniedNeverAskAgain`_
data PermissionResult = Granted | Denied | DeniedNeverAskAgain

derive instance eqPermissionResult :: Eq PermissionResult
derive instance genericPermissionResult :: Generic PermissionResult _
instance showPermissionResult :: Show PermissionResult where show = genericShow

-- | Requests Android SMS Read permission from the user
requestSmsReadPermission :: Aff Boolean
requestSmsReadPermission = do
  _ <- liftEffect $ trackAction {permissions_requested : ["READ_SMS", "RECEIVE_SMS"] } "otp"
  permissionGranted <- makeAff (\cb -> requestSmsReadPermissionImpl (Right >>> cb) *> pure nonCanceler)
  shouldShowRationale <- liftEffect shouldShowRequestPermissionRationale
  let fnName = "requestSmsReadPermission"
  case permissionGranted, toMaybe shouldShowRationale of
    true, _           -> do
      _ <- liftEffect $ trackAction {smsReadPermission: (show Granted), service: fnName} "otp_info"
      pure Granted
    false, Just false -> do
      _ <- liftEffect $ trackAction {smsReadPermission: (show DeniedNeverAskAgain), service: fnName} "otp_info"
      pure DeniedNeverAskAgain
    false, _ -> do
      _ <- liftEffect $ trackAction {smsReadPermission: (show Denied), service: fnName} "otp_info"
      pure Denied


foreign import fetchSmsRetriever :: (Either Error String -> Effect Unit)
  -> (Error -> Either Error String) -> (String -> Either Error String) -> Effect Unit
foreign import startSmsRetriever :: (Either Error String -> Effect Unit)
  -> (Error -> Either Error String) -> (String -> Either Error String) -> Effect Unit
foreign import stopSmsRetriever :: Effect Unit
foreign import cancelFetchSmsRetriever :: Effect Unit

initiateSMSRetriever :: Aff (Either Error String)
initiateSMSRetriever = do
  makeAff (\cb -> startSmsRetriever (Right >>> cb) Left Right *> pure (effectCanceler stopSmsRetriever))


foreign import startSmsReceiver :: (Either Error String -> Effect Unit)
  -> (Error -> Either Error String) -> (String -> Either Error String) -> Effect Unit
foreign import stopSmsReceiver :: Effect Unit

-- | Capture incoming SMSs by registering an Android Broadcast Receiver for
-- | SMS_RECEIVED action. This requires SMS permission to work.
-- | Calling `getName` on this will return the string "SMS_RECEIVER".
smsReceiver :: SmsReader
smsReceiver = SmsReader "SMS_RECEIVER" (runExceptT getNextSms)
  where
  getNextSms :: ExceptT Error Aff (Array Sms)
  getNextSms = do
    _ <- liftEffect $ trackAction {sms_receiver_started: true} "sms_info"
    smsString <- ExceptT $ makeAff (\cb -> startSmsReceiver (Right >>> cb) Left Right *> pure (effectCanceler stopSmsReceiver))
    let sms = (decodeAndTrack >>> hush >>> maybe [] identity) smsString
    if length sms < 1
      then getNextSms
      else do
        _ <- liftEffect $ trackAction {is_sms_received: true, service: "sms_receiver"} "otp"
        pure sms

foreign import readSms :: String -> (Error -> Either Error String)
  -> (String -> Either Error String) -> Effect (Either Error String)
foreign import md5Hash :: String -> String
foreign import getCurrentTime :: Effect Number

-- | Capture incoming SMSs by polling the SMS inbox at regular intervals. The
-- | first argument specifies the earliest time from which SMSs should be read
-- | (eg: session start time or time just before OTP trigger). The second
-- | argument specifies the frequency with which the poller should run (suggested
-- | frequency: 2 seconds). This requires SMS permission to work
-- | Calling `getName` on this will return the string "SMS_POLLER".
smsPoller :: Milliseconds -> Milliseconds -> Effect SmsReader
smsPoller startTime frequency = do
  processedRef <- Ref.new []
  pure $ SmsReader "SMS_POLLER" $ (runExceptT $ getNextSms processedRef)
  where
    getNextSms :: Ref (Array String) -> ExceptT Error Aff (Array Sms)
    getNextSms processedRef = do
      _ <- liftEffect $ trackAction {sms_poller_started: true} "sms_info"
      lift $ delay frequency
      _ <- liftEffect $ trackAction {timestamp_sent_to_native: (startTime)} "sms_info"
      decodedSmsString <- ExceptT $ liftEffect $ readSms (encodeJSON (unwrap startTime)) Left Right
      processed <- liftEffect $ Ref.read processedRef
      currentTime <- Milliseconds <$> liftEffect getCurrentTime
      let decodedSms = filter (notProcessed processed)
            $ filter (notFutureSms currentTime)
            $ (decodeAndTrack >>> hush >>> fromMaybe []) decodedSmsString
      liftEffect $ Ref.write (processed <> (hashSms <$> decodedSms)) processedRef
      if length decodedSms < 1
        then getNextSms processedRef
        else do
          _ <- liftEffect $ trackAction {is_sms_received: true, service: "sms_poller"} "otp"
          pure decodedSms
    
    getSenderName :: Sms -> String
    getSenderName (Sms sms') = sms'.from

    getSmsTime :: Sms -> Maybe Milliseconds
    getSmsTime (Sms sms') = Milliseconds <$> fromString sms'.time

    hashSms :: Sms -> String
    hashSms (Sms sms') = md5Hash $ sms'.body <> sms'.time

    notProcessed :: Array String -> Sms -> Boolean
    notProcessed processed sms' = not $ elem (hashSms sms') processed

    notFutureSms :: Milliseconds -> Sms -> Boolean
    notFutureSms currentTime sms' = fromMaybe true $ (greaterThan currentTime) <$> getSmsTime sms'



foreign import startSmsConsentAPI :: (Either Error String -> Effect Unit)
  -> (Error -> Either Error String) -> (String -> Either Error String) -> Effect Unit
foreign import stopSmsConsentAPI :: Effect Unit

-- | Check if User Consent API functions are available
foreign import isConsentAPISupported :: Effect Boolean
foreign import updateUnmatchedSms' :: Array(Sms) ->  Effect Unit
foreign import updateExtractedOtpStatus :: Boolean ->  Effect Unit

consentDeniedErrorMessage :: String
consentDeniedErrorMessage = "User denied consent for the SMS"

-- | Capture incoming SMSs by using Android's User Consent API.
-- | Make sure you call `isConsentAPISupported` first to check if it's supported,
-- | else it will throw an error immediately
-- | Calling `getName` on this will return the string "SMS_CONSENT".
smsConsentAPI :: SmsReader
smsConsentAPI = SmsReader "SMS_CONSENT" (runExceptT getNextSms)
  where
  getNextSms :: ExceptT Error Aff (Array Sms)
  getNextSms = do
    consentAPISupported <- liftEffect isConsentAPISupported
    if not consentAPISupported then throwError $ error "User Consent API is not available" else pure unit
    _ <- liftEffect $ trackAction {sms_consent_listener_started: true} "sms_info"
    smsString <- ExceptT $ makeAff (\cb -> startSmsConsentAPI (Right >>> cb) Left Right *> pure (effectCanceler stopSmsConsentAPI))
    _ <- liftEffect $ trackAction {sms_consent_shown: true} "sms_info"
    let isConsent = (smsString == "DENIED")
    _ <- liftEffect $ trackAction {is_consent_permission: (not isConsent)} "otp"
    if isConsent
      then throwError $ error consentDeniedErrorMessage
      else pure unit
    let sms = (decodeAndTrack >>> hush >>> maybe [] singleton) smsString
    if length sms < 1
      then getNextSms
      else do
        _ <- liftEffect $ trackAction {is_sms_received: true, service: "consent_api"} "otp"
        pure sms

-- | Check if the error is because the User hit "Deny" on the SMS consent dialog
isConsentDeniedError :: OtpError -> Boolean
isConsentDeniedError (SmsReaderError e reader) = getName reader == "SMS_CONSENT" && message e == consentDeniedErrorMessage
isConsentDeniedError _ = false


foreign import onClipboardChange :: (Either Error String -> Effect Unit)
  -> (Error -> Either Error String) -> (String -> Either Error String) -> Effect Unit


-- | Check if the JBridge functions for Clipboard are available.
foreign import isClipboardSupported :: Effect Boolean

-- | Capture incoming OTPs by listening for clipboard changes. The body could
-- | either be the the entire SMS body or the OTP itself. In both cases, the OTP
-- | should be extractable by `extractOtp`
-- | Make sure you first call `isClipboardSupported` before using this reader,
-- | else it will immediately throw an Error
-- | Calling `getName` on this will return the string "CLIPBOARD"
clipboard :: SmsReader
clipboard = SmsReader "CLIPBOARD" (runExceptT getNextSms)
  where
    getNextSms :: ExceptT Error Aff (Array Sms)
    getNextSms = do
      clipboardSupported <- liftEffect $ isClipboardSupported
      if not clipboardSupported then throwError $ error "Clipboard API not available" else pure unit
      _ <- liftEffect $ trackAction {clipboard_listener_started: true} "sms_info"
      smsString <- ExceptT $ makeAff (\cb -> onClipboardChange (Right >>> cb) Left Right *> pure nonCanceler)
      currentTime <- liftEffect getCurrentTime
      let stringArray = (decodeAndTrack >>> hush >>> maybe [] identity) smsString
          sms = toSms currentTime <$> stringArray
      if length sms < 1
        then getNextSms
        else do
          _ <- liftEffect $ trackAction {is_sms_received: true, service: "clipboard"} "otp"
          pure sms

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
getOtpListener :: NonEmptyArray SmsReader -> Aff OtpListener
getOtpListener readers = do
  _ <- liftEffect $ trackAction {otp_listener_started: true} "sms_info"
  otpRulesVar <- AVar.empty -- The OTP rules to be used. Empty at first until `setOtpRules` is called for the first time
  unprocessedSmsVar <- AVar.new [] -- Accumulates any SMSs that arrive before OTP rules are set (so they can be processed once it's set)

  let
    setOtpRules rules = do
      _ <- liftEffect $ trackAction {otp_rules: (show rules)} "sms_info"
      AVar.tryTake otpRulesVar *> AVar.put rules otpRulesVar

    getNextOtp = do
      otpRulesSet <- isOtpRulesSet otpRulesVar
      {--
      If the otp rules have already been set (ie, `setOtpRules` was called once before `getNextOtp`),
      start waiting for SMSs to arrive and process them for OTPs. If otp rules haven't been
      set yet, do 2 things in parallel:
        1. Listen for SMSs but don't process them for OTPs just yet. Queue them up instead.
        2. Wait for otp rules to be set. When it's set, start processinng those queued up SMSs from (1.)
      --}
      smsList <- if otpRulesSet
          then ExceptT $ waitForSms readers
          else ExceptT $ oneOfAff [
            waitForSms readers,
            waitForOtpRules otpRulesVar *> (Right <$> getUnprocessedSms unprocessedSmsVar)
          ]
      otpRules <- getOtpRules otpRulesVar
      case otpRules of
        Nothing -> addToUnprocessed unprocessedSmsVar smsList *> getNextOtp
        Just rules -> do
          result <- pure $ findMap (tryExtract rules) smsList
          isOtpMatched <- pure $ isJust result
          _ <- liftEffect $ case isOtpMatched of
            true -> do
              _ <- liftEffect $ trackAction {is_detected: true} "otp"
              updateExtractedOtpStatus true
            false -> do
              -- updating Unmatched smsList of window
              _ <- updateUnmatchedSms smsList
              updateExtractedOtpStatus false
          maybe getNextOtp pure result


  pure {
    getNextOtp: (either Error identity) <$> runExceptT getNextOtp,
    setOtpRules,
    smsReaders: readers
  }
  where
    personalNumberRegex :: Regex
    personalNumberRegex = unsafePartial $ fromRight $ regex "^[+][0-9]{10,}$" global

    isNonPersonalMessage :: ReceivedSms -> Boolean
    isNonPersonalMessage (ReceivedSms (Sms sms) reader) = not $ isJust $ match personalNumberRegex sms.from

    updateUnmatchedSms :: Array (ReceivedSms) -> Effect Unit
    updateUnmatchedSms smsArray = do
      -- this contains only the bank messages by masking digits (OTP) and excluding messages from personal number
      let nonPersonalSMS = filter isNonPersonalMessage smsArray
      let maskedSmsArray = map getMaskedSms nonPersonalSMS
      updateUnmatchedSms' maskedSmsArray

    -- Take a set of Affs, run them in parallel and return the first one that succeeds
    oneOfAff :: forall f a. (Foldable f) => (Functor f) => f (Aff a) -> Aff a
    oneOfAff affs = sequential $ oneOf $ parallel <$> affs

    -- Take an Array of SmsReaders, start them all in parallel and return the response of the first one that succeeds (or fails)
    waitForSms :: NonEmptyArray SmsReader -> Aff (Either OtpError (Array ReceivedSms))
    waitForSms smsReaders = oneOfAff $ smsReaders <#> (\reader@(SmsReader _ r) -> do
        res <- r
        pure $ case res of
          Left err -> Left (SmsReaderError err reader)
          Right smses -> Right $ (\s -> ReceivedSms s reader) <$> smses
      )

    -- Used to wait for otp rules to be set for the first time (by `OtpListener.setOtpRules`)
    waitForOtpRules :: AVar (Array OtpRule) -> Aff (Array OtpRule)
    waitForOtpRules otpRulesVar = AVar.read otpRulesVar

    getOtpRules :: AVar (Array OtpRule) -> OtpM (Maybe (Array OtpRule))
    getOtpRules otpRulesVar = lift $ AVar.tryRead otpRulesVar

    getUnprocessedSms :: AVar (Array ReceivedSms) -> Aff (Array ReceivedSms)
    getUnprocessedSms unprocessedSmsVar = AVar.read unprocessedSmsVar

    isOtpRulesSet :: AVar (Array OtpRule) -> OtpM Boolean
    isOtpRulesSet otpRulesVar = lift $ AVar.isFilled <$> AVar.status otpRulesVar

    addToUnprocessed :: AVar (Array ReceivedSms) -> Array ReceivedSms -> OtpM Unit
    addToUnprocessed unprocessedSmsVar sms' = lift do
      unprocessed <- AVar.take unprocessedSmsVar
      AVar.put (unprocessed <> sms') unprocessedSmsVar

    tryExtract :: Array OtpRule -> ReceivedSms -> Maybe Otp
    tryExtract rules (ReceivedSms sms' reader) = (\o -> Otp o sms' reader) <$> extractOtp sms' rules


--Returns SMS with masked OTP
getMaskedSms :: ReceivedSms -> Sms
getMaskedSms (ReceivedSms (Sms sms) reader) = Sms{from : sms.from, body : replaceDigitWithX sms.body, time : sms.time}


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
  setOtpRules :: Array OtpRule -> Aff Unit,
  smsReaders :: NonEmptyArray SmsReader
}


-- | Return type of `OtpListener.getNextOtp`.
-- |
-- | In case of success, it provides the OTP string, the SMS from which that OTP
-- | was extracted and the `SmsReader` that captured that SMS. (You can use
-- | `getName` to get the name of the SMS Reader that caputed the SMS).
-- |
-- | In case of an error, it provides an `OtpError` type.
data Otp = Otp String Sms SmsReader | Error OtpError
derive instance otp :: Generic Otp _
instance showOtp :: Show Otp where
  show = genericShow

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
extractOtp sms' rules =
  findMap (\rule -> matchAndExtract rule sms') rules



-- | Match a given SMS against a given rule and attempt to extract the OTP
-- | from the SMS. Returns `Nothing` if it fails.
matchAndExtract :: OtpRule -> Sms -> Maybe String
matchAndExtract orule@(OtpRule rule) sms' =
 -- Succeeds if the SMS's 'from' matches any one of the 'from's in the OTP rule (or if the SMS's 'from' is "UNKNOWN_BANK")
  matchSender orule sms' >>= matchMessage orule >>= extract orule

-- Succeeds if the SMS's 'from' matches any one of the 'from's in the OTP rule (or if the SMS's 'from' is "UNKNOWN_BANK")
matchSender :: OtpRule -> Sms -> Maybe Sms
matchSender orule@(OtpRule rule) (Sms sms') =
  let
    senderRules = catMaybes $ makeRegex orule <$> rule.matches.sender
    matches = not null $ catMaybes $ (\r -> match r sms'.from) <$> senderRules
    fromRetrieverApi = sms'.from == "UNKNOWN_BANK"
  in if matches || fromRetrieverApi then Just (Sms sms') else Nothing

-- Succeeds if the SMS's body matches the body regex in the OTP rule
matchMessage :: OtpRule -> Sms -> Maybe Sms
matchMessage orule@(OtpRule rule)  (Sms sms') =
  let
    matchesBody = isJust $ makeRegex orule rule.matches.message >>= (\r -> match r sms'.body)
    matchesOtp = isJust $ makeRegex orule ("^" <> rule.otp <> "$") >>= (\r -> match r sms'.body) -- if the whole message body is the OTP (like from clipboard)
  in if (matchesBody || matchesOtp) then Just (Sms sms') else Nothing

-- Attempt to extract the OTP from the SMS body using the otp regex in the OTP rule
extract :: OtpRule -> Sms -> Maybe String
extract orule@(OtpRule rule)  (Sms sms') =
  let
    group = fromMaybe 0 rule.group
    extractedOtp = join $ makeRegex orule rule.otp >>= (\r -> match r sms'.body) >>= (\arr -> arr !! group)
  in extractedOtp

-- Helper function to attempt creating a regex from a given string
makeRegex :: OtpRule -> String -> Maybe Regex
makeRegex (OtpRule rule)  s = case regex s (ignoreCase) of
  Right r -> Just r
  Left err -> Nothing

logRegexError :: OtpRule -> String -> Effect Unit
logRegexError (OtpRule rule) s = case regex s (ignoreCase) of
    Right r -> pure unit
    Left err -> do
      _ <- Tracker.trackException Tracker.ACTION Tracker.System Tracker.DETAILS "otp_reader" ("Regex syntax error \"" <> err <> "\" for rule: " <> encodeJSON (OtpRule rule)) (Object.empty)
      pure unit
  
-- | Used for tracking decode errors for values that should never have failed
-- | a decode (such as `Sms` values retreived from Android or `OtpRule`s)

decodeAndTrack :: forall a. Decode a => String -> Effect (Either MultipleErrors a)
decodeAndTrack s = case runExcept $ decodeJSON s of
  Right a -> pure $ Right a
  Left err -> do
    _ <- liftEffect $ Tracker.trackException Tracker.ACTION Tracker.System Tracker.DETAILS "otp_reader" ("decode exception: " <> show err) Object.empty
    pure $ Left err