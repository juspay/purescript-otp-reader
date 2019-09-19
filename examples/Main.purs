module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes)
import Data.Array.NonEmpty (fromArray, toArray)
import Data.Either (either)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, log)
import Effect.Exception (error, throwException)
import Juspay.OTP.Reader (Otp(..), OtpListener, clipboard, getGodelOtpRules, getName, getOtpListener, isClipboardSupported, isConsentAPISupported, isConsentDeniedError, requestSmsReadPermission, smsConsentAPI, smsPoller, smsReceiver)

foreign import init :: Effect Unit
foreign import getTime :: Effect Number

main :: Effect Unit
main = do
  init
  runAff_ (either errorShow pure) example


example :: Aff Unit
example = do
  -- Attempt to get HDFC OTP rules from Godel's config. Throw an error if it fails to decode
  rulesF <- liftEffect $ runExcept <$> getGodelOtpRules "HDFC"
  otpRules <- liftEffect $ either (show >>> error >>> throwException) (pure) rulesF

  -- initialize an SMS inbox poller with current time as start time
  currentTime <- liftEffect getTime
  let pollerStartTime = Milliseconds currentTime
      pollerFrequency = Milliseconds 2000.0
  poller <- liftEffect $ smsPoller pollerStartTime pollerFrequency

  -- Request SMS permission. If granted, use Receiver and Poller. Else try Clipboard and Consent API. Else throw an error
  permissionGranted <- requestSmsReadPermission
  clipboardSupported <- liftEffect isClipboardSupported
  consentAPISupported <- liftEffect isConsentAPISupported
  let mSmsReaders = fromArray $ catMaybes [
                    if permissionGranted then Just smsReceiver else Nothing,
                    if permissionGranted then Just poller else Nothing,
                    if not permissionGranted && clipboardSupported then Just clipboard else Nothing,
                    if not permissionGranted && consentAPISupported then Just smsConsentAPI else Nothing
                  ]
  smsReaders <- maybe (throwError $ error "No supported methods for SMS reading") pure mSmsReaders
  log $ "Using SMS Readers: " <> joinWith "," (getName <$> toArray smsReaders)

  -- Create an OTP listener and set the OTP rules
  otpListener <- getOtpListener smsReaders
  otpListener.setOtpRules otpRules

  -- Loop and log incoming HDFC OTPs
  otpLoop otpListener
  pure unit

otpLoop :: OtpListener -> Aff Unit
otpLoop listener = do
  log "Listening for otp"
  res <- listener.getNextOtp -- blocks until an HDFC OTP is received
  case res of
    Otp otp sms reader -> do
      log $ "OTP received from " <> getName reader <> ": " <> otp <> "\nSMS: " <> genericShow sms
      otpLoop listener
    Error err -> if isConsentDeniedError err
      then log "OTP consent denied" *> otpLoop listener
      else throwError $ error $ show err