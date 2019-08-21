module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, log, logShow)
import Effect.Exception (error, throwException)
import Juspay.OTP.Reader (OtpListener, clipboard, getGodelOtpRules, getOtpListener, requestSmsReadPermission, smsPoller, smsReceiver)

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
  poller <- liftEffect $ smsPoller (Milliseconds currentTime) (Milliseconds 2000.0)

  -- Request SMS permission. If granted, use Receiver and Poller. Else listen to Clipboard for copied OTP/SMS
  permissionGranted <- requestSmsReadPermission
  let smsReaders = if permissionGranted then [smsReceiver, poller] else [clipboard]

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
    Right otp -> log $ "OTP: " <> otp
    Left err -> logShow err
  otpLoop listener