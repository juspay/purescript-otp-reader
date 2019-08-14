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
import Juspay.OTP.Reader (OtpListener, getGodelOtpRules, getOtpListener, requestSmsReadPermission, smsPoller, smsReceiver)

foreign import init :: Effect Unit
foreign import getTime :: Effect Number

main :: Effect Unit
main = do
  init
  runAff_ (either errorShow pure) example


example :: Aff Unit
example = do
  -- Request SMS permission. Throw an error if not granted
  permissionGranted <- requestSmsReadPermission
  if not permissionGranted then throwError (error "No permission") else pure unit

  -- Attempt to get HDFC OTP rules from Godel's config. Throw an error if it fails to decode
  rulesF <- liftEffect $ runExcept <$> getGodelOtpRules "HDFC"
  otpRules <- liftEffect $ either (show >>> error >>> throwException) (pure) rulesF

  -- Get an OTP listener that uses SMS Receiver and SMS Poller
  currentTime <- liftEffect getTime
  let pollerStartTime = Milliseconds currentTime
      pollerFrequency = Milliseconds 2000.0
  poller <- liftEffect $ smsPoller (Milliseconds currentTime) (Milliseconds 2000.0)
  otpListener <- getOtpListener [smsReceiver, poller]

  -- Set the OTP rules
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