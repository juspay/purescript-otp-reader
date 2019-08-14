module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(Milliseconds), runAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Juspay.OTP.Reader (OtpListener, getGodelOtpRules, getOtpListener, requestSmsReadPermission, smsPoller, smsReceiver)

foreign import init :: forall e. Eff e Unit
foreign import getTime :: forall e. Eff e Number

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  init
  runAff_ (either (Console.errorShow) pure) example


example :: forall e. Aff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
example = do
  -- Request SMS permission. Throw an error if not granted
  permissionGranted <- requestSmsReadPermission
  if not permissionGranted then throwError (error "No permission") else pure unit

  -- Attempt to get HDFC OTP rules from Godel's config. Throw an error if it fails to decode
  rulesF <- liftEff $ runExcept <$> getGodelOtpRules "HDFC"
  otpRules <- liftEff $ either (show >>> error >>> throwException) (pure) rulesF

  -- Get an OTP listener that uses SMS Receiver and SMS Poller
  currentTime <- liftEff getTime
  let pollerStartTime = Milliseconds currentTime
      pollerFrequency = Milliseconds 2000.0
  poller <- liftEff $ smsPoller (Milliseconds currentTime) (Milliseconds 2000.0)
  otpListener <- getOtpListener [smsReceiver, poller]

  -- Set the OTP rules
  otpListener.setOtpRules otpRules

  -- Loop and log incoming HDFC OTPs
  otpLoop otpListener
  pure unit

otpLoop :: forall e. OtpListener -> Aff (console :: CONSOLE | e) Unit
otpLoop listener = do
  log "Listening for otp"
  res <- listener.getNextOtp -- blocks until an HDFC OTP is received
  case res of
    Right otp -> log $ "OTP: " <> otp
    Left err -> logShow err
  otpLoop listener