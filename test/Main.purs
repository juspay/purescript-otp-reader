module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(Milliseconds), runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, log, logShow)
import Effect.Exception (error, throwException)
import Foreign.Generic (encodeJSON)
import Juspay.OTP.Reader (OtpListener, getGodelOtpRules, getOtpListener, requestSmsReadPermission, smsPoller)

foreign import init :: Effect Unit
foreign import getTime :: Effect Number

main :: Effect Unit
main = runAff_ (either errorShow pure) do
  liftEffect init
  rules <- liftEffect $ runExcept <$> getGodelOtpRules "ICICICC" >>= either (show >>> error >>> throwException) (pure)
  log $ encodeJSON rules
  permissionGranted <- requestSmsReadPermission
  if not permissionGranted then throwError (error "No permission") else pure unit
  time <- liftEffect getTime
  poller <- liftEffect $ smsPoller (Milliseconds time) (Milliseconds 2000.0)
  otpListener <- getOtpListener [poller]
  otpListener.setOtpRules rules
  waitForOtp otpListener
  pure unit

waitForOtp :: OtpListener-> Aff Unit
waitForOtp listener = do
  log "Listening for otp"
  otp <- listener.getNextOtp
  logShow otp
  waitForOtp listener