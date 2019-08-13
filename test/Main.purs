module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(..), forkAff, launchAff_, runAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Juspay.OTP.OTPReader (OtpListener, getOtpListener, getSmsReadPermission, requestSmsReadPermission, smsPoller, smsReceiver)
import Juspay.OTP.Rule (getGodelOtpRules)

foreign import init :: forall e. Eff e Unit
foreign import getTime :: forall e. Eff e Number

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = runAff_ (either (Console.errorShow) pure) do
  liftEff init
  rules <- liftEff $ getGodelOtpRules "" >>= either (show >>> error >>> throwException) (pure)
  permissionGranted <- requestSmsReadPermission
  if not permissionGranted then throwError (error "No permission") else pure unit
  time <- liftEff getTime
  poller <- liftEff $ smsPoller (Milliseconds time) (Milliseconds 2000.0)
  otpListener <- getOtpListener [poller]
  otpListener.setOtpRules rules
  waitForOtp otpListener
  pure unit

waitForOtp :: forall e. OtpListener e -> Aff e Unit
waitForOtp listener = do
  unsafeCoerceAff $ log "Listening for otp"
  otp <- listener.getNextOtp
  unsafeCoerceAff $ logShow otp
  waitForOtp listener