module Test.Main where

import Prelude

import Control.Monad.Aff (forkAff, launchAff_, runAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Juspay.OTP.OTPReader (getOtpListener, getSmsReadPermission, requestSmsReadPermission, smsReceiver)
import Juspay.OTP.Rule (getGodelOtpRules)

foreign import init :: forall e. Eff e Unit

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = runAff_ (either (Console.errorShow) pure) do
  liftEff init
  rules <- liftEff $ getGodelOtpRules "" >>= either (show >>> error >>> throwException) (pure)
  permissionGranted <- requestSmsReadPermission
  if not permissionGranted then throwError (error "No permission") else pure unit
  otpListener <- getOtpListener [smsReceiver]
  otpListener.setOtpRules rules
  log "Listening for otp"
  void $ forkAff $ loop otpListener
  pure unit
  where
    loop listener = (listener.getNextOtp >>= logShow) *> loop listener