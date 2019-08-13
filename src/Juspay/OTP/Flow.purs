module Juspay.OTP.OTPReader.Flow (
    module Juspay.OTP.OTPReader,
    getSmsReadPermission,
    requestSmsReadPermission,
    smsPoller,
    getOtpListener
  ) where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Juspay.OTP.OTPReader (Sms(..), SmsReader, OtpListener, smsReceiver, extractOtp)
import Juspay.OTP.OTPReader as O
import Presto.Core.Types.Language.Flow (Flow, doAff)

doAff' :: forall e a. Aff e a -> Flow a
doAff' aff = doAff do unsafeCoerceAff aff

doEff' :: forall e a. Eff e a -> Flow a
doEff' eff = doAff' $ liftEff eff


smsPoller :: Milliseconds -> Milliseconds -> Flow SmsReader
smsPoller startTime frequency = do
  doEff' $ O.smsPoller startTime frequency


getSmsReadPermission :: Flow Boolean
getSmsReadPermission = doAff' $ liftEff O.getSmsReadPermission

requestSmsReadPermission :: Flow Boolean
requestSmsReadPermission = doAff' O.requestSmsReadPermission


getOtpListener :: Array SmsReader -> Flow OtpListener
getOtpListener = doAff' <<< O.getOtpListener