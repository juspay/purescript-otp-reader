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
import Juspay.OTP.OTPReader (Sms(..), SmsReader(..), OtpListener, smsReceiver, extractOtp)
import Juspay.OTP.OTPReader as O
import Presto.Core.Types.Language.Flow (Flow, doAff)

doAff' :: forall e a. Aff e a -> Flow a
doAff' aff = doAff do unsafeCoerceAff aff

doEff' :: forall e a. Eff e a -> Flow a
doEff' eff = doAff' $ liftEff eff

-- | Flow version of smsPoller from Juspay.OTP.OTPReader
-- | Capture incoming SMSs by polling the SMS inbox at regular intervals. The
-- | first argument specifies the earliest time from which SMSs should be read
-- | (eg: session start time or time just before OTP trigger). The second
-- | argument specifies the frequency with which the poller should run (suggested
-- | frequency: 2 seconds). This requires SMS permission to work
smsPoller :: Milliseconds -> Milliseconds -> Flow SmsReader
smsPoller startTime frequency = do
  doEff' $ O.smsPoller startTime frequency

-- | Flow version of getSmsReadPermission from Juspay.OTP.OTPReader
-- | Checks if Android SMS Read permission has been granted
getSmsReadPermission :: Flow Boolean
getSmsReadPermission = doAff' $ liftEff O.getSmsReadPermission

-- | Flow version of getSmsReadPermission from Juspay.OTP.OTPReader
-- | Requests Android SMS Read permission from the user
requestSmsReadPermission :: Flow Boolean
requestSmsReadPermission = doAff' O.requestSmsReadPermission

-- | Flow version of getSmsReadPermission from Juspay.OTP.OTPReader
-- | Takes an array of `SmsReader`s and returns functions to get OTPs. It uses the
-- | supplied `SmsReader`s  by running them in parallel to capture any incoming
-- | SMSs and attempts to extract an OTP from them using given OTP rules. Check
-- | the `OtpListener` type for more info on how to get OTPs and set OTP rules.
getOtpListener :: Array SmsReader -> Flow OtpListener
getOtpListener = doAff' <<< O.getOtpListener