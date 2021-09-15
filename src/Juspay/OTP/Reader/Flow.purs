module Juspay.OTP.Reader.Flow (
    module Juspay.OTP.Reader,
    getGodelOtpRules,
    getSmsReadPermission,
    requestSmsReadPermission,
    smsPoller,
    isConsentAPISupported,
    isClipboardSupported,
    OtpListener,
    getOtpListener
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds)
import Effect.Class (liftEffect)
import Foreign (F)
import Juspay.OTP.Reader (Otp(..), OtpError(..), OtpRule(..), Sms(..), SmsReader(..), clipboard, extractOtp, getName, isConsentDeniedError, smsConsentAPI, smsReceiver)
import Juspay.OTP.Reader as O
import Presto.Core.Types.Language.Flow (Flow, doAff)

doAff':: forall a e. Aff a -> Flow e a
doAff' aff = doAff aff

doEff' :: forall a e. Effect a -> Flow e a
doEff' eff = doAff' $ liftEffect eff

-- | Flow version of `getGodelOtpRules` from `Juspay.OTP.Reader`
-- | Gets bank OTP rules from Godel's config.
getGodelOtpRules :: forall e. String -> Flow e (F (Array OtpRule))
getGodelOtpRules = doEff' <<< O.getGodelOtpRules

-- | Flow version of smsPoller from Juspay.OTP.Reader
-- | Capture incoming SMSs by polling the SMS inbox at regular intervals. The
-- | first argument specifies the earliest time from which SMSs should be read
-- | (eg: session start time or time just before OTP trigger). The second
-- | argument specifies the frequency with which the poller should run (suggested
-- | frequency: 2 seconds). This requires SMS permission to work
smsPoller :: forall e. Milliseconds -> Milliseconds -> Flow e SmsReader
smsPoller startTime frequency = do
  doEff' $ O.smsPoller startTime frequency

-- | Check if User Consent API functions are available
isConsentAPISupported :: forall e. Flow e Boolean
isConsentAPISupported = doEff' O.isConsentAPISupported

-- | Check if the JBridge functions for Clipboard are available.
isClipboardSupported :: forall e. Flow e Boolean
isClipboardSupported = doEff' O.isClipboardSupported

-- | Flow e version of `getSmsReadPermission` from Juspay.OTP.Reader
-- | Checks if Android SMS Read permission has been granted
getSmsReadPermission :: forall e. Flow e Boolean
getSmsReadPermission = doAff' $ liftEffect O.getSmsReadPermission

-- | Flow e version of `getSmsReadPermission` from Juspay.OTP.Reader
-- | Requests Android SMS Read permission from the user
requestSmsReadPermission :: forall e. Flow e Boolean
requestSmsReadPermission = doAff' O.requestSmsReadPermission

type OtpListener e = {
  getNextOtp :: Flow e Otp,
  setOtpRules :: Array OtpRule -> Flow e Unit
}

-- | Flow version of `getSmsReadPermission` from Juspay.OTP.Reader
-- | Takes an array of `SmsReader`s and returns functions to get OTPs. It uses the
-- | supplied `SmsReader`s  by running them in parallel to capture any incoming
-- | SMSs and attempts to extract an OTP from them using given OTP rules. Check
-- | the `OtpListener` type for more info on how to get OTPs and set OTP rules.
getOtpListener :: forall e. NonEmptyArray SmsReader -> Flow e (OtpListener e)
getOtpListener readers = do
  listener <- doAff' $ O.getOtpListener readers
  pure {
    getNextOtp: doAff' listener.getNextOtp,
    setOtpRules: doAff' <<< listener.setOtpRules
  }