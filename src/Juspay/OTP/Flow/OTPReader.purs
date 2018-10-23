module Juspay.OTP.Flow.OTPReader where

import Juspay.OTP.OTPReader (OtpRule, Result)
import Juspay.OTP.OTPReader as OTP
import Presto.Core.Types.Language.Flow (Flow, doAff)


getOtp :: Array OtpRule -> Flow Result
getOtp rules = doAff do OTP.getOtp rules