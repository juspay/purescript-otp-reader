module Juspay.OTP.Flow.OTPReader where

import Data.Time.Duration (Milliseconds)
import Juspay.OTP.OTPReader (OtpRule, Result, Sms)
import Juspay.OTP.OTPReader as OTP
import Presto.Core.Types.Language.Flow (Flow, doAff)

getSmsReadPermission :: Flow Boolean
getSmsReadPermission = doAff do OTP.getSmsReadPermission

requestSmsReadPermission :: Flow Boolean
requestSmsReadPermission = doAff do OTP.requestSmsReadPermission

getOtp :: Array OtpRule -> Flow Result
getOtp rules = doAff do OTP.getOtp rules

smsReceiver :: Flow (Array Sms)
smsReceiver = doAff do OTP.smsReceiver

smsPoller :: Number -> Milliseconds -> Flow (Array Sms)
smsPoller startTime pollFrequency = doAff do OTP.smsPoller startTime pollFrequency