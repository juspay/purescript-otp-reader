## Module Juspay.OTP.Reader.Flow

#### `getGodelOtpRules`

``` purescript
getGodelOtpRules :: String -> Flow (F (Array OtpRule))
```

Flow version of `getGodelOtpRules` from `Juspay.OTP.Reader`
Gets bank OTP rules from Godel's config.

#### `getSmsReadPermission`

``` purescript
getSmsReadPermission :: Flow Boolean
```

Flow version of `getSmsReadPermission` from Juspay.OTP.Reader
Checks if Android SMS Read permission has been granted

#### `requestSmsReadPermission`

``` purescript
requestSmsReadPermission :: Flow Boolean
```

Flow version of `getSmsReadPermission` from Juspay.OTP.Reader
Requests Android SMS Read permission from the user

#### `smsPoller`

``` purescript
smsPoller :: Milliseconds -> Milliseconds -> Flow SmsReader
```

Flow version of smsPoller from Juspay.OTP.Reader
Capture incoming SMSs by polling the SMS inbox at regular intervals. The
first argument specifies the earliest time from which SMSs should be read
(eg: session start time or time just before OTP trigger). The second
argument specifies the frequency with which the poller should run (suggested
frequency: 2 seconds). This requires SMS permission to work

#### `isConsentAPISupported`

``` purescript
isConsentAPISupported :: Flow Boolean
```

Check if User Consent API functions are available

#### `isClipboardSupported`

``` purescript
isClipboardSupported :: Flow Boolean
```

Check if the JBridge functions for Clipboard are available.

#### `OtpListener`

``` purescript
type OtpListener = { getNextOtp :: Flow Otp, setOtpRules :: Array OtpRule -> Flow Unit }
```

#### `getOtpListener`

``` purescript
getOtpListener :: Array SmsReader -> Flow OtpListener
```

Flow version of `getSmsReadPermission` from Juspay.OTP.Reader
Takes an array of `SmsReader`s and returns functions to get OTPs. It uses the
supplied `SmsReader`s  by running them in parallel to capture any incoming
SMSs and attempts to extract an OTP from them using given OTP rules. Check
the `OtpListener` type for more info on how to get OTPs and set OTP rules.


### Re-exported from Juspay.OTP.Reader:

#### `SmsReader`

``` purescript
data SmsReader
  = SmsReader String (Aff (Either Error (Array Sms)))
```

This type represents a method of reading incoming SMSs from the OS. If newer
methods of reading SMSs need to be created, use this type.
The first parameter is the name of the SMS Reader as a String (useful for
differentiating between SmsReaders`s). The second argument is the function
to be used to wait for the next SMS.

#### `Sms`

``` purescript
newtype Sms
  = Sms { body :: String, from :: String, time :: String }
```

Type representing an SMS received using any `SmsReader`s.

##### Instances
``` purescript
Eq Sms
Newtype Sms _
Generic Sms _
Ord Sms
Encode Sms
Decode Sms
```

#### `OtpRule`

``` purescript
newtype OtpRule
  = OtpRule { group :: Maybe Int, matches :: { message :: String, sender :: Array String }, otp :: String }
```

Represents a rule for matching and extracting an OTP. An SMS will
successfully match a given `OtpRule` if the following are true:
 1. `message`, `otp` and the strings in the `sender` array are all
   valid regex strings.
 2. The SMS body matches the `message` regex
 3. The SMS 'from' field matches one of the `sender` regexes
 4. The OTP part of the SMS body successfuly matches the `otp` regex
 5. If the `otp` regex contains groups, the `group` value references
   a valid group matched by the `otp` regex. (defaults to 0 if `Nothing`)

##### Instances
``` purescript
Newtype OtpRule _
Generic OtpRule _
Encode OtpRule
Decode OtpRule
```

#### `OtpError`

``` purescript
data OtpError
  = SmsReaderError Error SmsReader
  | OtherError Error
```

Represents an error that occured during `OtpListener.getNextOtp`. It can
either be an `Error` thrown by one of the `SmsReader`s or some other generic
`Error`. In case of an `SmsReader` error, the `SmsReader` that threw the
error is also provided. (You can use `getName` to get the name of the SMS
Reader that threw the error).

##### Instances
``` purescript
Show OtpError
```

#### `Otp`

``` purescript
data Otp
  = Otp String Sms SmsReader
  | Error OtpError
```

Return type of `OtpListener.getNextOtp`.

In case of success, it provides the OTP string, the SMS from which that OTP
was extracted and the `SmsReader` that captured that SMS. (You can use
`getName` to get the name of the SMS Reader that caputed the SMS).

In case of an error, it provides an `OtpError` type.

#### `smsReceiver`

``` purescript
smsReceiver :: SmsReader
```

Capture incoming SMSs by registering an Android Broadcast Receiver for
SMS_RECEIVED action. This requires SMS permission to work.
Calling `getName` on this will return the string "SMS_RECEIVER".

#### `smsConsentAPI`

``` purescript
smsConsentAPI :: SmsReader
```

Capture incoming SMSs by using Android's User Consent API.
Make sure you call `isConsentAPISupported` first to check if it's supported,
else it will throw an error immediately
Calling `getName` on this will return the string "SMS_CONSENT".

#### `getName`

``` purescript
getName :: SmsReader -> String
```

Get the name of an `SmsReader`. Useful for differentiating between `SmsReader`s

#### `extractOtp`

``` purescript
extractOtp :: Sms -> Array OtpRule -> Maybe String
```

Given an SMS and a list of OTP rules, it will return the first OTP
that matches one of the given rules or `Nothing` if none of them match.

#### `clipboard`

``` purescript
clipboard :: SmsReader
```

Capture incoming OTPs by listening for clipboard changes. The body could
either be the the entire SMS body or the OTP itself. In both cases, the OTP
should be extractable by `extractOtp`
Make sure you first call `isClipboardSupported` before using this reader,
else it will immediately throw an Error
Calling `getName` on this will return the string "CLIPBOARD"

