## Module Juspay.OTP.Reader

#### `Sms`

``` purescript
newtype Sms
  = Sms { from :: String, body :: String, time :: String }
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
  = OtpRule { matches :: { sender :: Array String, message :: String }, otp :: String, group :: Maybe Int }
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

#### `getGodelOtpRules`

``` purescript
getGodelOtpRules :: String -> Effect (F (Array OtpRule))
```

Gets bank OTP rules from Godel's config.

#### `SmsReader`

``` purescript
newtype SmsReader
  = SmsReader (Aff (Either Error (Array Sms)))
```

This type represents a method of reading incoming SMSs from the OS. If newer
methods of reading SMSs need to be created, use this type

#### `smsReceiver`

``` purescript
smsReceiver :: SmsReader
```

Capture incoming SMSs by registering an Android Broadcast Receiver for
SMS_RECEIVED action. This requires SMS permission to work

#### `smsPoller`

``` purescript
smsPoller :: Milliseconds -> Milliseconds -> Effect SmsReader
```

Capture incoming SMSs by polling the SMS inbox at regular intervals. The
first argument specifies the earliest time from which SMSs should be read
(eg: session start time or time just before OTP trigger). The second
argument specifies the frequency with which the poller should run (suggested
frequency: 2 seconds). This requires SMS permission to work

#### `clipboard`

``` purescript
clipboard :: SmsReader
```

Capture incoming OTPs by listening for clipboard changes. The body could
either be the the entire SMS body or the OTP itself. In both cases, the OTP
should be extractable by `extractOtp`

#### `getSmsReadPermission`

``` purescript
getSmsReadPermission :: Effect Boolean
```

Checks if Android SMS Read permission has been granted

#### `requestSmsReadPermission`

``` purescript
requestSmsReadPermission :: Aff Boolean
```

Requests Android SMS Read permission from the user

#### `OtpListener`

``` purescript
type OtpListener = { getNextOtp :: Aff (Either Error String), setOtpRules :: Array OtpRule -> Aff Unit }
```

Return type of `getOtpListener` function.

The `getNextOtp` function blocks until an OTP is received. It uses the list
of `SmsReader`s passed to `getOtpListener` to caputre incoming SMSs and
returns the first one to match an `OtpRule`.

The `setOtpRules` function is used to set the OtpRules that should be used
for attempting to exctract an OTP from any incoming SMSs.

At first, no OTP rules are set and calling `getNextOtp` will not return any
OTPs. Instead any incoming SMSs will be queued until `setOtpRules` is called
for the first time at which point, the queued up SMSs will be validated and
an OTP returned if any of them match any rule.

#### `getOtpListener`

``` purescript
getOtpListener :: Array SmsReader -> Aff OtpListener
```

Takes an array of `SmsReader`s and returns functions to get OTPs. It uses the
supplied `SmsReader`s  by running them in parallel to capture any incoming
SMSs and attempts to extract an OTP from them using given OTP rules. Check
the `OtpListener` type for more info on how to get OTPs and set OTP rules.

#### `extractOtp`

``` purescript
extractOtp :: Array Sms -> Array OtpRule -> Maybe String
```

Given a list of SMSs and a list of OTP rules, it will return the first OTP
that matches one of the given rules or `Nothing` if none of them match.


