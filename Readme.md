# OTP Reader helper library

Use this library to do any kind of OTP reading. It currently supports the following methods of SMS reading:

* SMS Receiver
* SMS Inbox Polling
* Android User Consent API
* Clipboard

# How to use

### Adding dependency

To add this module as a dependency to your project, add this line to the dependencies section in your `bower.json`

```json
{
  "dependencies": {
    "purescript-otp-reader": "https://bitbucket.org/juspay/purescript-otp-reader.git#<version>"
  }
}
```

Replace `<version>` with the tag of the version you want to use.

To see all versions, check the [changelog](docs/Changelog.md).

_Note: please avoid writing a branch name after the `#`. Use either tags or  commit hashes_

### Example code

Example code for detecting OTP can be found [here](examples/Main.purs). Read the sections below for details.

### `Aff` vs `Flow`

If you're running in an Aff context, use the functions present in the `Juspay.OTP.Reader` module

If you're running in a `Flow` context, the same functions are either re-exported or wrapped inside `Flow` in the `Juspay.OTP.Reader.Flow` module

### Permissions

If you're using SMS Receiver or SMS inbox poller, you need Android SMS permissions.

Use `getSmsReadPermission` to check if SMS permission has already been granted or not

Use `requestSmsReadPermission` to ask the user for permission.

### OTP Listener

This is the main function you'll be using to read OTPs

```purescript
getOtpListener :: Array SmsReader -> Flow OtpListener
```


`SmsReader` is a type that represents a method of reading incoming SMSs. For example, `smsReceiver` and `smsPoller` are types of `SmsReader`s (all the available `SmsReader`s are mentioned in the [`Juspay.OTP.Reader` module](docs/Juspay/OTP/Reader.md))

`OtpListener` is a type alias for a record containing 2 functions:

```purescript
type OtpListener = {
  getNextOtp :: Flow Otp,
  setOtpRules :: Array OtpRule -> Flow Unit
}
```

Call the `getNextOtp` function when you want to wait for an OTP. The function will block until a received SMS succeeds OTP extraction (or an error occurs).

Call the `setOtpRules` function to set the OTP regex rules that need to be used for extracting the OTP from incoming SMSs.

At first, no OTP rules are set. So calling `getNextOtp` will not return any OTPs even if the correct SMS is received. Instead, it will be internally queued and will be processed only when you call `setOtpRules` for the first time. This is useful in situations where you don't know the bank until _after_ the OTP is triggered but you want to start listening for incoming SMSs as early as possible.

In situations where you already know the bank before the OTP is triggered, you can just call `setOtpRules` first and then `getNextOtp`.

You can call `getNextOtp` as many times as you want (inside a loop for example) to keep listening for OTPs.

### OTP Rules

You can either create your own OTP Rules using the `OtpRule` type or you can get the default bank OTP rules used by Godel through the `getGodelOtpRules` function.

``` purescript
getGodelOtpRules :: String -> Flow (F (Array OtpRule))
```

The OTP rules are decoded from JS so it can throw a decode error. Handle accordingly

Godel's OTP rules are defined in Godel's [config.js](https://bitbucket.org/juspay/godel-core/src/d4bc77f68b08ab87ae3c55349b6eeeaa4e9094cd/godel/src/main/js/juspay/payments/in.juspay.godel/config.js#lines-355)

### Module docs

Module docs are [here](docs/)