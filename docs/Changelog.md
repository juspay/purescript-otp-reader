All versions and their changelogs. The version names are git tags. So if you want to checkout that version's commit, checkout the tag with that name (eg: `git checkout v0.2`).

For a given `vX.X` tag, there might be a `vX.X-purs11` tag which is the same version but in PureScript version 11 in case you need it.

## v0.2 / v0.2-purs11

Features:

* Added `clipboard` as an `SmsReader` for reading OTPs from Clipboard.
* Along with OTP, return the SMS it was extracted from and the `SmsReader` that captured it
* Every `SmsReader` now has a String that represents its name. A `getName` function was also added that can be used to get the name of a given `SmsReader`

NPM Dependency URL:

* https://bitbucket.org/juspay/purescript-otp-reader.git#v0.2
* https://bitbucket.org/juspay/purescript-otp-reader.git#v0.2-purs11

## v0.1 / v0.1-purs11
First release

Features:

* SMS Receiver
* SMS Poller
* Functions to get SMS Read permissions
* Functions to get OTP Rules from Godel

NPM Dependency URL:

* https://bitbucket.org/juspay/purescript-otp-reader.git#v0.1
* https://bitbucket.org/juspay/purescript-otp-reader.git#v0.1-purs11