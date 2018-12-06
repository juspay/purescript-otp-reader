var callbackMapper = require('@juspay/mystique-backend').helpers.android.callbackMapper;

exports.startSmsRetriever = function (success) {
  return function() {
    var cb = callbackMapper.map(function(data) {
      success(data)();
    });
    DUIGatekeeper.attach("SMS_RETRIEVE","{}",cb);
  }
};

exports.stopSmsRetriever = function () {
  DUIGatekeeper.detach(["SMS_RETRIEVE"]);
};

exports.md5Hash = function (s) {
  return DUIGatekeeper.getMd5(s);
};

exports.trackException = function (label) {
  return function(value) {
    JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
  }
};