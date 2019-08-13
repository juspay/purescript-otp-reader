var callbackMapper = require('@juspay/mystique-backend').helpers.android.callbackMapper;

exports["getSmsReadPermission'"] = function () {
  try {
    var data = DUIGatekeeper.checkReadSMSPermission();
    var permissions = JSON.parse(data);
    if(permissions["READ_SMS"] === true && permissions["RECEIVE_SMS"] === true) {
      return true
    } else {
      return false
    }
  } catch(e) {
    //TODO track this
    return false
  }
};

exports["requestSmsReadPermission'"] = function(callback) {
  return function() {
    var cb = callbackMapper.map(function(params) {
      var permissions = JSON.parse(params);
      try {
        if(permissions["READ_SMS"] === true && permissions["RECEIVE_SMS"] === true) {
          callback(true)();
        } else {
          callback(false)();
        }
      } catch(e) {
        //TODO track this
        callback(false)();
      }
    });
    DUIGatekeeper.requestPermission(["android.permission.READ_SMS", "android.permission.RECEIVE_SMS"], 10, cb);
  }
}

exports.startSmsReceiver = function (success) {
  return function() {
    var cb = callbackMapper.map(function(data) {
      success(data)();
    });
    DUIGatekeeper.attach("SMS_RECEIVE","{}",cb);
  }
};

exports.stopSmsReceiver = function () {
  DUIGatekeeper.detach(["SMS_RECEIVE"]);
};

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

exports.readSms = function (time) {
  return function() {
    return DUIGatekeeper.fetchFromInbox(time);
  }
};

exports.md5Hash = function (s) {
  return DUIGatekeeper.getMd5(s);
};

exports.trackException = function (label) {
  return function(value) {
    JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
  }
};