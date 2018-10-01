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

exports["requestSmsReadPermission'"] = function(success) {
  return function() {
    var cb = callbackMapper.map(function(params) {
      var permissions = JSON.parse(params);
      try {
        if(permissions["READ_SMS"] === true && permissions["RECEIVE_SMS"] === true) {
          success(true)();
        } else {
          success(false)();
        }
      } catch(e) {
        //TODO track this
        success(false)();
      }
    });
    DUIGatekeeper.requestSMSPermission(cb);
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
    //TODO implment
    console.log(label + ": " + value)
  }
};