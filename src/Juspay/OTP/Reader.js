const callbackMapper = {
  map : function(fn) {
    if(typeof window.__FN_INDEX !== 'undefined' && window.__FN_INDEX !== null) {
      var proxyFnName = 'F' + window.__FN_INDEX;
      window.__PROXY_FN[proxyFnName] = fn;
      window.__FN_INDEX++;
      return proxyFnName;
    } else {
      throw new Error("Please initialise window.__FN_INDEX = 0 in index.js of your project.");
    }
  }
}

exports["getGodelOtpRules'"] = function() {
  try {
    //Temporarily create an iframe just for loading godel config (so that this microapp's context isn't modified)
    var configString = DUIGatekeeper.loadFileInDUI('payments/in.juspay.godel/v1-config.jsa');
    var iframe = document.createElement('iframe');
    document.body.appendChild(iframe);
    var w = iframe.contentWindow;

    w.godelVersion = "0.0rc0_0";
    w.godelRemotesVersion = "0.0rc0_0";
    w.clientId = "otp_reader";
    w.eval.call(w, configString);
    var config = JSON.parse(w.getConfigString());

    document.body.removeChild(iframe);
    return config.otp_rules;
  } catch(e) {
    console.error(e);
    return null;
  }
}

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

exports.readSms = function (time) {
  return function() {
    return DUIGatekeeper.fetchFromInbox(time);
  }
};

exports.getCurrentTime = function() {
  return Date.now();
}

exports.isClipboardSupported = function() {
  return typeof DUIGatekeeper.onClipboardChange == "function";
}

exports.onClipboardChange = function(callback) {
  return function() {
    var cb = callbackMapper.map(function(s) {
      callback(s)();
    })
    DUIGatekeeper.onClipboardChange(cb);
  }
}

exports.md5Hash = function (s) {
  return DUIGatekeeper.getMd5(s);
};

exports.trackException = function (label) {
  return function(value) {
    JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
  }
};