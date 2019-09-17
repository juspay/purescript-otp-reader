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
    var configString = JBridge.loadFileInDUI('payments/in.juspay.godel/v1-config.jsa');
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
    var data = JBridge.checkReadSMSPermission();
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
    try {
      var cb = callbackMapper.map(function(params) {
        try {
          var permissions = JSON.parse(params);
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
      JBridge.requestPermission(["android.permission.READ_SMS", "android.permission.RECEIVE_SMS"], 10, cb);
    } catch(e) {
      //TODO track this
      callback(false)();
    }
  }
}

exports.startSmsReceiver = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          var cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          JBridge.attach("SMS_RECEIVE","{}",cb);
        } catch(e) {
          //TODO track this
          setTimeout(function() { callback(left(e))(); }, 0);
        }
      }
    }
  }
};

exports.stopSmsReceiver = function () {
  try {
    JBridge.detach(["SMS_RECEIVE"]);
  } catch(e) {
    //TODO track this
  }
};

exports.isConsentAPISupported = function() {
  try {
    // User consent API was added along with androidX migration in godel-core
    var usingAndroidX = JBridge.getResourceByName("using_androidx");
    return usingAndroidX === "true";
  } catch(e) {
    return false;
  }
}

exports.startSmsConsentAPI = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          var cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          JBridge.attach("SMS_CONSENT","{}",cb);
        } catch(e) {
          //TODO track this
          setTimeout(function() { callback(left(e))(); }, 0);
        }
      }
    }
  }
};

exports.stopSmsConsentAPI = function () {
  try {
    JBridge.detach(["SMS_CONSENT"]);
  } catch(e) {
    //TODO track this
  }
};

exports.readSms = function (time) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          return right(JBridge.fetchFromInbox(time));
        } catch(e) {
          //TODO track this
          return left(e);
        }
      }
    }
  }
};

exports.getCurrentTime = function() {
  return Date.now();
}

exports.isClipboardSupported = function() {
  return typeof JBridge.onClipboardChange == "function";
}

exports.onClipboardChange = function(callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          var cb = callbackMapper.map(function(s) {
            callback(right(s))();
          })
          JBridge.onClipboardChange(cb);
        } catch(e) {
          //TODO track this
          setTimeout(function() { callback(left(e))(); }, 0);
        }
      }
    }
  }
}

exports.md5Hash = function (s) {
  return JBridge.getMd5(s);
};

exports.trackException = function (label) {
  return function(value) {
    JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
  }
};