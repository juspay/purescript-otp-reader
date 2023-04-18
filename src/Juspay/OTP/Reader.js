import { callbackMapper } from 'presto-ui';

export const getGodelOtpRulesImpl = function() {
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
    return [];
  }
}

export const getSmsReadPermissionImpl = function () {
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

export const requestSmsReadPermissionImpl = function(callback) {
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

export const startSmsReceiver = function (callback) {
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

export const stopSmsRetriever = function () {
  try {
    JBridge.detach(["SMS_RETRIEVER"]);
  } catch(e) {
    //TODO track this
  }
};

export const startSmsRetriever = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          var cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          JBridge.attach("SMS_RETRIEVER","{}",cb);
        } catch(e) {
          //TODO track this
          setTimeout(function() { callback(left(e))(); }, 0);
        }
      }
    }
  }
};

export const fetchSmsRetriever = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          var cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          JBridge.execute("SMS_RETRIEVER","getOtp","{}",cb);
        } catch(e) {
          //TODO track this
          setTimeout(function() { callback(left(e))(); }, 0);
        }
      }
    }
  }
};

export const cancelFetchSmsRetriever = function () {
  return function() {
    try {
      var cb = callbackMapper.map(function(data) {
        //Add logs here to debug
      });
      JBridge.execute("SMS_RETRIEVER","getOtp","{}",cb);
    } catch(e) {
      //TODO track this
    }
  }
};


export const stopSmsReceiver = function () {
  try {
    JBridge.detach(["SMS_RECEIVE"]);
  } catch(e) {
    //TODO track this
  }
};

export const isConsentAPISupported = function() {
  try {
    // User consent API was added along with androidX migration in godel-core
    var usingAndroidX = JBridge.getResourceByName("using_androidx");
    return usingAndroidX === "true";
  } catch(e) {
    return false;
  }
}

export const startSmsConsentAPI = function (callback) {
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

export const stopSmsConsentAPI = function () {
  try {
    JBridge.detach(["SMS_CONSENT"]);
  } catch(e) {
    //TODO track this
  }
};

export const readSms = function (time) {
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

export const getCurrentTime = function() {
  return Date.now();
}

export const isClipboardSupported = function() {
  return typeof JBridge.onClipboardChange == "function";
}

export const onClipboardChange = function(callback) {
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

export const md5Hash = function (s) {
  return JBridge.getMd5(s);
};

// Previous Exception log : For Reference
// export const trackException = function (label) {
//   return function(value) {
//     JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
//   }
// };

const loopedFunction = function(){
  return loopedFunction
}
const getTracker = function(){
  var trackerJson = window.JOS && window.JOS.tracker || {};
  if (typeof trackerJson._trackException != "function"){
      trackerJson._trackException = loopedFunction;
  }
  return trackerJson;
}
const tracker = getTracker();

export const _trackException = function(message){
    return function(stacktrace) {
        tracker._trackException("Action")("System")("DETAILS")(message)(stacktrace)();
    }
}
