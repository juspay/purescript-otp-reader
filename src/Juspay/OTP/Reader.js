import * as prestoUI from "presto-ui";
const callbackMapper = prestoUI.callbackMapper;

const loopedFunction = function(){
  return loopedFunction
}
var newInterface = false;
const getTracker = function(){
  const trackerJson = window.JOS && window.JOS.tracker || {};
  if (typeof trackerJson._trackException !== "function"){
    trackerJson._trackException = loopedFunction;
  }
  if (typeof trackerJson.__trackLifeCycle == "function" ){
    newInterface = true;
  }
  if (typeof trackerJson._trackAction != "function"){
      trackerJson._trackAction = loopedFunction;
  }
  if (typeof trackerJson.__trackAction != "function"){
    trackerJson.__trackAction = loopedFunction;
  }
  return trackerJson;
}
const tracker = getTracker();

export const updateUnmatchedSmsImpl = function(smsArray){
  return function(){
    if (window && window.updateUnmatchedSms){
        window.updateUnmatchedSms = window.updateUnmatchedSms.concat(smsArray);
    }
    else{
      window.updateUnmatchedSms = smsArray;
    }
  }
}

export const updateExtractedOtpStatus = function(hasOtpExtracted){
  return function(){
    if(window){
      if (window.isOtpExtracted != null || window.isOtpExtracted != undefined) {
        window.isOtpExtracted = hasOtpExtracted || window.isOtpExtracted;
      }
      else {
        window.isOtpExtracted = hasOtpExtracted;
      }
    }
  }
}

export const replaceDigitWithX = function(s){
  try {
    return s.replace(/[0-9]/g, "X")
  }
  catch (e){
    return "";
  }
}

export const getLogJson = function(){
  return window.logJson
}

export const trackAction = function(value){
  return function(lbl){
    return function(){
      const sub = "system";
      const label = lbl;
      const json = (window.logJson && typeof window.logJson == "object") ? window.logJson : {};
      const level = "info";
      if(typeof value == "string"){
          try{
              value = JSON.parse(value);
          }
          catch(e){
              //Ignore
          }
      }
      newInterface ? tracker.__trackAction(sub)(level)(label)(value)(json)() : tracker._trackAction(sub)(level)(label)(value)();
    }
  }
}

export const trackException = function (label) {
  return function(value) {
    JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
  }
};

export const getGodelOtpRulesImpl = function() {
  try {
    //Temporarily create an iframe just for loading godel config (so that this microapp's context isn't modified)
    const configString = window.JBridge.loadFileInDUI("payments/in.juspay.godel/v1-config.jsa");
    const iframe = document.createElement("iframe");
    document.body.appendChild(iframe);
    const w = iframe.contentWindow;

    w.godelVersion = "0.0rc0_0";
    w.godelRemotesVersion = "0.0rc0_0";
    w.clientId = "otp_reader";
    // Don't remove this eslint ignore
    // eslint-disable-next-line
    w.eval.call(w, configString);
    const config = JSON.parse(w.getConfigString());

    document.body.removeChild(iframe);
    newInterface ? tracker.__trackAction("system")("info")("page")({godel_config_version : config.version})({})() : tracker._trackAction("system")("info")("page")({godel_config_version : config.version})();
    return config.otp_rules;
  } catch(e) {
    console.error(e);
    return [];
  }
}

export const getSmsReadPermissionImpl = function () {
  try {
    const data = window.JBridge.checkReadSMSPermission();
    const permissions = JSON.parse(data);
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

export const isSmsPermissionGrantedImpl = function () {
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
      const cb = callbackMapper.map(function(params) {
        try {
          const permissions = JSON.parse(params);
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
      window.JBridge.requestPermission(["android.permission.READ_SMS", "android.permission.RECEIVE_SMS"], 10, cb);
    } catch(e) {
      //TODO track this
      callback(false)();
    }
  }
}

exports.shouldShowRequestPermissionRationale = function() {
  try {
    if(typeof JBridge.shouldShowRequestPermissionRationale == "function") {
      var shouldShowREAD = JBridge.shouldShowRequestPermissionRationale("android.permission.READ_SMS");
      var shouldShowRECEIVE = JBridge.shouldShowRequestPermissionRationale("android.permission.RECEIVE_SMS");

      var isBooleanString = function(a) { return a === "true" || a === "false" }

      if(!isBooleanString(shouldShowREAD) || !isBooleanString(shouldShowRECEIVE)) {
        return null;
      } else {
        return shouldShowREAD === "true" && shouldShowRECEIVE === "true"
      }
    }
  } catch(e) {
    //TODO track this
    return null;
  }
}

export const startSmsReceiver = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          const cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          window.JBridge.attach("SMS_RECEIVE","{}",cb);
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
    window.JBridge.detach(["SMS_RETRIEVER"]);
  } catch(e) {
    //TODO track this
  }
};

export const startSmsRetriever = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          const cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          window.JBridge.attach("SMS_RETRIEVER","{}",cb);
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
          const cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });
          window.JBridge.execute("SMS_RETRIEVER","getOtp","{}",cb);
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
      const cb = callbackMapper.map(function(data) {
        //Add logs here to debug
      });
      window.JBridge.execute("SMS_RETRIEVER","getOtp","{}",cb);
    } catch(e) {
      //TODO track this
    }
  }
};


export const stopSmsReceiver = function () {
  try {
    window.JBridge.detach(["SMS_RECEIVE"]);
  } catch(e) {
    //TODO track this
  }
};

export const isConsentAPISupported = function() {
  try {
    // User consent API was added along with androidX migration in godel-core
    const usingAndroidX = window.JBridge.getResourceByName("using_androidx");
    return usingAndroidX === "true";
  } catch(e) {
    return false;
  }
}

var consentStartedLogged = false;

export const startSmsConsentAPI = function (callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          const cb = callbackMapper.map(function(data) {
            callback(right(data))();
          });


          window.JBridge.attach("SMS_CONSENT","{}",cb);
          if(!consentStartedLogged) {
            try{
              const sub = "system";
              const label = "SMS_CONSENT";
              const level = "info";
              const value = {sms_consent_listener: "SmsConsent listener started successfully"}
              const json = (window.logJson && typeof window.logJson == "object") ? window.logJson : {};
              newInterface ? tracker.__trackAction(sub)(level)(label)(value)(json)()
                            : tracker._trackAction(sub)(level)(label)(value)();
              consentStartedLogged = true
            } catch(err) {}
          }

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
    window.JBridge.detach(["SMS_CONSENT"]);
  } catch(e) {
    //TODO track this
  }
};

export const readSms = function (time) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          return right(window.JBridge.fetchFromInbox(time));
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
  return typeof window.JBridge.onClipboardChange === "function";
}

export const onClipboardChange = function(callback) {
  return function(left) {
    return function(right) {
      return function() {
        try {
          const cb = callbackMapper.map(function(s) {
            callback(right(s))();
          })
          window.JBridge.onClipboardChange(cb);
        } catch(e) {
          //TODO track this
          setTimeout(function() { callback(left(e))(); }, 0);
        }
      }
    }
  }
}

export const md5Hash = function (s) {
  return window.JBridge.getMd5(s);
};

// Previous Exception log : For Reference
// export const trackException = function (label) {
//   return function(value) {
//     window.JBridge.trackEvent("dui", "error", "OTPReader_Exception", label + ": " + value)
//   }
// };


export const _trackException = function(message){
  return function(stacktrace) {
    tracker._trackException("Action")("System")("DETAILS")(message)(stacktrace)();
  }
}
