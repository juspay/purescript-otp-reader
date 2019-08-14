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