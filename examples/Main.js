exports.init = function() {
  window.__PROXY_FN = {};
  top.window.__PROXY_FN = window.__PROXY_FN;
  window.__FN_INDEX = 0;
}

exports.getTime = Date.now;