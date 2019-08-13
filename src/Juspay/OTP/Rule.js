exports["getGodelOtpRules'"] = function() {
  return [
    {
      "matches": {
        "sender": [
          ".*"
        ],
        "message": "One Time Password"
      },
      "otp": "\\d{6}"
    }
  ];
}