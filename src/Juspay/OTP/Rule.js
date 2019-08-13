exports["getGodelOtpRules'"] = function() {
  return [
    {
      "matches": {
        "sender": [
          "ICICIB",
          "NPCIJP"
        ],
        "message": "One Time Password"
      },
      "otp": "\\d{6}",
      "bank": "ICICIDC",
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "NPCIJP"
        ],
        "message": "is OTP for"
      },
      "otp": "\\d{6}",
      "bank": "ICICIDC",
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "5676766",
          "NPCIJP"
        ],
        "message": "One Time Password"
      },
      "otp": "\\d{6}",
      "bank": "ICICICC",
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "5676766",
          "NPCIJP"
        ],
        "message": "IS OTP FOR"
      },
      "otp": "\\d{6}",
      "bank": "ICICICC",
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "NPCIJP"
        ],
        "message": "OTP is"
      },
      "otp": ".* OTP is (\\d{6})",
      "bank": "ICICIDC",
      "group": 1,
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "NPCIJP"
        ],
        "message": "OTP IS"
      },
      "otp": ".* OTP IS (\\d{6})",
      "bank": "ICICIDC",
      "group": 1,
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "5676766",
          "NPCIJP"
        ],
        "message": "OTP is"
      },
      "otp": "\\d{6}",
      "bank": "ICICICC",
      "otp_timeout": 60
    },
    {
      "matches": {
        "sender": [
          "ICICIB",
          "NPCIJP"
        ],
        "message": "OTP IS"
      },
      "otp": ".* OTP IS (\\d{6})",
      "bank": "ICICICC",
      "group": 1,
      "otp_timeout": 60
    }
  ];
}