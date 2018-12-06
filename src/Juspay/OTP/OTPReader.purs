module Juspay.OTP.OTPReader (
    OtpRule(..)
  , Sms(..)
  , Result(..)
  , getOtp
  , extractOtp
  , hashSms
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (findMap, (!!))
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (ignoreCase)
 -- To prevent shadow definitions warning for `id` from `Prelude`
import Juspay.Compat (Aff, Eff, makeAffCanceler, match, throw)

-- | Internal type used for encoding from and decoding to `OtpRule`.
newtype OtpRule' = OtpRule' {
  matches :: Matches,
  otp :: String,
  group :: NullOrUndefined Int
}

derive instance newtypeOtpRule' :: Newtype OtpRule' _
derive instance genericOtpRule' :: Generic OtpRule' _

-- | Internal type used for encoding from and decoding to `OtpRule`.
newtype Matches = Matches {
  sender :: Array String,
  message :: String
}

derive instance newtypeMatches :: Newtype Matches _
derive instance genericMatches :: Generic Matches _
instance encodeMatches :: Encode Matches where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeMatches :: Decode Matches where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


-- | Represents a rule for matching and extracting an OTP. An SMS will
-- | successfully match a given `OtpRule` if the following are true:
-- |  1. `message`, `otp` and the strings in the `sender` array are all
-- |    valid regex strings.
-- |  2. The SMS body matches the `message` regex
-- |  3. The SMS 'from' field matches one of the `sender` regexes
-- |  4. The OTP part of the SMS body successfuly matches the `otp` regex
-- |  5. If the `otp` regex contains groups, the `group` value references
-- |    a valid group matched by the `otp` regex. (defaults to 0 if `Nothing`)
newtype OtpRule = OtpRule {
  matches :: {
    sender :: Array String,
    message :: String
  },
  otp :: String,
  group :: Maybe Int
}

derive instance newtypeOtpRule :: Newtype OtpRule _
derive instance genericOtpRule :: Generic OtpRule _
instance encodeOtpRule :: Encode OtpRule where
  encode (OtpRule r) = genericEncode defaultOptions {unwrapSingleConstructors = true}
    (OtpRule' r{
      group = NullOrUndefined r.group,
      matches = wrap r.matches
    })
instance decodeOtpRule :: Decode OtpRule where
  decode f = do
    (OtpRule' r) <- genericDecode defaultOptions{unwrapSingleConstructors = true} f
    pure $ OtpRule r{
      group = unNullOrUndefined r.group,
      matches = unwrap r.matches
    }


-- | Type representing an SMS received using `smsReceiver` or `smsPoller`.
newtype Sms = Sms {
  from :: String,
  body :: String,
  time :: String
}

derive instance eqSms :: Eq Sms
derive instance newtypeSms :: Newtype Sms _
derive instance genericSms :: Generic Sms _
instance encodeSms :: Encode Sms where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeSms :: Decode Sms where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


-- | Type representing the final response from `getOtp`. When an OTP has
-- | been successfully extracted from an SMS, the `MatchedOtp` constructer is
-- | returned with the OTP itself and the SMS from which it was extracted.
-- | If an error occured, `Error` is returned.
data Result = MatchedOtp String Sms | Error String

derive instance genericResult :: Generic Result _
instance encodeResult :: Encode Result where encode = genericEncode defaultOptions {unwrapSingleConstructors = true}
instance decodeResult :: Decode Result where decode = genericDecode defaultOptions {unwrapSingleConstructors = true}


foreign import startSmsRetriever :: forall e. (String -> Eff e Unit) -> Eff e Unit
foreign import stopSmsRetriever :: forall e. Eff e Unit
foreign import md5Hash :: String -> String
foreign import trackException :: String -> String -> Unit



-- | Uses the SMS Retriever API to wait for an SMS. When an SMS is retrieved,
-- | an OTP extraction will be attempted using the given array of OTP rules
-- | If it fails to match the given rules, it will loop and wait again for
-- | another OTP.
getOtp :: forall e. Array OtpRule -> Aff e Result
getOtp rules = do
  sms <- retrieveSms
  case extractOtp [sms] rules of
    Just result -> pure result
    Nothing -> getOtp rules


-- | Starts the SMS Retriever API and waits for an SMS. Throws an error if
-- | if something goes wrong
retrieveSms :: forall e. Aff e Sms
retrieveSms = do
  s <- makeAffCanceler (\cb -> startSmsRetriever cb *> pure stopSmsRetriever)
  case runExcept $ decodeJSON s of
    Right sms -> pure sms
    Left _ -> case s of
      "TIMEOUT" -> retrieveSms
      err -> throw err


-- | Given a list of SMSs and a list of OTP rules, it will return the first OTP
-- | that matches one of the given rules or `Nothing` if none of them match.
extractOtp :: Array Sms -> Array OtpRule -> Maybe (Result)
extractOtp sms rules =
  findMap (\rule -> findMap (matchAndExtract rule) sms) rules


-- | Match a given SMS against a given rule and attempt to extract the OTP
-- | from the SMS. Returns `Nothing` if it fails. Ignores the SMS if a hash of
-- | it is present in the given `ProcessedSms` object.
matchAndExtract :: OtpRule -> Sms -> Maybe Result
matchAndExtract (OtpRule rule) sms =
  matchMessage sms >>= extract
  where
    matchMessage :: Sms -> Maybe Sms
    matchMessage (Sms sms') =
      let
        matches = isJust $ makeRegex rule.matches.message >>= (\r -> match r sms'.body)
      in if matches then Just (Sms sms') else Nothing

    extract :: Sms -> Maybe Result
    extract (Sms sms') =
      let
        group = fromMaybe 0 rule.group
        otp = join $ makeRegex rule.otp >>= (\r -> match r sms'.body) >>= (\arr -> arr !! group)
      in case otp of
        Just otp' -> Just $ MatchedOtp otp' (Sms sms')
        Nothing -> Nothing

    makeRegex :: String -> Maybe Regex
    makeRegex s = case regex s (ignoreCase) of
      Right r -> Just r
      Left err -> let _ = trackException "otp_reader" ("Regex syntax error \"" <> err <> "\" for rule: " <> encodeJSON (OtpRule rule)) in Nothing


-- | Used for tracking decode errors for values that should never have failed
-- | a decode (such as `Sms` values retreived from Android or `OtpRule`s)
decodeAndTrack :: forall a. Decode a => String -> Either MultipleErrors a
decodeAndTrack s = case runExcept $ decodeJSON s of
  Right a -> Right a
  Left err -> let _ = trackException "otp_reader" ("decode exception: " <> show err) in Left err

-- | Creates a hash value for a given SMS by MD5 hashing the SMS body and time
hashSms :: Sms -> String
hashSms (Sms s) = md5Hash $ s.body <> s.time