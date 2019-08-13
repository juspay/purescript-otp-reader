module Juspay.OTP.Rule where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (class Decode, class Encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)

foreign import getGodelOtpRules' :: forall e. Eff e Foreign

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

getGodelOtpRules :: forall e. String -> Eff e (Either MultipleErrors (Array OtpRule))
getGodelOtpRules bank = do
  f <- getGodelOtpRules'
  pure $ runExcept $ decode f