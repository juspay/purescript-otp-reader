module Juspay.Compat where

import Prelude

import Control.Category as Cat
import Control.Monad.Aff (Aff, delay, makeAff) as Aff
import Control.Monad.Aff (ParAff(..))
import Control.Monad.Eff (Eff) as Eff
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Data.Foldable (oneOf)
import Data.Foreign (Foreign) as Foreign
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (Regex)
import Data.String.Regex as Reg
import Data.Time.Duration (Milliseconds)

type Aff e a = Aff.Aff e a
type Eff e a = Eff.Eff e a

type Foreign = Foreign.Foreign

delay :: forall e. Milliseconds -> Aff e Unit
delay = Aff.delay

liftEff :: forall a m eff. MonadEff eff m => Eff eff a -> m a
liftEff = Eff.liftEff

makeAff :: forall e a. ((a -> Eff e Unit) -> Eff e Unit) -> Aff e a
makeAff eff = Aff.makeAff (\err sc -> eff sc)

parAff :: forall e a. Array (Aff e a) -> Aff e a
parAff affs = unwrap $ oneOf $ ParAff <$> affs

id :: forall t a. Category a => a t t
id = Cat.id

match :: Regex -> String -> Maybe (Array (Maybe String))
match = Reg.match
