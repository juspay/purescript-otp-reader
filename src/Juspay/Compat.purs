module Juspay.Compat where

import Prelude

import Control.Monad.Aff (Aff, delay, makeAff) as Aff
import Control.Monad.Aff (ParAff(..))
import Control.Monad.Eff (Eff) as Eff
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Data.Foldable (oneOf)
import Data.Foreign (Foreign, MultipleErrors) as Foreign
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds)

type Aff e a = Aff.Aff e a
type Eff e a = Eff.Eff e a

type Foreign = Foreign.Foreign
type MultipleErrors = Foreign.MultipleErrors

delay :: forall e. Milliseconds -> Aff e Unit
delay = Aff.delay

liftEff :: forall a m eff. MonadEff eff m => Eff eff a -> m a
liftEff = Eff.liftEff

makeAff :: forall e a. ((a -> Eff e Unit) -> Eff e Unit) -> Aff e a
makeAff eff = Aff.makeAff (\err sc -> eff sc)

parAff :: forall e a. Array (Aff e a) -> Aff e a
parAff affs = unwrap $ oneOf $ ParAff <$> affs