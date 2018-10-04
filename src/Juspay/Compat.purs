module Juspay.Compat where

import Prelude

import Control.Category as Cat
import Control.Monad.Aff (Aff, delay, makeAff, parallel, sequential) as Aff
import Control.Monad.Aff (nonCanceler)
import Control.Monad.Eff (Eff) as Eff
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Foreign (Foreign) as Foreign
import Data.Maybe (Maybe)
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
makeAff eff = Aff.makeAff (\callback -> eff (Right >>> callback) *> pure nonCanceler)

parAff :: forall e a. Array (Aff e a) -> Aff e a
parAff affs = Aff.sequential $ oneOf $ Aff.parallel <$> affs

id :: forall t a. Category a => a t t
id = Cat.id

match :: Regex -> String -> Maybe (Array (Maybe String))
match = Reg.match
