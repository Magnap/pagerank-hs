module LocalChan (
  pageRankSTM
                 ) where

import Control.Monad.STM (STM)

import Numeric.Natural (Natural)
import qualified Data.Map.Strict as Map

type RealNum = Double
type Map a b = Map.Map a b

pageRankSTM :: (Ord a) => RealNum -> Natural -> Map a [a] -> STM (Map a RealNum)
pageRankSTM = undefined
