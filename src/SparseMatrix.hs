{-# LANGUAGE TupleSections #-}
module SparseMatrix (
  pageRank
                    ) where

import Data.Graph (Graph, Edge, graphFromEdges, edges, outdegree)
import Data.Array ((!), range, bounds)

import qualified Numeric.LinearAlgebra as M
import Numeric.LinearAlgebra ((!#>), tr, mkSparse, cmap, size, konst)

import Numeric.Natural (Natural)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (keys, mapWithKey)

type RealNum = Double
type Vector = M.Vector RealNum
type Matrix = M.GMatrix
type Map a b = Map.Map a b

pageRank :: (Ord a) => RealNum -> Natural -> Map a [a] -> Map a RealNum
pageRank damp its as = Map.fromList . zip (keys as) . M.toList . pageRank' damp its $ as

pageRank' :: (Ord a) => RealNum -> Natural -> Map a [a] -> Vector
pageRank' damp its m =
  (!! fromIntegral its)
  . flip iterate (uniform . length $ m) . (.) (dampen damp) . (!#>)
  . pageRankMatrix $ m

uniform :: Int -> Vector
uniform n = konst (recip . fromIntegral $ n) n

dampen :: RealNum -> Vector -> Vector
dampen factor v = cmap ((+) ((1-factor)/(fromIntegral . size $ v)) . (*) factor) v

pageRankMatrix :: (Ord a) => Map a [a] -> Matrix
--pageRankMatrix = adjacency . normalize . unstick . connectionGraph
pageRankMatrix = adjacency . normalize . unstick . connectionGraph

normalize :: Graph -> Map Edge RealNum
normalize g = mapWithKey (\ (f,_) x -> x / out f) . Map.fromAscList . fmap (,1) . edges $ g
  where
    out = fromIntegral . (outdegree g !)

adjacency :: Map Edge RealNum -> Matrix
adjacency = tr . mkSparse . Map.toList

connectionGraph :: (Ord a) => Map a [a] -> Graph
connectionGraph = (\ (x,_,_) -> x) . graphFromEdges . fmap (\ (x,xs) -> (x,x,xs)) . Map.toList

unstick :: Graph -> Graph
unstick g = fmap (\ l -> if null l then (range . bounds) g else l) g
