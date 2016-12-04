{-# LANGUAGE TupleSections #-}
module Main where

import SparseMatrix

import Data.List (sortOn)
import qualified System.IO.Strict as Strict
import Data.Map.Strict (fromList, toList)

main :: IO ()
main = Strict.readFile "hackage.graph.hs" >>= mapM_ print
  . (fmap . fmap) (*100) . take 20 . sortOn (negate . snd) . toList
  . pageRank 0.85 2
  . fromList . (read :: String -> [(String,[String])])
