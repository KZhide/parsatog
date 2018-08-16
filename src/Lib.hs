{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Lib where

import GSQL
import Parser

someFunc :: IO ()
someFunc = do
  print $ parse createQuery "" "create query splitMind(int i, uint x, vertex<mesh> m) for graph MyGraph returns (int)"
