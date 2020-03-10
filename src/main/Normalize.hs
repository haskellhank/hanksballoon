{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Functor             (void)

import           Process                  (process)

main :: IO ()
main = do
  void $ process True
