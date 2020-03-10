{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Foldable            (for_)
import qualified Data.Map                 as Map (elems, toList)
import           Data.Semigroup           (Min(getMin), Max(getMax))
import           Data.Text.Conversions    (ToText(toText))
import           Text.Printf              (printf)

import           Process                  (Stats(Stats, minTemperature, maxTemperature, sumTemperature,
                                           observations, distance), process)

main :: IO ()
main = do
  Stats{..} <- process False
  for_ minTemperature $ printf "min. temp = %.1f Celsius\n" . getMin
  for_ maxTemperature $ printf "max. temp = %.1f Celsius\n" . getMax
  printf "mean temp = %.1f Celsius\n" $ sumTemperature / (fromIntegral . sum $ Map.elems observations)
  for_ (Map.toList observations) $ \(observatory, count) -> printf "%s = %d\n" (toText observatory) count
  printf "distance = %.1f km\n" $ distance
