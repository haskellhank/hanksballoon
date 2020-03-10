{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad            ((<=<))
import           Data.Foldable            (for_)
import           Data.Maybe               (isJust)
import           Data.Random.Normal       (mkNormals')
import           Data.Text                (Text)
import           Data.Text                as T (pack, intercalate)
import           Data.Text.Conversions    (ToText(toText))
import qualified Data.Text.IO             as T (hPutStrLn)
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime(UTCTime), NominalDiffTime, addUTCTime)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           System.Environment       (getArgs)
import           System.IO                (stdout, stderr)
import           Test.QuickCheck          (Gen, frequency, generate, arbitrary, choose)
import           Text.Read                (read, readMaybe)

import           Types                    (Observatory(AU, US, FR, Other), Location(Location),
                                           Temperature(Temperature), Sample(Sample))

genObservatory :: Gen Observatory
genObservatory = frequency [(33, pure AU), (33, pure US), (33, pure FR), (1, pure $ Other "XY")]

genTimestamp :: NominalDiffTime -> UTCTime -> Gen UTCTime
genTimestamp sigma mean =
  flip addUTCTime mean . fromRational . (toRational :: Float -> Rational) . head . mkNormals' (0, fromRational . toRational $ sigma) <$> arbitrary

genLocation :: Gen Location
genLocation = Location <$> choose (0, 100) <*> choose (0, 100)

genTemperature :: Gen Temperature
genTemperature = Temperature . head . mkNormals' (20.0, 5.0) <$> arbitrary

genSample :: UTCTime -> Gen Sample
genSample timestamp =
  Sample timestamp <$> genLocation <*> genTemperature <*> genObservatory

genLogWithErrors :: Sample -> Gen Text
genLogWithErrors (Sample timestamp (Location x y) (Temperature temp) observatory) =
  let printDistance =
       T.pack . show . (round :: Float -> Int) .
         (case observatory of
           AU -> id
           US -> (* 0.621371) -- miles
           FR -> (* 1000) -- meters
           Other _ -> id)
      printTemp =
        T.pack . show . (round :: Float -> Int) . case observatory of
          AU -> id
          US -> (+ 32.0) . (/ 5.0) . (* 9.0) -- fahrenheit
          FR -> (+ 273.15) -- kelvin
          Other _ -> (+ 273.15) -- kelvin
  in fmap (T.intercalate "|") . traverse (\val -> frequency [ (99, pure val), (1, pure "foo") ]) $
      [ T.pack . formatTime defaultTimeLocale "%FT%R" $ timestamp
      , printDistance x <> "," <> printDistance y
      , printTemp temp
      , toText observatory
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] | (isJust (readMaybe arg :: Maybe Int)) ->
      let count = read arg :: Int
          startTime = UTCTime (fromGregorian 2020 1 1) 0 -- Jan 1st, 2020  00:00 UTC
          sigma = fromIntegral (500 :: Int) -- 500 seconds sigma
      in for_ [0..count] $ T.hPutStrLn stdout <=< generate . (genLogWithErrors <=< genSample <=< genTimestamp sigma . flip addUTCTime startTime . fromIntegral)
    _       -> T.hPutStrLn stderr "please specify a line count"
