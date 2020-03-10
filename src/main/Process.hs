{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Process where

import           Control.DeepSeq          (NFData, force)
import           Control.Exception        (throw, handle)
import           Control.Monad            (forever, when)
import           Control.Monad.Except     (runExceptT, throwError)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Loops      (unfoldrM)
import           Control.Monad.State      (execStateT, get, put)
import           Data.Bool                (bool)
import           Data.Foldable            (find, foldl', traverse_)
import           Data.Functor             ((<&>))
import           Data.List                (sortOn)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map (insertWith)
import           Data.Maybe               (mapMaybe)
import           Data.Semigroup           (Min(Min), Max(Max))
import           Data.Text                (Text)
import           Data.Text                as T (split, pack)
import           Data.Text.Conversions    (ToText(toText), FromText(fromText))
import qualified Data.Text.IO             as T (hGetLine, hPutStrLn)
import           Data.Time                (UTCTime)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
import           GHC.Generics             (Generic)
import           System.IO                (Handle, stdout, stdin)
import           System.IO.Error          (isEOFError)
import           Text.Parsec              (parse, many, many1, digit, char)
import           Text.Parsec.Combinator   (anyToken)
import           Text.Read                (read)

import           Types                    (Observatory(AU, US, FR, Other), Location(Location),
                                           Temperature(Temperature), Sample(Sample, timestamp))

parseLogLine :: Text -> Maybe Sample
parseLogLine line =
  case T.split (== '|') line of
    [a, b, c, d] -> either (const Nothing) pure $ do
      timestamp <- parse (parseTimeM False defaultTimeLocale "%FT%R" =<< many anyToken) "timestamp" a
      observatory <- parse (maybe (fail "boo") pure . fromText . T.pack =<< many anyToken) "observatory" d
      let intP = (read :: String -> Int) <$> many1 digit
      let distanceP = intP <&>
           (case observatory of
             AU -> id
             US -> (/ 0.621371) -- miles
             FR -> (/ 1000.0) -- meters
             Other _ -> id) . (fromIntegral :: Int -> Float)
      let temperatureP = intP <&>
           (case observatory of
             AU -> id
             US -> (/ 9.0) . (* 5.0) . subtract 32.0 -- fahrenheit
             FR -> subtract 273.15 -- kelvin
             Other _ -> subtract 273.15) . (fromIntegral :: Int -> Float)
      location <- parse (Location <$> (distanceP <* char ',') <*> distanceP) "location" b
      temperature <- parse (Temperature <$> temperatureP) "temperature" c
      pure $ Sample timestamp location temperature observatory
    _ -> Nothing

data Stats = Stats
  { lastTimestamp :: Maybe UTCTime
  , minTemperature :: Maybe (Min Float)
  , maxTemperature :: Maybe (Max Float)
  , sumTemperature :: Double
  , observations :: Map Observatory Int
  , lastLocation :: Maybe Location
  , distance :: Double
  } deriving (Show, Generic, NFData)

initStats :: Stats
initStats = Stats Nothing Nothing Nothing 0.0 mempty Nothing 0.0

updateStats :: Stats -> Sample -> Stats
updateStats Stats{..} (Sample newTimestamp (Location x y) (Temperature temp) observatory) =
  Stats
    (Just newTimestamp)
    (minTemperature <> Just (Min temp))
    (maxTemperature <> Just (Max temp))
    ((+ sumTemperature) . realToFrac $ temp)
    (Map.insertWith (+) observatory 1 observations)
    (Just $ Location x y)
    ((+ distance) . realToFrac . maybe 0 (\(Location lastX lastY) -> sqrt ((x - lastX) * (x - lastX) + (y - lastY) * (y - lastY))) $ lastLocation)

getLineMaybe :: Handle -> IO (Maybe Text)
getLineMaybe = handle (traverse throw . find (not . isEOFError) . Just) . fmap Just . T.hGetLine

-- read `count` lines from file handle `fh` or fewer if end of file
readLines :: Handle -> Int -> IO [Text]
readLines fh count =
  flip unfoldrM (count :: Int) $ \remaining -> bool (pure Nothing) (fmap (, pred remaining) <$> getLineMaybe fh) $ remaining > 0

process :: Bool -> IO Stats
process printNormalized = do
  let maxBatch = 2500
  (_, stats) <- flip execStateT (mempty, initStats) . runExceptT . forever $ do
    (oldBatch, oldStats) <- get
    -- new input to replenish buffer
    input <- liftIO $ readLines stdin (maxBatch * 2 - length oldBatch)
    -- add new input to old data, sort, drop invalid (out of sequence) sample and split off `maxBatch` for output
    let buffer = sortOn timestamp . (oldBatch <>) . mapMaybe parseLogLine $ input
    let (output, newBatch) = splitAt maxBatch . maybe id (\old -> dropWhile $ (< old) . timestamp) (lastTimestamp oldStats) $ buffer
    -- update stats
    let newStats = foldl' updateStats oldStats output
    -- output for `normalize`
    liftIO . when printNormalized . traverse_ (T.hPutStrLn stdout) . map toText $ output
    put . force $ (newBatch, newStats)
    -- terminate loop if no input (end of file)
    traverse_ (const $ throwError ()) . find null . Just $ input
  pure $ stats
