{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.DeepSeq          (NFData)
import           Data.Text                (Text)
import qualified Data.Text                as T (pack, intercalate)
import           Data.Text.Conversions    (ToText(toText), FromText(fromText))
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           GHC.Generics             (Generic)
import           Text.Regex.TDFA          ((=~))
import           Text.Regex.TDFA.Text     ()

-- Other must be 2 letters upper case A..Z, not enforced by type but by FromText
data Observatory = AU | US | FR | Other Text deriving (Eq, Ord, Show, Generic, NFData)

instance ToText Observatory where
  toText AU = "AU"
  toText US = "US"
  toText FR = "FR"
  toText (Other o) = o

instance FromText (Maybe Observatory) where
  fromText "AU" = Just AU
  fromText "US" = Just US
  fromText "FR" = Just FR
  fromText o | o =~ ("[A-Z]{2}" :: String) = Just $ Other o
  fromText _ = Nothing

-- in km
data Location = Location Float Float deriving (Show, Generic, NFData)

-- in Celsius
newtype Temperature = Temperature Float deriving (Show, Generic, NFData)

data Sample = Sample
  { timestamp :: UTCTime
  , location :: Location
  , temperature :: Temperature
  , observatory :: Observatory
  } deriving (Show, Generic, NFData)

-- normalized output
instance ToText Sample where
  toText (Sample timestamp (Location x y) (Temperature temp) observatory) =
    let printInt = T.pack . show . (round :: Float -> Int)
    in
      T.intercalate "|" $
        [ T.pack . formatTime defaultTimeLocale "%FT%R" $ timestamp
        , printInt x <> "," <> printInt y
        , printInt temp
        , toText observatory
        ]
