module Csv.Utils ( module Data.Conduit.List
             , module Data.Conduit.Combinators
             , module Data.Maybe
             , module Data.CSV.Conduit.Conversion
             , CCsv.transformCSV
             , CCsv.intoCSV
             , CCsv.runResourceT
             , C.runConduit
             , parseDate
             , parseDouble
             , percentToDecimal
             , csvFormat
             , strictByteStringToStrictText
             , stringToStrictByteString
             , year
             , month
             , day
             , ) where

import Data.Text.Encoding              as Te
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Maybe            as M
import qualified Text.Read             as R
import qualified Data.CSV.Conduit      as CCsv
import qualified Data.Conduit          as C
import qualified Data.Time.Calendar    as Cal
import Data.CSV.Conduit.Conversion        ( parseNamedRecord
                                          , toNamedRecord )
import Data.Conduit.Combinators           ( sourceFile
                                          , sinkFile )
import Data.Conduit.List hiding           ( concat
                                          , head )
import Data.Maybe                         ( maybeToList
                                          , maybe
                                          , fromMaybe )
import Data.Time                          ( parseTimeM
                                          , defaultTimeLocale
                                          , Day )

csvFormat :: CCsv.CSVSettings
csvFormat = CCsv.CSVSettings ',' Nothing

year :: Day -> Integer
year date = y
  where (y,_,_) = Cal.toGregorian date

month :: Day -> Int
month date = m
  where (_,m,_) = Cal.toGregorian date

day :: Day -> Int
day date = d
  where (_,_,d) = Cal.toGregorian date

parseDate :: T.Text -> Either String Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" . T.unpack

instance String ~ err => MonadFail (Either err) where
  fail :: forall a. String -> Either String a
  fail = Left

parseDouble :: T.Text -> Either String Double
parseDouble rate = M.maybe (Left errorMsg) Right . R.readMaybe . T.unpack $ rate
  where
  errorMsg = "Could not parse " <> show rate <> " as a double"

strictByteStringToStrictText ::B.ByteString -> T.Text
strictByteStringToStrictText = Te.decodeUtf8

stringToStrictByteString :: String -> B.ByteString
stringToStrictByteString = Te.encodeUtf8 . T.pack

percentToDecimal :: Double -> Double
percentToDecimal percent = percent / 100

