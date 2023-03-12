module Main where

import qualified Csv.Types  as CSVT
import Csv.Stream.StreamCsv ( mkDailyAccountBalanceAndMonthlyPayout )

csvRunParameters :: CSVT.RunParameters
csvRunParameters = CSVT.RunParameters "./res/rates.csv" "./res/users.csv" "./out" 2 1200 1 25

-- TODO: Make command line interface
main :: IO ()
main = mkDailyAccountBalanceAndMonthlyPayout csvRunParameters

