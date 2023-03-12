module Main where

import Csv.Stream.StreamCsv ( mkDailyAccountBalanceAndMonthlyPayout )
import qualified Csv.Types as T

main :: IO ()
main = mkDailyAccountBalanceAndMonthlyPayout $ T.RunParameters "./res/rates.csv" "./res/users.csv" "./out/results.csv" 2 1200 1 25

