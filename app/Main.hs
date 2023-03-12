module Main where

import qualified Csv.Types  as CSVT
import Csv.Stream.StreamCsv ( mkDailyAccountBalanceAndMonthlyPayout )
import System.Environment   ( getArgs )
import System.Exit          ( exitFailure
                            , exitSuccess )

main :: IO ()
main = getArgs >>= run

-- main :: IO ()
-- main = mkDailyAccountBalanceAndMonthlyPayout $ CSVT.RunParameters "./res/rates.csv" "./res/users.csv" "./out" 2 2100 25 1

run :: [FilePath] -> IO ()
run ["-h"] = usage   >> exit
run ["-v"] = version >> exit
run ["--payout-rate"     , payoutRrate,
     "--max-age"         , maxAge,
     "--interest-path"   , interestPath,
     "--in-path"         , inPath,
     "--out-dir"         , outDir,
     "--payout-day"      , payoutDay,
     "--contribution-day" , contributionDay ] = 
  let runParameters = CSVT.RunParameters interestPath inPath outDir (read payoutRrate) (read maxAge) (read payoutDay) (read contributionDay)
  in mkDailyAccountBalanceAndMonthlyPayout runParameters >> exit
run _     = putStrLn "Not a valid command" >> die

usage :: IO ()
usage   = putStrLn "Usage: balance-payouts-interview-solution [--payout-rate] <payout rate> [--max-age] <max age> [--interest-path] <file path> [--in-path] <file path> [--out-dir] <dir path> [--payout-day] <day of month> [--contribution-day] <day of month>"

version :: IO ()
version = putStrLn "balance-payouts-interview-solution 0.1.0.0"

exit :: IO a
exit    = exitSuccess

die :: IO a
die     = exitFailure