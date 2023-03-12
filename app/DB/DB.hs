module DB.DB ( insertUser
             , insertBalancePayout ) where

import qualified Csv.Types    as CSVT
import qualified Data.Text    as T
import Database.Beam.Sqlite   ( runBeamSqliteDebug )
import Database.SQLite.Simple (open
                              , Connection)
import DB.Types               ( BalancePayoutT(BalancePayout)
                              , UserT(User)
                              , BalancePayoutDB(bpdb_users, bpdb_balancePayout) )
import Database.Beam          ( insertValues
                              , insert
                              , runInsert
                              , setEntityName
                              , dbModification
                              , withDbModification
                              , defaultDbSettings
                              , DatabaseSettings
                              , pk, Identity )

getConnection :: IO Connection
getConnection = open "balancePayout.db"

balancePayoutDB :: DatabaseSettings be BalancePayoutDB
balancePayoutDB = defaultDbSettings `withDbModification`
  dbModification { 
    bpdb_users = setEntityName "users"
  , bpdb_balancePayout = setEntityName "balancePayout"
  }

insertUser :: CSVT.User -> IO (UserT Identity)
insertUser CSVT.User{..} = do
  conn <- getConnection
  let user = User (fromIntegral ud'userID) (T.pack . show $ ud'dob) (T.pack . show $ ud'joinDob) ud'joinContribution ud'monthlyContribution (T.pack . show $ ud'retirementDate)
  runBeamSqliteDebug putStrLn conn $ runInsert $
    insert (bpdb_users balancePayoutDB) $ insertValues [user]
  return user

insertBalancePayout :: UserT Identity -> CSVT.BalancePayout -> IO ()
insertBalancePayout user CSVT.BalancePayout{..} = do
  conn <- getConnection
  let balancePayout = BalancePayout (T.pack . show $ bpd'date) bpd'balance bpd'payout (pk user)
  runBeamSqliteDebug putStrLn conn $ runInsert $
    insert (bpdb_balancePayout balancePayoutDB) $ insertValues [balancePayout]