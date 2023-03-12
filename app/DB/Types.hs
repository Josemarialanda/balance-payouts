module DB.Types ( UserT (..)
                , BalancePayoutT (..)
                , BalancePayoutDB (..) ) where

import Data.Int      ( Int32 )
import Data.Text     ( Text )
import Database.Beam ( Generic
                     , C
                     , Columnar
                     , Identity
                     , Table(..)
                     , Beamable
                     , TableEntity
                     , Database )

data BalancePayoutDB f = BalancePayoutDB { 
    bpdb_users         :: f (TableEntity UserT)
  , bpdb_balancePayout :: f (TableEntity BalancePayoutT) }
  deriving (Generic, Database be)

type User = UserT Identity

data UserT f = User { 
    ut_userID               :: C f Int32
  , ut_dob                  :: C f Text
  , ut_joinDob              :: C f Text
  , ut_joinContribution     :: C f Double
  , ut_monthlyContribution  :: C f Double
  , ut_retirementDate       :: C f Text }
  deriving (Generic, Beamable)

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey :: UserT column -> PrimaryKey UserT column
   primaryKey = UserId . ut_userID

deriving instance Show User
deriving instance Eq User

data BalancePayoutT f = BalancePayout { 
    bpt_date    :: C f Text
  , bpt_balance :: C f Double
  , bpt_payout  :: C f Double
  , bpt         :: PrimaryKey UserT f }
  deriving (Generic, Beamable)

type BalancePayout = BalancePayoutT Identity

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show BalancePayout

instance Table BalancePayoutT where
    data PrimaryKey BalancePayoutT f = BalancePayoutID (PrimaryKey UserT f) deriving (Generic, Beamable)
    primaryKey :: BalancePayoutT column -> PrimaryKey BalancePayoutT column
    primaryKey = BalancePayoutID . bpt