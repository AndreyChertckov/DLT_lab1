{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Part1 (part1) where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

newtype Amount = Amount {unAmount :: Float} deriving (Num, Show)

instance FromField Amount where
  fromField f = Amount <$> (fromField f)

instance ToField Amount where
  toField = toField . unAmount


data Account = Account { id_     :: Int
                       , name    :: [Char] 
                       , credits :: Amount
                       } deriving (Show)

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field

instance ToRow Account where
  toRow Account{..} = toRow (id_, name, unAmount credits)

makeSimpleTransaction 
  :: Amount
  -> Account -- ^ From account
  -> Account -- ^ To account
  -> (Account, Account) -- ^ (From account, To account)
makeSimpleTransaction amount acc1 acc2 = (newAcc1, newAcc2)
  where
    newAcc1 = acc1 {credits = credits acc1 - amount}
    newAcc2 = acc2 {credits = credits acc2 + amount}

makeTransactionInDataBase
  :: Connection
  -> Amount
  -> Account
  -> Account
  -> IO ()
makeTransactionInDataBase conn amount (Account id1 _ _) (Account id2 _ _) = do
    (acc1 : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only id1) :: IO [Account]
    (acc2 : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only id2) :: IO [Account]
    let (newAcc1, newAcc2) = makeSimpleTransaction amount acc1 acc2
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits newAcc1, id_ newAcc1) 
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits newAcc2, id_ newAcc2)


-- Create a table of accounts, Each account should have:
-- 1. A unique ID
-- 2. Name
-- 3. Creadit(Rub)
-- Generate and insert 3 accounts into the table, each account has 1000 Rub.
-- Create Transactions:
-- T1. Account 1 send 500 RUB to Account 3
-- T2. Account 2 send 700 RUB to Account 1
-- T3. Account 2 send 100 RUB to Account 3
-- Return the amount Credit for all Account.
part1 :: IO ()
part1 = do
  conn <- open "part1.db"
  let acc1 = Account 1 "Account 1" 1000
  let acc2 = Account 2 "Account 2" 1000
  let acc3 = Account 3 "Account 3" 1000
  execute_ conn "CREATE TABLE IF NOT EXISTS accounts (id INTEGER PRIMARY KEY, name TEXT, credits FLOAT)"
  execute conn "INSERT INTO accounts (id, name, credits) VALUES (?,?,?)" (acc1)
  execute conn "INSERT INTO accounts (id, name, credits) VALUES (?,?,?)" (acc2)
  execute conn "INSERT INTO accounts (id, name, credits) VALUES (?,?,?)" (acc3)
  makeTransactionInDataBase conn 500 acc1 acc3
  makeTransactionInDataBase conn 700 acc2 acc1
  makeTransactionInDataBase conn 100 acc2 acc3
  accounts <- query_ conn "SELECT * FROM accounts" :: IO [Account]
  (putStrLn . show) accounts
