{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Control.Applicative
import Control.Monad
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as Pg

data Client = Client { firstName :: Text
                     , lastName :: Text
                     , clientLocation :: Location
                     }

data Location = Location { address :: Text
                         , location :: Text
                         }

instance Pg.FromRow Location where
  fromRow = Location <$> field <*> field

instance Pg.FromRow Client where
  fromRow = Client <$> field <*> field <*> liftM2 Location field field

queryClients :: Connection -> IO [Client]
queryClients c = query_ c "SELECT firstname, lastname, location FROM clients"

main :: IO [Client]
main = do
  uri <- B.getLine
  conn <- connectPostgreSQL uri -- TODO see uri format in function doc postgres://postgres:123456@localhost/hsdb
  queryClients conn
