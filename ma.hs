{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.ByteString
import qualified Database.PostgreSQL.Simple

createDb = do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='hsdb' user='postgres' password='123456'"
  execute_ conn "CREATE TABLE auths (login character varying(100), pass character varying(100) )"
  return () 
