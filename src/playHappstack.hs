-- pragma CPP - language extension, legally encorporate C pre-processor syntax into your Haskell program

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Main where

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Happstack.Server     ( Response, ServerPart, dir
                            , nullDir, nullConf, ok
                            , simpleHTTP, toResponse )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar

data CounterState = CounterState { count :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Blogger = Blogger {
  name :: String,
  papers :: Integer,
  job :: Bool,
  birthDay :: UTCTime,
  alias :: Maybe String
  }
             deriving (Eq, Ord, Read, Show, Data, Typeable)

-- in Happstack Template Haskell (like macros) used to derive instances of classes like SafeCopy and IsAcidic (similar to typeclasses)
$(deriveSafeCopy 0 'base ''CounterState)

$(deriveSafeCopy 0 'base ''Blogger)

initialCounterState :: CounterState
initialCounterState = CounterState 0

blogger1 :: Blogger
blogger1 = Blogger "Fred" 24 True (UTCTime (fromGregorian 1971 03 26) (fromIntegral $ 12 * 3600)) (Just "Lee")
blogger2 :: Blogger
blogger2 = Blogger "Bob" 56 False (UTCTime (fromGregorian 1987 12 16) (fromIntegral $ 12 * 3600)) Nothing
bloggerList :: [Blogger]
bloggerList = [blogger1, blogger2]

--incCountBy1 :: Integer -> CounterState -> (CounterState, Integer)
--incCountBy1

incCountBy :: Integer -> Update CounterState Integer
incCountBy n =
    do c@CounterState{..} <- get
       let newCount = count + n
       put $ c { count = newCount }
       return newCount

main :: IO ()
main = print "Hello!!!"
