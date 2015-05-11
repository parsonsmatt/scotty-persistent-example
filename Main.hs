{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Applicative        (Applicative)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Logger       (runStdoutLoggingT)
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

import           Data.Pool (Pool)
import           Database.Persist
import           Database.Persist.Postgresql as DB
import           Database.Persist.TH

import qualified Web.Scotty
import           Web.Scotty.Trans as S
import           Network.Wai.Middleware.RequestLogger(logStdoutDev)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

data Config = Config { getPool :: ConnectionPool }

newtype ConfigM b = ConfigM
    { runConfigM :: ReaderT Config IO b
    } deriving (Applicative, Functor, Monad, 
                MonadIO, MonadReader Config)

connStr :: ConnectionString
connStr = "host=localhost dbname=perscotty user=test password=test port=5432"

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
    let cfg = Config pool
        r m = runReaderT (runConfigM m) cfg
    scottyT 3000 r r app

app :: ScottyT T.Text ConfigM ()
app = S.get "/" $ S.html "Hello world"
