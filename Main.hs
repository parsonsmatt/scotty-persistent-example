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
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
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

newtype ConfigM a = ConfigM
    { runConfigM :: ReaderT Config IO a
    } deriving (Applicative, Functor, Monad, 
                MonadIO, MonadReader Config)

type Error = T.Text

connStr :: ConnectionString
connStr = "host=localhost dbname=perscotty user=test password=test port=5432"

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
    let cfg = Config pool
    let r m = runReaderT (runConfigM m) cfg
    runDb pool doMigrations 
    runDb pool doDbStuff 
    scottyT 3000 id id $ do
        middleware logStdoutDev
        S.get "/" $ S.html "Hello World"
        S.get "/posts" $ do
            posts <- runDb pool (selectList [] [])
            html ("Posts!" <> T.pack (show $ length (posts :: [Entity BlogPost])))
        S.get "/posts/:id" $ do
            postId <- S.param "id"
            findPost <- runDb pool (DB.get (toSqlKey (read postId)))
            html $ "You requested post: <br>" <> T.pack (show (findPost :: Maybe BlogPost))

runDb pool query = liftIO (runSqlPool query pool)

doMigrations = runMigration migrateAll

doDbStuff = do
        johnId <- insert $ Person "John Doe" $ Just 35
        _ <- insert $ Person "Jane Doe" Nothing

        _ <- insert $ BlogPost "My fr1st p0st" johnId
        _ <- insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- DB.get johnId
        liftIO $ print (john :: Maybe Person)

        -- delete janeId
        -- deleteWhere [BlogPostAuthorId ==. johnId]
