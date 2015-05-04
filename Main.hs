{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

import           Database.Persist
import           Database.Persist.Postgresql as DB
import           Database.Persist.TH

import Web.Scotty as S
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

connStr :: ConnectionString
connStr = "host=localhost dbname=perscotty user=test password=test port=5433"

main :: IO ()
main = do
    pool <- runStderrLoggingT $ createPostgresqlPool connStr 10
    runDb doMigrations pool
    runDb doDbStuff pool
    scotty 3000 $ do
        middleware logStdoutDev
        S.get "/" $ S.html "Hello World"
        S.get "/posts" $ do
            posts <- runDb (selectList [] []) pool
            html ("Posts!" <> T.pack (show $ length (posts :: [Entity BlogPost])))
        S.get "/posts/:id" $ do
            postId <- S.param "id"
            findPost <- runDb (DB.get (toSqlKey (read postId))) pool
            html $ "You requested post: <br>" <> T.pack (show (findPost :: Maybe BlogPost))

runDb query pool = liftIO (runSqlPool query pool)

doMigrations = runMigration migrateAll

doDbStuff = do
        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        _ <- insert $ BlogPost "My fr1st p0st" johnId
        _ <- insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- DB.get johnId
        liftIO $ print (john :: Maybe Person)

        -- delete janeId
        -- deleteWhere [BlogPostAuthorId ==. johnId]
