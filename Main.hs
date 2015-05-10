{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Applicative        (Applicative)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Reader       (asks)
import           Control.Monad.Logger       (runStdoutLoggingT)
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

import           Data.Pool ()
import           Database.Persist.Postgresql as DB

import qualified Web.Scotty()
import           Web.Scotty.Trans as S
import           Network.Wai.Middleware.RequestLogger(logStdoutDev)

import Model

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
    let cfg = Config { getPool = pool }
    let r m = runReaderT (runConfigM m) cfg
    scottyT 3000 r r app

app :: ScottyT T.Text ConfigM ()
app = do
    runDb doMigrations
    runDb doDbStuff
    middleware logStdoutDev
    S.get "/" (html "hello world")
    S.get "/posts" postsIndex

postsIndex :: ActionT T.Text ConfigM ()
postsIndex = do
    posts <- runDb (selectList [] [])
    html $ "This many posts!<br>" <> T.pack (show (length (posts :: [Entity BlogPost])))

runDb :: (MonadTrans t, MonadIO (t ConfigM)) => SqlPersistT IO a -> t ConfigM a
runDb query = do
    pool <- lift $ asks getPool
    liftIO (runSqlPool query pool)

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

doDbStuff :: ReaderT SqlBackend IO ()
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
