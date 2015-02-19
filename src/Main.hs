
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad (forever, void)
import           Control.Concurrent (threadDelay)
import           Control.Exception (SomeException, catch)

import           Text.Printf
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Configurator.Types (Config)
import qualified Data.Configurator as Config
import qualified Database.PostgreSQL.Simple as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Data.Pool (Pool, createPool, withResource)
import           System.Posix.Syslog
                  (withSyslog, syslog
                  ,Option(..), Facility(USER), Priority(Info,Debug,Error))
import qualified System.Environment as Env

import Network.Curl
import Network.HTTP.Base (urlEncode)


main :: IO ()
main = withCurlDo $ do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    "one-shot":args -> do
      let [user, pass, from, to, text] = map T.pack args
      withSyslog prog [PID,PERROR] USER
        $ smsdirect user pass from to text >>= print

    [configPath] -> do
      withSyslog prog [PID] USER $ do
        syslog Info $ "Loading config from " ++ configPath
        conf <- Config.load [Config.Required configPath]

        Just host <- Config.lookup conf "pg.host"
        Just port <- Config.lookup conf "pg.port"
        Just user <- Config.lookup conf "pg.user"
        Just pwd  <- Config.lookup conf "pg.pass"
        Just db   <- Config.lookup conf "pg.db"

        syslog Info $ "Connecting to Postgres on " ++ host
        let cInfo = Pg.ConnectInfo host port user pwd db
        pgPool <- createPool (Pg.connect cInfo) Pg.close
            1 -- number of distinct sub-pools
            5 -- time for which an unused resource is kept open
            7 -- maximum number of resources to keep open

        syslog Info "Starting loop"
        loop conf pgPool

    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


smsdirect :: Text -> Text -> Text -> Text -> Text -> IO (CurlCode, String)
smsdirect login pass from to text
  = syslog Debug ("SMSDirect query: " ++ url)
  >> curlGetString url []
  where
    url = printf
      "https://%s/submit_message?login=%s&pass=%s&from=%s&to=%s&text=%s"
      ("www.smsdirect.ru" :: String)
      (T.unpack login) (T.unpack pass)
      (T.unpack from) (T.unpack $ T.dropWhile (== '+') to)
      (urlEncode $ T.unpack text)


loop :: Config -> Pool Pg.Connection -> IO ()
loop conf pgPool = forever (catchAll go >> threadDelay (10^(6 :: Int)))
  where
    go = getJob pgPool >>= \case
      [] -> return ()
      [msg@(msgId,_,_,_)] -> do
        syslog Info $ "Got job: " ++ show (msgId::Int)
        res <- sendSMS conf msg `catch` \e ->
                return (Left $ show (e :: SomeException))
        case res of
          Left err -> do
            syslog Error err
            updateJob pgPool msgId Nothing "error"
          Right fMsgId -> do
            syslog Info $ "SMSDirect message id: " ++ show fMsgId
            updateJob pgPool msgId (Just fMsgId) "sent"
      res -> error $ "BUG: " ++ show res


sendSMS :: Config -> (Int, Text, Text, Text) -> IO (Either String Text)
sendSMS conf (_, to, from, text) = do
  Just user <- Config.lookup conf "smsdirect.user"
  Just pass <- Config.lookup conf "smsdirect.pass"
  smsdirect user pass from to text >>= \case
    (CurlOK, "")    -> return $ Left  $ "SMSDirect returned no message id"
    (CurlOK, msgId) -> return $ Right $ T.pack msgId
    err             -> return $ Left $ "SMSDirect ErrorCode: " ++ show err


getJob :: Pool Pg.Connection -> IO [(Int,Text,Text,Text)]
getJob pgPool
  = withResource pgPool $ \c -> Pg.query_ c
    [sql|
      update "Sms"
        set status = 'processing',
            mtime  = statement_timestamp()
        where id in
          (select id from "Sms"
            where status = 'please-send'
              and ctime > statement_timestamp() - interval '20 minutes'
            limit 1)
        returning id, phone, sender, msgText
    |]


updateJob :: Pool Pg.Connection -> Int -> Maybe Text -> Text -> IO ()
updateJob pgPool ident fIdent st
  = void $ withResource pgPool $ \c -> Pg.execute c
    [sql|
      update "Sms"
        set status    = ?,
            mtime     = statement_timestamp(),
            foreignId = ?
        where id = ?
    |]
    (st, fIdent, ident)


catchAll :: IO () -> IO ()
catchAll f = f `catch` \e -> syslog Error $ show (e :: SomeException)
