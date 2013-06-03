{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, TupleSections #-}

import Network.IRC.Bot
import Network.IRC (Message)
import qualified Network.IRC as IRC

import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe as L (mapMaybe)

import Data.Time.Clock (UTCTime, getCurrentTime)

import Data.Typeable

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Monoid

import Network.IRC.Bot.Part.Ping
import Network.IRC.Bot.Part.Channels

import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy

type UserName = String

data TimeSlice = TimeSlice
                    { tsJoined :: Set UserName
                    , tsLeft :: Set UserName
                    , tsMessages :: [Message]
                    } deriving (Typeable)

data UserPrefs = UserPrefs { upDoLog :: Bool } deriving (Typeable)

defaultUserPrefs = UserPrefs { upDoLog = False }

data BotState = BotState
                    { bsLog :: Map UTCTime TimeSlice
                    , bsUserPrefs :: Map UserName UserPrefs
                    } deriving (Typeable)

$(deriveSafeCopy 0 'base ''UserPrefs)
$(deriveSafeCopy 0 'base ''IRC.Prefix)
$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''TimeSlice)
$(deriveSafeCopy 0 'base ''BotState)

instance Monoid TimeSlice where
    mappend a b = TimeSlice 
                    { tsJoined = tsJoined a `mappend` tsJoined b
                    , tsLeft = tsLeft a `mappend` tsLeft b
                    , tsMessages = tsMessages a `mappend` tsMessages b
                    }

    mempty = TimeSlice { tsJoined = mempty, tsLeft = mempty, tsMessages = mempty }

getLog :: Query BotState (Map UTCTime TimeSlice)
getLog = ask >>= return . bsLog

getUserPrefs :: UserName -> Query BotState (Maybe UserPrefs)
getUserPrefs user = ask >>= return . M.lookup user . bsUserPrefs

setDoLog :: UserName -> Bool -> Update BotState ()
setDoLog user do_log = modify $ \ (BotState log prefs) -> BotState log $ M.insertWith (\ _ prefs -> prefs { upDoLog = do_log }) user (defaultUserPrefs { upDoLog = do_log }) prefs

logMessage :: UTCTime -> Message -> Update BotState ()
logMessage time message = modify $ \ (BotState log users) -> BotState (M.insertWith mappend time (TimeSlice S.empty S.empty [message]) log) users
                        
logArrival :: UTCTime -> UserName -> Update BotState ()
logArrival time user = modify $ \ (BotState log users) -> BotState (M.insertWith mappend time (TimeSlice (S.singleton user) S.empty []) log) (M.insert user defaultUserPrefs users)

logDeparture :: UTCTime -> UserName -> Update BotState ()
logDeparture time user = modify $ \ (BotState log users) -> BotState (M.insertWith mappend time (TimeSlice S.empty (S.singleton user) []) log) users

forgetUser :: UserName -> Update BotState ()
forgetUser user = modify $ \ (BotState log users) -> BotState log $ M.delete user users

$(makeAcidic ''BotState [ 'getLog, 'getUserPrefs
                        , 'setDoLog , 'logMessage, 'logArrival, 'logDeparture
                        , 'forgetUser
                        ])

greeting :: [String]
greeting =
    [ "Welcome to #snowdrift!"
    , "This bot provides some basic logging, to fill gaps when users are offline."
    , ""
    , "Respond with one of the following commands:"
    , "log"
    , "    enable logging"
    , ""
    , "nolog"
    , "    disable logging"
    ]

logPart :: AcidState BotState -> BotPartT IO ()
logPart database = do
    time <- lift getCurrentTime
    message <- askMessage

    let departing :: BotPartT IO ()
        departing = do
            IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
            logM Normal $ "NOTING THAT USER DEPARTED: " ++ user
            update' database $ LogDeparture time user

        arriving :: BotPartT IO ()
        arriving = do
            IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message

            guard $ user /= "snowbot"

            prefs <- query' database $ GetUserPrefs user
            log <- query' database GetLog

            case prefs of
                Just x | upDoLog x -> do
                    let messages :: [(UTCTime, Message)]
                        messages = M.foldlWithKey (\ l t ts -> L.map (t ,) (tsMessages ts) ++ if (S.member user (tsLeft ts)) then [] else l) [] log

                        renderMessage :: UTCTime -> Message -> Maybe String
                        renderMessage t (IRC.Message (Just (IRC.NickName name _ _)) "PRIVMSG" (sender:msg:_)) = Just $ show t ++ ": <" ++ name ++ "> " ++ msg
                        renderMessage _ _ = Nothing

                        msg_list = case messages of
                            [] -> [ "no missed messages" ]
                            _ -> "you missed the following while away: " : reverse (L.mapMaybe (uncurry renderMessage) messages)

                    forM_ msg_list $ sendMessage . IRC.Message Nothing "PRIVMSG" . (user:) . (:[])

                Nothing -> forM_ greeting $ sendMessage . IRC.Message Nothing "PRIVMSG" . (user:) . (:[])

                _ -> return ()

            logM Normal $ "NOTING THAT USER ARRIVED: " ++ user

            update' database $ LogArrival time user

        messaged :: BotPartT IO ()
        messaged = do
            IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
            let channel : msg : _ = IRC.msg_params message

            case IRC.msg_params message of
                ("#snowdrift" : _) -> update' database $ LogMessage time message
                ("snowbot" : "log" : _) -> update' database $ SetDoLog user True
                ("snowbot" : "nolog" : _) -> update' database $ SetDoLog user False
                ("snowbot" : "forget" : _) -> update' database $ ForgetUser user

    case IRC.msg_command message of
        "PRIVMSG" -> messaged
        "QUIT" -> departing
        "PART" -> departing
        "JOIN" -> arriving
            
        cmd -> logM Normal $ show time ++ ": unrecognized command: " ++ cmd


main = do
    database <- openLocalState (BotState M.empty M.empty)

    let channels = S.singleton "#snowdrift"
        config = nullBotConf
                    { logger = stdoutLogger Debug
                    , host = "irc.freenode.net"
                    , nick = "snowbot"
                    , user = nullUser
                                { username = "snowbot"
                                , hostname = "localhost"
                                , servername = "irc.freenode.net"
                                , realname = "Snowdrift bot"
                                }
                    , channels = channels
                    , limits = Just (10, 100)
                    }

    (channels_tvar, channels_part) <- initChannelsPart channels

    (threads, _) <- simpleBot config [pingPart, logPart database, channels_part]

    forM threads $ \ thread -> do
        putStrLn $ "spawned thread " ++ show thread

    forever $ do
        putStrLn "tick"
        threadDelay $ 60 * 1000 * 1000




