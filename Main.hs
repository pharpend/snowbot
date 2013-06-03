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

data BotState = BotState
                    { bsLog :: Map UTCTime TimeSlice
                    , bsKnownUsers :: Set UserName
                    } deriving (Typeable)

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

getKnownUsers :: Query BotState (Set UserName)
getKnownUsers = ask >>= return . bsKnownUsers

logMessage :: UTCTime -> Message -> Update BotState ()
logMessage time message = modify $ \ (BotState log users) -> BotState (M.insertWith mappend time (TimeSlice S.empty S.empty [message]) log) users
                        
logArrival :: UTCTime -> UserName -> Update BotState ()
logArrival time user = modify $ \ (BotState log users) -> BotState (M.insertWith mappend time (TimeSlice (S.singleton user) S.empty []) log) (S.insert user users)

logDeparture :: UTCTime -> UserName -> Update BotState ()
logDeparture time user = modify $ \ (BotState log users) -> BotState (M.insertWith mappend time (TimeSlice S.empty (S.singleton user) []) log) users

$(makeAcidic ''BotState [ 'getLog, 'getKnownUsers
                        , 'logMessage, 'logArrival, 'logDeparture
                        ])

greeting :: String
greeting = "This is a test welcome message.  Welcome!"

logPart :: AcidState BotState -> BotPartT IO ()
logPart database = do
    time <- lift getCurrentTime
    message <- askMessage

    let departing :: BotPartT IO ()
        departing = do
            IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
            update' database $ LogDeparture time user

        arriving :: BotPartT IO ()
        arriving = do
            IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message

            guard $ user /= "snowbot"

            known_users <- query' database GetKnownUsers
            log <- query' database GetLog

            if (S.member user known_users) 
                then let messages :: [(UTCTime, Message)]
                         messages = M.foldlWithKey (\ l t ts -> L.map (t ,) (tsMessages ts) ++ if (S.member user (tsLeft ts)) then [] else l) [] log

                         renderMessage :: UTCTime -> Message -> Maybe String
                         renderMessage t (IRC.Message (Just (IRC.NickName name _ _)) "PRIVMSG" (sender:msg:_)) = Just $ show t ++ ": <" ++ name ++ "> " ++ msg
                         renderMessage _ _ = Nothing

                         msg_list = case messages of
                             [] -> [ "no missed messages" ]
                             _ -> "you missed the following while away: " : reverse (L.mapMaybe (uncurry renderMessage) messages)

                      in do
                        mapM_ (sendMessage . IRC.Message Nothing "PRIVMSG" . (user:) . (:[])) msg_list

                else sendMessage $ IRC.Message Nothing "PRIVMSG" [user, greeting]


            update' database $ LogArrival time user

        messaged :: BotPartT IO ()
        messaged = do
            IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
            let channel : msg : _ = IRC.msg_params message

            when (channel == "#snowdrift") $ update' database $ LogMessage time message

    case IRC.msg_command message of
        "PRIVMSG" -> messaged
        "QUIT" -> departing
        "PART" -> departing
        "JOIN" -> arriving
            
        cmd -> logM Normal $ show time ++ ": unrecognized command: " ++ cmd


main = do
    database <- openLocalState (BotState M.empty S.empty)

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




