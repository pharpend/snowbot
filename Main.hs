{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- snowbot - bot for the #snowdrift channel on FreeNode
-- Copyright (c) 2013-2015, Snowdrift.coop
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Affero General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see
-- <http://www.gnu.org/licenses/agpl>.

-- | 
-- Module      : Main
-- Description : Runs snowbot
-- Copyright   : Copyright (c) 2013-2015, Snowdrift.coop
-- License     : AGPL-3
-- Maintainer  : dev@lists.snowdrift.coop
-- Stability   : experimental
-- Portability : POSIX

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.Acid
import Data.Acid.Advanced
import Data.ByteString.Char8 as BSC
import Data.List as L
import Data.Map as M
import Data.Maybe as L (mapMaybe, maybeToList)
import Data.Monoid
import Data.SafeCopy
import Data.Set as S
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable
import Network.IRC (Message)
import Network.IRC.Bot
import Network.IRC.Bot.Part.Channels
import Network.IRC.Bot.Part.Ping
import qualified Network.IRC as IRC

main :: IO ()
main =
  do database <- openLocalState $ BotState mempty mempty mempty
     let channels = S.singleton botChannel
         config =
           nullBotConf {logger = stdoutLogger Debug
                       ,host = "irc.freenode.net"
                       ,nick = nickName
                       ,user = nullUser {username = nickName
                                        ,hostname = "localhost"
                                        ,servername = "irc.freenode.net"
                                        ,realname = realName}
                       ,channels = channels
                       ,limits = Just (10,100)}

     (channels_tvar,channels_part) <- initChannelsPart channels
     (threads,_) <- simpleBot config [pingPart,logPart database,channels_part]
     forever $ do threadDelay $ 60 * 1000 * 1000

-------- Settings --------
nickName :: ByteString
nickName = "snowbot"

realName :: ByteString
realName = "Snowdrift bot - development"

botChannel :: ByteString
botChannel = "#snowdrift"
--------------------------

type UserName = ByteString

data TimeSlice =
  TimeSlice {tsJoined :: Set UserName
            ,tsLeft :: Set UserName
            ,tsMessages :: [Message]}
  deriving (Typeable)

data UserPrefs =
  UserPrefs {upDoLog :: Bool}
  deriving (Show,Typeable)

defaultUserPrefs :: UserPrefs
defaultUserPrefs = UserPrefs {upDoLog = False}

data BotState =
  BotState {bsLog :: Map UTCTime TimeSlice
           ,bsUserPrefs :: Map UserName UserPrefs
           ,bsMemos :: Map UserName [Memo]}
  deriving (Typeable)

data Memo =
  Memo {memoTime :: UTCTime
       ,memoSender :: UserName
       ,memoContent :: ByteString}
  deriving (Show,Typeable)

$(deriveSafeCopy 0 'base ''UserPrefs)
$(deriveSafeCopy 0 'base ''IRC.Prefix)
$(deriveSafeCopy 0 'base ''Message)
$(deriveSafeCopy 0 'base ''TimeSlice)
$(deriveSafeCopy 0 'base ''BotState)
$(deriveSafeCopy 0 'base ''Memo)

instance Monoid TimeSlice where
  mappend a b =
    TimeSlice {tsJoined = tsJoined a `mappend` tsJoined b
              ,tsLeft = tsLeft a `mappend` tsLeft b
              ,tsMessages = tsMessages a `mappend` tsMessages b}
  mempty =
    TimeSlice {tsJoined = mempty
              ,tsLeft = mempty
              ,tsMessages = mempty}

getLog :: Query BotState (Map UTCTime TimeSlice)
getLog = ask >>= return . bsLog

getUserPrefs :: UserName -> Query BotState (Maybe UserPrefs)
getUserPrefs user = ask >>= return . M.lookup user . bsUserPrefs

getAllUserPrefs :: Query BotState (Map UserName UserPrefs)
getAllUserPrefs = ask >>= return . bsUserPrefs

getMemos :: UserName -> Query BotState [Memo]
getMemos user = ask >>= return . join . L.maybeToList . M.lookup user . bsMemos

setDoLog :: UserName -> Bool -> Update BotState ()
setDoLog user do_log =
  modify $
  \(BotState log prefs memos) ->
    BotState log
             (M.insertWith
                (\_ prefs ->
                   prefs {upDoLog = do_log})
                user
                (defaultUserPrefs {upDoLog = do_log})
                prefs)
             memos

logMessage :: UTCTime -> Message -> Update BotState ()
logMessage time message =
  modify $
  \(BotState log users memos) ->
    BotState (M.insertWith mappend
                           time
                           (TimeSlice S.empty S.empty [message])
                           log)
             users
             memos

logArrival :: UTCTime -> UserName -> Update BotState ()
logArrival time user =
  modify $
  \(BotState log users memos) ->
    BotState (M.insertWith mappend
                           time
                           (TimeSlice (S.singleton user) S.empty [])
                           log)
             (M.insertWith (flip const)
                           user
                           defaultUserPrefs
                           users)
             memos

logDeparture :: UTCTime -> UserName -> Update BotState ()
logDeparture time user =
  modify $
  \(BotState log users memos) ->
    BotState (M.insertWith mappend
                           time
                           (TimeSlice S.empty (S.singleton user) [])
                           log)
             users
             memos

recordMemo :: UserName -> UserName -> UTCTime -> ByteString -> Update BotState ()
recordMemo sender recipient time content =
  modify $
  \(BotState log users memos) ->
    let appendMemo :: Memo -> Maybe [Memo] -> Maybe [Memo]
        appendMemo m (Just ms) = Just $ m : ms
        appendMemo m Nothing = Just [m]
        memo = Memo time sender content
        memos' =
          M.insertWith mappend
                       recipient
                       [memo]
                       memos
    in BotState log users memos'

clearMemos :: UserName -> Update BotState ()
clearMemos user =
  modify $
  \(BotState l u _) -> BotState l u M.empty

forgetUser :: UserName -> Update BotState ()
forgetUser user =
  modify $
  \(BotState log users memos) ->
    BotState log (M.delete user users) memos

$(makeAcidic
    ''BotState
    ['getLog
    ,'getAllUserPrefs
    ,'getUserPrefs
    ,'setDoLog
    ,'logMessage
    ,'logArrival
    ,'logDeparture
    ,'forgetUser
    ,'getMemos
    ,'recordMemo
    ,'clearMemos])

greeting :: [ByteString]
greeting =
  [BSC.replicate 60 '*'
  ,"* Welcome to " <> botChannel <> "!"
  ,"* This bot provides some basic logging, to fill gaps when users are offline."
  ,"*"
  ,"* If you would like notice of comments you missed, reply with:"
  ,"* log"
  ,"*     to enable logging"
  ,"*"
  ,"* nolog"
  ,"*     will later disable logging"
  ,"*"
  ,BSC.replicate 60 '*']

memoToMessageFor :: UserName -> Memo -> Message
memoToMessageFor recipient (Memo time sender content) =
  IRC.privmsg recipient text
  where time' = BSC.pack (show time)
        sender' = "<" <> sender <> "> "
        text = time' <> ": " <> sender' <> content

notice :: UserName -> ByteString -> Message
notice nick msg =
  IRC.Message Nothing
              "NOTICE"
              [nick,msg]

logPart :: AcidState BotState -> BotPartT IO ()
logPart database =
  do time <- lift getCurrentTime
     message <- askMessage
     let sendMemos :: UserName -> BotPartT IO ()
         sendMemos user =
           do memos <-
                query' database $
                GetMemos user
              sendMessage $
                case memos of
                  [] -> notice user "no memos"
                  _ ->
                    IRC.privmsg user "some memos were left for you:"
              forM_ (L.reverse memos)
                    (sendMessage . memoToMessageFor user)
              update' database $
                ClearMemos user
         departing :: BotPartT IO ()
         departing =
           do IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
              logM Normal $
                "NOTING THAT USER DEPARTED: " <> user
              update' database $
                LogDeparture time user
         arriving :: BotPartT IO ()
         arriving =
           do IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
              guard $ user /= nickName
              guard $ user /= nickName <> "-devel"
              prefs <-
                query' database $
                GetUserPrefs user
              log <- query' database GetLog
              sendMemos user
              case prefs of
                Just x
                  | upDoLog x ->
                    do let messages :: [(UTCTime,Message)]
                           messages =
                             M.foldlWithKey
                               (\l t ts ->
                                  L.map (t,)
                                        (tsMessages ts) ++
                                  if (S.member user (tsLeft ts))
                                     then []
                                     else l)
                               []
                               log
                           renderMessage :: UTCTime
                                         -> Message
                                         -> Maybe ByteString
                           renderMessage t (IRC.Message (Just (IRC.NickName name _ _)) "PRIVMSG" (sender:msg:_)) =
                             Just $
                             BSC.pack (show t) <>
                             ": <" <>
                             name <>
                             "> " <>
                             msg
                           renderMessage _ _ = Nothing
                       case messages of
                         [] ->
                           sendMessage $
                           notice user "no missed messages"
                         _ ->
                           let msg_list =
                                 "you missed the following while away: " :
                                 L.reverse (L.mapMaybe (uncurry renderMessage)
                                                       messages)
                           in forM_ msg_list $ sendMessage . IRC.privmsg user
                Nothing -> forM_ greeting $ sendMessage . IRC.privmsg user
                _ -> return ()
              logM Normal $
                "NOTING THAT USER ARRIVED: " <> user
              update' database $
                LogArrival time user
         messaged :: BotPartT IO ()
         messaged =
           do IRC.NickName user _ _ <- maybeZero $ IRC.msg_prefix message
              let channel:msg:_ = IRC.msg_params message
              when (channel == botChannel) $
                update' database (LogMessage time message)
              let highlights :: [ByteString] -> ByteString -> Bool
                  highlights (x:_) nick =
                    L.any (== x) validHighlights
                    where suffixes = ["",":",","]
                          validHighlights =
                            L.map (nick <>) suffixes
                  highlights [] _ = False
                  msg' = BSC.split ' ' msg
                  isPrivateMessage = channel == nickName
                  isCommand =
                    (channel == botChannel && msg' `highlights` nickName) ||
                    isPrivateMessage
              guard isCommand
              let command =
                    if isPrivateMessage
                       then msg'
                       else L.tail msg'
              case command of
                ["log"] ->
                  do update' database $
                       SetDoLog user True
                     if isPrivateMessage
                        then sendMessage $
                             IRC.privmsg user
                                         "when you log in, you'll be sent messages since you last left"
                        else sendMessage $
                             IRC.privmsg
                               channel
                               (user <>
                                ", when you log in, you'll be sent messages since you last left")
                ["nolog"] ->
                  do update' database $
                       SetDoLog user False
                     if isPrivateMessage
                        then sendMessage $
                             IRC.privmsg user "you will no longer be sent messages on your return"
                        else sendMessage $
                             IRC.privmsg
                               channel
                               (user <>
                                ", you will no longer be sent messages on your return")
                ["forget"] ->
                  update' database $
                  ForgetUser user
                ["memos"] -> do sendMemos user
                ("memo":recipient:content)
                  | not $ L.null content ->
                    do let content' =
                             BSC.concat (L.intersperse " " content)
                       logM Normal $
                         "Recorded memo from " <> user <> " for " <> recipient <>
                         " with content \"" <> content' <> "\""
                       update' database $
                         RecordMemo user recipient time content'
                       if isPrivateMessage
                          then sendMessage $
                               IRC.privmsg user "Memo recorded."
                          else sendMessage $
                               IRC.privmsg channel
                                           (user <> ", memo recorded.")
                -- unknown command
                (n:_)
                  | isPrivateMessage ->
                    sendMessage $
                    IRC.privmsg user "what?"
                  | otherwise ->
                    sendMessage $
                    IRC.privmsg channel
                                (user <> ", what?")
                -- empty command (happens when you just highlight the bot in the channel)
                _
                  | isPrivateMessage ->
                    sendMessage $
                    IRC.privmsg user "what?"
                  | otherwise ->
                    sendMessage $
                    IRC.privmsg channel
                                (user <> ", what?")
     case IRC.msg_command message of
       "PRIVMSG" -> messaged
       "QUIT" -> departing
       "PART" -> departing
       "JOIN" -> arriving
       cmd ->
         logM Normal $
         BSC.pack (show time) <>
         ": unrecognized command: " <>
         cmd
