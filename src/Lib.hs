{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( bot
  ) where

import           Control.Concurrent           (threadDelay)
import           Control.Monad                (forM_, when)
import           Data.Char                    (toLower)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import           Language.Haskell.Interpreter
import           System.Environment           (getEnv)

import           Discord
import qualified Discord.Requests             as R
import           Discord.Types

prefix = "*"

bot :: IO ()
bot = do
  tok <- TIO.readFile "./src/auth-token.secret"
  t <-
    runDiscord $
    def
      { discordToken = tok
      , discordOnStart = startHandler
      , discordOnEnd = putStrLn "\nBye"
      , discordOnEvent = eventHandler
      , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
      }
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandle -> IO ()
startHandler dis = TIO.putStrLn "Starting..."

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event =
  case event of
    MessageCreate m ->
      when (not (fromBot m) && startsWithPrefix m) $
      case firstWord $ messageText m of
        "eval" -> doEval dis m
        "e"    -> doEval dis m
        "type" -> doTypeOf dis m
        "t"    -> doTypeOf dis m
        "ping" -> doPing dis m
        _      -> pure ()
    _ -> pure ()

-- commands
doEval :: DiscordHandle -> Message -> IO ()
doEval dis m = doInInterpreter dis m eval "evaluate"

doTypeOf :: DiscordHandle -> Message -> IO ()
doTypeOf dis m = doInInterpreter dis m typeOf "get type"

doInInterpreter :: DiscordHandle -> Message -> (String -> InterpreterT IO String) -> T.Text -> IO ()
doInInterpreter dis m func task = do
  t <-
    runInterpreter $ do
      setImports ["Prelude"]
      func (T.unpack expr)
  _ <- handleInterpreterOutput dis m task t
  pure ()
  where
    expr = T.intercalate " " (arguments (messageText m))

doPing :: DiscordHandle -> Message -> IO ()
doPing dis m = do
  _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "exclamation")
  _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
  pure ()

-- output handling
handleInterpreterOutput :: DiscordHandle -> Message -> T.Text -> Either InterpreterError String -> IO ()
handleInterpreterOutput dis m task res =
  case res of
    Right res -> do
      _ <- restCall dis (R.CreateMessage (messageChannel m) resMsg)
      pure ()
      where resMsg = T.intercalate "\n" ["```hs", T.pack res, "```"]
    Left err -> do
      _ <- restCall dis (R.CreateMessage (messageChannel m) errMsg)
      pure ()
      where errMsg = T.intercalate "" ["Could not ", task, ": ", T.pack (show err)]

-- helpers
startsWithPrefix :: Message -> Bool
startsWithPrefix = (prefix `T.isPrefixOf`) . T.map toLower . messageText

firstWord :: T.Text -> T.Text
firstWord t = T.drop (T.length prefix) (head $ T.splitOn " " t)

arguments :: T.Text -> [T.Text]
arguments t = drop 1 (T.splitOn " " t)

--startsWithCommand :: Message -> T.Text -> Bool
--startsWithCommand m cmd = (startsWithPrefix m) && ((firstWord $ messageText m) == cmd)
--
--startsWithAnyCommand :: Message -> [T.Text] -> Bool
--startsWithAnyCommand m []     = False
--startsWithAnyCommand m (c:cs) | startsWithCommand m c = True
--                              | otherwise             = startsWithAnyCommand m cs
fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)
