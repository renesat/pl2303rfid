{-|
Module      : System.Hardware.PL2303Rfid.Cli
Copyright   : (c) Smolyakov Ivan, 2021
License     : MIT
Maintainer  : smol.ivan97@gmail.com
Stability   : experimental
-}
module System.Hardware.PL2303Rfid.Cli
  ( -- * Types
    CliArgs(..)
  , CommandArgs(..)
    -- * Parsers
  , cliParser
  , commandParser
  , writeLockParser
  , tokenParser
  , writeTypeParser
  , writeParser
  , deviceParser
  , beepParser
    -- * Main
  , main
  ) where

import           Options.Applicative
import           Options.Applicative.Types (OptProperties(..), Backtracking(..))
import           Options.Applicative.Builder.Internal (optionMod)
import           Options.Applicative.Help.Pretty (text, (<+>))
import           Data.List (elemIndex)
import           System.Hardware.Serialport
import           Control.Monad.Catch (throwM, MonadThrow, Exception)

import qualified System.Hardware.PL2303Rfid.Core as Core

-- An expansion MonadThrow for ReadM

instance MonadThrow ReadM where
  throwM err = readerAbort $ ErrorMsg $ show err

-- Types

-- | Main type for all cli arguments
data CliArgs
  = CliArgs { cliCommand :: CommandArgs
            , cliDevice :: String
            , cliBeep :: Bool
            , cliCommSpeed :: CommSpeed
            }
  deriving (Read, Show, Eq)

-- | Type for command arguments
data CommandArgs
  = InfoArgs
  | ReadArgs
  | WriteArgs { token     :: Core.Token
              , lock      :: Core.WriteLock
              , writeType :: WriteType
              }
  deriving (Read, Show, Eq)

-- | Write type
data WriteType
  = Write
  | Write2
  | Write3
  deriving (Read, Show, Eq)

decodeWriteType :: String -> Either String WriteType
decodeWriteType s = case s of
                      "write"  -> Right Write
                      "write2" -> Right Write2
                      "write3" -> Right Write3
                      _        -> Left "Not corect write type."

encodeWriteType :: WriteType -> String
encodeWriteType wt = case wt of
                       Write  -> "write"
                       Write2 -> "write2"
                       Write3 -> "write3"

-- | Parsers

-- | Write lock parser.
writeLockParser :: Parser Core.WriteLock
writeLockParser = parser desc
  where
    desc = ( long "lock"
          <> short 'l'
          <> help "Lock after write.")
    parser = flag Core.DefultLock Core.Lock

-- | Token parser.
tokenParser :: Parser Core.Token
tokenParser = parser desc
  where
    desc = ( metavar "TOKEN"
          <> help "Token in hex fomat. Length 5 byte (10 symbols).")
    parser = argument tokenReader

tokenReader :: ReadM Core.Token
tokenReader = str >>= Core.fromHex

-- | Write type parser.
writeTypeParser :: Parser WriteType
writeTypeParser = enumOption (map encodeWriteType values)
                             values
      ( long "type"
     <> short 't'
     <> metavar "WRITE_TYPE"
     <> showDefaultWith encodeWriteType
     <> value Write
     <> help "write type."
      )
  where
    values = [Write, Write2, Write3]

-- | Write parser.
writeParser :: Parser CommandArgs
writeParser = WriteArgs
  <$> tokenParser
  <*> writeLockParser
  <*> writeTypeParser

-- | Command parser.
commandParser :: Parser CommandArgs
commandParser = hsubparser
  (  command "info" (info
                     (pure InfoArgs)
                     (progDesc "Get info from device"))
  <> command "read" (info
                     (pure ReadArgs)
                     (progDesc "Read token"))
  <> command "write" (info
                      writeParser
                       ( fullDesc
                      <> progDesc "Write token"))
  )

-- | Device parser
deviceParser :: Parser String
deviceParser = strOption
   ( long "device"
  <> short 'd'
  <> metavar "device"
  <> action "file"
  <> showDefault
  <> value "/dev/ttyUSB0"
  <> help "Path to serial port device." )

-- | Beep parser
beepParser :: Parser Bool
beepParser = switch
   ( long "beep"
  <> short 'b'
  <> help "Beep when start command." )

-- | comm speed parser
commSpeedParser :: Parser CommSpeed
commSpeedParser = enumOption ["110", "300", "600", "1200", "2400",
                              "4800", "9600", "19200", "38400", "57600",
                              "115200"]
                             [CS110, CS300, CS600, CS1200, CS2400,
                              CS4800, CS9600, CS19200, CS38400, CS57600,
                              CS115200]
      ( long "comm-speed"
     <> short 's'
     <> metavar "COMM_SPEED"
     <> showDefaultWith (\_ -> "38400")
     <> value CS38400
     <> help "serial port speed"
      )

-- | Cli args parser.
cliParser :: Parser CliArgs
cliParser = CliArgs
  <$> commandParser
  <*> deviceParser
  <*> beepParser
  <*> commSpeedParser

-- Greeters

greetInfo :: SerialPort -> CommandArgs -> IO ()
greetInfo s _ = do
  infoData <- Core.doInfo s
  putStrLn $ show infoData

greetRead :: SerialPort -> CommandArgs -> IO ()
greetRead s _ = do
  t <- Core.doRead s
  putStrLn $ Core.toHex t

greetWrite :: SerialPort -> CommandArgs -> IO ()
greetWrite s args = do
  let wType = writeType args
  let action = case wType of
                 Write  -> Core.doWrite
                 Write2 -> Core.doWrite2
                 Write3 -> Core.doWrite3
  _ <- action (lock args) (token args) s
  return ()


greet :: CliArgs -> IO ()
greet args = do
  s <- openSerial (cliDevice args) defaultSerialSettings { commSpeed = cliCommSpeed args }
  Core.doLedColor Core.RedColor s

  commandArgs <- return $ cliCommand args
  print args
  case commandArgs of
    InfoArgs        -> greetInfo  s commandArgs
    ReadArgs        -> greetRead  s commandArgs
    WriteArgs _ _ _ -> greetWrite s commandArgs

  Core.doLedColor Core.GreenColor s
  closeSerial s

-- Main

main :: IO ()
main = greet =<< customExecParser defaultPrefs { prefBacktrack = SubparserInline
                                               , prefShowHelpOnEmpty = True}
                                  opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> header "pl2303rfid - cli utils for PL2303 125khz rfid reader/writer" )

-- Helpers

enumReader :: [String] -> [a] -> String -> ReadM a
enumReader items values item
  = case mIndex of
      Just index -> return $ values !! index
      Nothing    -> readError
  where
    mIndex = elemIndex item items
    readError = readerAbort $ ErrorMsg  ( "available values: "
                                       ++ foldl1 (\s x -> s ++ ", " ++ x) items )

enumOption :: [String] -> [a] -> Mod OptionFields a -> Parser a
enumOption items values desc = option (str >>= enumReader items values)
                                      ( desc
                                     <> availableParamMod items
                                     <> completeWith items)

availableParamMod :: [String] -> Mod f a
availableParamMod items = optionMod (\x -> x { propHelp = propHelp x >>= (\t -> return $ t <+> text l)
                                             })
  where
    l = ( "(available values: "
       ++ foldl1 (\s x -> s ++ ", " ++ x) items
       ++ ")")
