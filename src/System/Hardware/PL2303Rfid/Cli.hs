-- | 

module System.Hardware.PL2303Rfid.Cli where

import           Options.Applicative
import qualified Data.ByteString.Char8 as B

import qualified System.Hardware.PL2303Rfid as Core

data CliArgs
  = CliArgs { cliCommand :: CommandArgs
            , cliDevice :: String
            , cliBeep :: Bool
            -- , color :: Core.LedColor
            }
  deriving (Read, Show, Eq)

data CommandArgs
  = InfoArgs
  | ReadArgs
  | WriteArgs { token     :: B.ByteString
              , lock      :: Core.WriteLock
              -- , writeType :: Core.Command
              }
  deriving (Read, Show, Eq)

writeLockParser :: Parser Core.WriteLock
writeLockParser = getLock <$> lockSwitch
  where
    lockSwitch = switch
                 ( long "lock"
                <> short 'l'
                <> help "Lock after write.")
    getLock = \x -> if x then
                      Core.Lock
                    else
                      Core.DefultLock


writeParser :: Parser CommandArgs
writeParser = WriteArgs
  <$> (B.pack <$> argument str (metavar "TOKEN" <> help "Token in hex fomat. Length 5 byte."))
  <*> writeLockParser

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

cliParser :: Parser CliArgs
cliParser = CliArgs
  <$> commandParser
  <*> strOption
      ( long "device"
     <> short 'd'
     <> metavar "device"
     <> showDefault
     <> value "/dev/ttyUSB0"
     <> help "Path to serial port device." )
  <*> switch
      ( long "beep"
     <> short 'b'
     <> help "Beep when start command." )

greet :: CliArgs -> IO ()
greet x = putStrLn $ show x

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> header "pl2303rfid - cli utils for PL2303 125khz rfid reader/writer" )
