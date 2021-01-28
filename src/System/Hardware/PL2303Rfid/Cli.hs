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


writeParser :: Parser CommandArgs
writeParser = WriteArgs
  <$> (B.pack <$> argument str (metavar "TOKEN" <> help "Retain all intermediate temporary files"))
  <*> ((\x -> if x then Core.Lock else Core.DefultLock) <$> switch -- TODO: rewrite
      ( long "lock"
     <> short 'l'
     <> help "Lock after write."))

commandParser :: Parser CommandArgs
commandParser = subparser
  (  command "info" (info
                     ((pure InfoArgs) <**> helper)
                     (progDesc "Get info from device"))
  <> command "read" (info
                     ((pure ReadArgs) <**> helper)
                     (progDesc "Read token"))
  <> command "write" (info
                      (writeParser <**> helper)
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
