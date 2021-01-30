-- | 

module System.Hardware.PL2303Rfid.Cli where

import           Options.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.List (elemIndex)
import           Options.Applicative.Types (OptProperties(..))
import           Options.Applicative.Builder.Internal (optionMod)
import           Options.Applicative.Help.Pretty (text, (<+>))

import qualified System.Hardware.PL2303Rfid as Core

data CliArgs
  = CliArgs { cliCommand :: CommandArgs
            , cliDevice :: String
            , cliBeep :: Bool
            }
  deriving (Read, Show, Eq)

data CommandArgs
  = InfoArgs
  | ReadArgs
  | WriteArgs { token     :: B.ByteString
              , lock      :: Core.WriteLock
              , writeType :: Core.Command
              }
  deriving (Read, Show, Eq)

-- | Write lock parser.
writeLockParser :: Parser Core.WriteLock
writeLockParser = parser desc
  where
    desc = ( long "lock"
          <> short 'l'
          <> help "Lock after write.")
    parser = flag Core.DefultLock Core.Lock

-- | Token parser.
tokenParser :: Parser B.ByteString
tokenParser = B.pack <$> parser desc
  where
    desc = ( metavar "TOKEN"
          <> help "Token in hex fomat. Length 5 byte.")
    parser = argument str

writeTypeParser :: Parser Core.Command
writeTypeParser = enumOption ["write2"    , "write3"   ]
                             [Core.Write2 , Core.Write3]
      ( long "type"
     <> short 't'
     <> metavar "WRITE_TYPE"
     <> showDefaultWith (\_ -> "write2")
     <> value Core.Write2
     <> help "write type."
      )


writeParser :: Parser CommandArgs
writeParser = WriteArgs
  <$> tokenParser
  <*> writeLockParser
  <*> writeTypeParser

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
                                      (desc <> availableParamMod items)

availableParamMod :: [String] -> Mod f a
availableParamMod items = optionMod (\x -> x { propHelp = propHelp x >>= (\t -> return $ t <+> text l)
                                             })
  where
    l = ( "(available values: "
       ++ foldl1 (\s x -> s ++ ", " ++ x) items
       ++ ")")
