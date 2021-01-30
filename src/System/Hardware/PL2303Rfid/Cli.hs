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
    -- * Main
  , main
  ) where

import           Options.Applicative
import qualified Data.ByteString.Char8 as B
import           Data.List (elemIndex)
import           Options.Applicative.Types (OptProperties(..))
import           Options.Applicative.Builder.Internal (optionMod)
import           Options.Applicative.Help.Pretty (text, (<+>))

import qualified System.Hardware.PL2303Rfid.Core as Core

-- Types

-- | Main type for all cli arguments
data CliArgs
  = CliArgs { cliCommand :: CommandArgs
            , cliDevice :: String
            , cliBeep :: Bool
            }
  deriving (Read, Show, Eq)

-- | Type for command arguments
data CommandArgs
  = InfoArgs
  | ReadArgs
  | WriteArgs { token     :: B.ByteString
              , lock      :: Core.WriteLock
              , writeType :: Core.Command
              }
  deriving (Read, Show, Eq)

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
tokenParser :: Parser B.ByteString
tokenParser = B.pack <$> parser desc
  where
    desc = ( metavar "TOKEN"
          <> help "Token in hex fomat. Length 5 byte.")
    parser = argument str

-- | Write type parser.
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

-- | Cli args parser.
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

-- Main

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
