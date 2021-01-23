{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : System.Hardware.PL2303Rfid
Copyright   : (c) Smolyakov Ivan, 2021
License     : MIT
Maintainer  : smol.ivan97@gmail.com
Stability   : experimental
-}
module System.Hardware.PL2303Rfid
  ( -- * PL2303Rfid types
    -- ** Command
    Command(..)
  , encodeCommand
  , decodeCommand
    -- ** Status
  , Status(..)
  , encodeStatus
  , decodeStatus
    -- ** Request
  , Request(..)
  , encodeRequest
  , decodeRequest
    -- ** Response
  , Response(..)
    -- ** Support function
  , dataChecksum
  , encodeLength
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.Attoparsec.ByteString.Char8 as AP
import           Data.Char (chr, ord)
import           System.Hardware.Serialport()
import           Data.Bits (xor, shiftL, shiftR)

-- | Conver ByteString to list of bytes
encodeCode :: [Int] -> B.ByteString
encodeCode = B.pack . map chr

-- | Conver list of bytes to ByteString
decodeCode :: B.ByteString -> [Int]
decodeCode = map ord . B.unpack

{-|
  Supported commands.

  Commands conformity:

  * 'Info'     -> 0x0102

  * 'Beep'     -> 0x0103

  * 'LedColor' -> 0x0104

  * 'Read'     -> 0x010C

  * 'Write2'   -> 0x020C

  * 'Write3'   -> 0x030C
-}
data Command
  = Info -- ^ Get model description
  | Beep -- ^ Beep
  | LedColor -- ^ Change LED color
  | Read -- ^ Read rfid code
  | Write2 -- ^ First method for write code to rfid
  | Write3 -- ^ Second method for write code to rfid
  deriving (Read, Show, Eq)

-- | Convert command to bytestring
encodeCommand :: Command -> B.ByteString
encodeCommand command = encodeCode code
  where
    code = case command of
      Info     -> [1,  2] -- 0x0102
      Beep     -> [1,  3] -- 0x0103
      LedColor -> [1,  4] -- 0x0104
      Read     -> [1, 12] -- 0x010C
      Write2   -> [2, 12] -- 0x020C
      Write3   -> [3, 12] -- 0x030C

commandParser :: Parser Command
commandParser = do
  code <- return . decodeCode =<< AP.take 2
  case code of
    [1,  2]   -> return Info     -- 0x0102
    [1,  3]   -> return Beep     -- 0x0103
    [1,  4]   -> return LedColor -- 0x0104
    [1, 12]   -> return Read     -- 0x010C
    [2, 12]   -> return Write2   -- 0x020C
    [3, 12]   -> return Write3   -- 0x030C
    otherwise -> fail "Not correct command code"

-- | Convert bytestring to command
decodeCommand :: B.ByteString -> Either String Command
decodeCommand = parseOnly commandParser

{-|
  Response status type

  Status conformity:

  * 'Ok' -> 0x00

  * 'Error' -> 0x01

  * 'ReadError1' -> 0x02

  * 'ReadError2' -> 0x03
-}
data Status
  = Ok
  | Error
  | ReadError1
  | ReadError2
  deriving (Read, Show, Eq)


encodeStatus :: Status -> B.ByteString
encodeStatus status = B.singleton $ chr code
  where
    code = case status of
      Ok         -> 0 -- 0x00
      Error      -> 1 -- 0x01
      ReadError1 -> 2 -- 0x02
      ReadError2 -> 3 -- 0x03

decodeStatus :: B.ByteString -> Status
decodeStatus bs = status
  where
    code = (decodeCode bs) !! 0
    status = case code of
      0 -> Ok         -- 0x00
      1 -> Error      -- 0x01
      2 -> ReadError1 -- 0x02
      3 -> ReadError2 -- 0x03


-- | Request type
data Request = Request
  { requestCommand :: Command
  , requestBody :: B.ByteString
  }
  deriving (Read, Show, Eq)

-- | Response type
data Response = Response
  { responseCommand :: Command
  , responseStatus :: Status
  , responseBody :: B.ByteString
  }
  deriving (Read, Show, Eq)

{-Requests must contain the checksum. Checksum is the xor of all bytes of data.
-}
dataChecksum :: B.ByteString -> Int
dataChecksum s = checksum
  where
    bytes = decodeCode s
    checksum = foldr (xor) 0 bytes

encodeLength :: Int -> B.ByteString
encodeLength l = encodeCode [byte1, byte2]
  where
    byte1 = shiftR l 8
    byte2 = xor (shiftL byte1 8) l

decodeLength :: B.ByteString -> Int
decodeLength l = byte1 * 256 + byte2
  where
    (byte1:byte2:[]) = decodeCode l

encodeRequest :: Request -> B.ByteString
encodeRequest req = reqHead <> reqLength <> reqCommand <> reqData <> reqChecksum
  where
    reqHead = encodeCode [170, 221] -- 0xAA 0xDD
    reqCommand = encodeCommand $ requestCommand req
    reqData = requestBody req
    dataLength = (B.length reqData) + 3
    reqLength = encodeLength dataLength
    reqChecksum = B.singleton . chr $ dataChecksum (reqCommand <> reqData)

requestParser :: Parser Request
requestParser = do
  _startCode <- (string $ encodeCode [170, 221]) <?> "Not correct start code (0xAA 0xDD)"

  reqLength <- return . decodeLength =<< AP.take 2
  reqCommand <- commandParser
  reqBody <- AP.take (reqLength - 3)

  realChecksum <- return $ chr $ dataChecksum (encodeCommand reqCommand <> reqBody)
  _checksum <- char realChecksum <?> "Not correct checksum"

  return $ Request reqCommand reqBody

decodeRequest :: B.ByteString -> Either String Request
decodeRequest = parseOnly requestParser


-- sendRequest :: SerialPort -> Command -> B.ByteString -> IO Int
-- sendRequest s c d = send s request
--   where
--     request = createRequest c d


-- formatResponseLength :: B.ByteString -> B.ByteString -> Int
-- formatResponseLength sbite1 sbite2 = l
--   where
--     bite1 = ord . head $ B.unpack sbite1
--     bite2 = ord . head $ B.unpack sbite2
--     l = (shiftL bite1 8) + bite2


-- readResponseData :: SerialPort -> Int -> IO B.ByteString
-- readResponseData s l = readResponseData' s l (B.pack "")

-- readResponseData' :: SerialPort -> Int -> B.ByteString -> IO Response
-- readResponseData' _ 0 d = return d
-- readResponseData' s l d = do
--   item <- recv s 1
--   readResponseData' s (l - 1) (d <> item)

-- readResponse :: SerialPort -> IO B.ByteString
-- readResponse s = do

--   -- Read control symbol
--   recv s 1 -- 0xAA
--   recv s 1 -- 0xDD

--   -- Read length of data
--   lb1 <- recv s 1 -- first bite of length
--   lb2 <- recv s 1 -- second bite of length
--   l <- return $ (formatResponseLength l1 l2) - 4

--   -- Read Command
--   recv s 1
--   recv s 1

--   -- Read status
--   status <- recv s 1

--   respData <- readResponseData s length
--   -- print respData
--   respData <- return $ hex respData
--   respData <- return $ (B.pack $ take (length * 2 - B.length respData) $ repeat '0') <> respData

--   recv s 1 >>= print
--   return respData
