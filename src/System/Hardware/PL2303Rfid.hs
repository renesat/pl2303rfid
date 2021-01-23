{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : System.Hardware.PL2303Rfid
Copyright   : (c) Smolyakov Ivan, 2021
License     : MIT
Maintainer  : smol.ivan97@gmail.com
Stability   : experimental
-}
module System.Hardware.PL2303Rfid
  ( Command(..)
  , encodeCommand
  , decodeCommand
  , Status(..)
  , encodeStatus
  , decodeStatus
  , Request(..)
  , encodeRequest
  , dataChecksum
  , encodeLength
  , Response(..)
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.Char (chr, ord)
import           System.Hardware.Serialport()
-- import           Numeric (readHex)
import           Data.Bits (xor, shiftL, shiftR)
-- import           Data.Hex (hex)

encodeCode :: [Int] -> B.ByteString
encodeCode = B.pack . map chr

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

 -- | Convert command to bytestring
decodeCommand :: B.ByteString -> Command
decodeCommand bs = command
  where
    code = decodeCode bs
    command = case code of
      [1,  2] -> Info -- 0x0102
      [1,  3] -> Beep -- 0x0103
      [1,  4] -> LedColor -- 0x0104
      [1, 12] -> Read -- 0x010C
      [2, 12] -> Write2 -- 0x020C
      [3, 12] -> Write3 -- 0x030C


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

-- hexSeqToByteString :: [String] -> B.ByteString
-- hexSeqToByteString = B.pack . map (chr . fst . head . readHex)

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

decodeLength :: [Int] -> Int
decodeLength (byte1:byte2:[]) = byte1 * 256 + byte2

encodeRequest :: Request -> B.ByteString
encodeRequest req = reqHead <> reqLength <> reqCommand <> reqData <> reqChecksum
  where
    reqHead = encodeCode [170, 221] -- 0xAA 0xDD
    reqCommand = encodeCommand $ requestCommand req
    reqData = requestBody req
    dataLength = (B.length reqData) + 3
    reqLength = encodeLength dataLength
    reqChecksum = B.singleton . chr $ dataChecksum reqData

splitBySizes :: [Int] -> [a] -> ([[a]], [a])
splitBySizes sizes d = foldl f ([], d) sizes
  where
    f = \s size -> let (res, currentTail) = s
                       (prev, next) = splitAt size currentTail
                   in (res ++ [prev], next)


decodeRequest :: B.ByteString -> Request
decodeRequest bs = Request (decodeCommand $ encodeCode reqCommand) (encodeCode reqBody)
  where
    ((_reqHead:reqLengthCode:[]), allData) = splitBySizes [2, 2] $ decodeCode bs
    reqLength = decodeLength reqLengthCode
    ((reqCommand:reqBody:[]), checksum) = splitBySizes [2, reqLength - 3] allData
    -- -- reqHead = encodeCode [170, 221] -- 0xAA 0xDD
    -- reqCommand = encodeCommand $ requestCommand req
    -- reqData = requestBody req
    -- dataLength = (B.length reqData) + 3
    -- reqLength = encodeLength dataLength
    -- reqChecksum = B.singleton . chr $ dataChecksum reqData




-- createRequest :: Command -> B.ByteString -> B.ByteString
-- createRequest command d = reqHead <> reqLength <> reqData <> reqChecksum
--   where
--     reqHead =  hexSeqToByteString ["AA", "DD"]
--     reqData = (commandCode command) <> d
--     dataLength = (B.length d) + 3
--     reqLength = formatDataLength (dataLength)
--     reqChecksum = B.singleton . chr $ dataChecksum reqData


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
