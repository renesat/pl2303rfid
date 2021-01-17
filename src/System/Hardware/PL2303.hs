module System.Hardware.PL2303 where

import qualified Data.ByteString.Char8 as B
import           Data.Char (chr, ord)
import           System.Hardware.Serialport
import           Numeric (readHex, showHex)
import           Data.Bits (xor, shiftL, shiftR)
import           Data.Hex (hex)

-- | Supported commands pl2303
data Command
  = Info -- ^ Get model description
  | Beep -- ^ Beep
  | LedColor -- ^ Change LED color
  | Read -- ^ Read rfid code
  | Write2 -- ^ First method for write code to rfid
  | Write3 -- ^ Second method for write code to rfid
  deriving (Read, Show, Eq)

{-|
  Get command code.

  Commands conformity:

  * 'Info'     -> 0x0102

  * 'Beep'     -> 0x0103

  * 'LedColor' -> 0x0104

  * 'Read'     -> 0x010C

  * 'Write2'   -> 0x020C

  * 'Write3'   -> 0x030C
-}
commandCode :: Command -> B.ByteString
commandCode command = B.pack $ map chr code
  where
    code = case command of
      Info     -> [1,  2] -- 0x0102
      Beep     -> [1,  3] -- 0x0103
      LedColor -> [1,  4] -- 0x0104
      Read     -> [1, 12] -- 0x010C
      Write2   -> [2, 12] -- 0x020C
      Write3   -> [3, 12] -- 0x030C


-- | Response status type
data ResponseStatus = Ok | Error | ReadError1 | ReadError2

-- | Request type
data Request = Request { requestCommand :: Command
                       , requestBody :: B.ByteString
                       }

-- | Response type
data Response = Response
  { responseCommand :: Command
  , responseStatus :: ResponseStatus
  , responseBody :: B.ByteString
  }



hexSeqToByteString :: [String] -> B.ByteString
hexSeqToByteString = B.pack . map (chr . fst . head . readHex)

dataChecksum :: B.ByteString -> Int
dataChecksum s = checksum
  where
    symbols = map ord $ B.unpack s
    checksum = foldr (xor) 0 symbols

formatDataLength :: Int -> B.ByteString
formatDataLength l = foldr1 (<>) $ map (B.singleton . chr) [byte1, byte2]
  where
    byte1 = shiftR l 8
    byte2 = xor (shiftL byte1 8) l



createRequest :: Command -> B.ByteString -> B.ByteString
createRequest command d = reqHead <> reqLength <> reqData <> reqChecksum
  where
    reqHead =  hexSeqToByteString ["AA", "DD"]
    reqData = (commandCode command) <> d
    dataLength = (B.length d) + 3
    reqLength = formatDataLength (dataLength)
    reqChecksum = B.singleton . chr $ dataChecksum reqData


sendRequest :: SerialPort -> Command -> B.ByteString -> IO Int
sendRequest s c d = send s request
  where
    request = createRequest c d


formatResponseLength :: B.ByteString -> B.ByteString -> Int
formatResponseLength sbite1 sbite2 = l
  where
    bite1 = ord . head $ B.unpack sbite1
    bite2 = ord . head $ B.unpack sbite2
    l = (shiftL bite1 8) + bite2


readResponseData :: SerialPort -> Int -> IO B.ByteString
readResponseData s l = readResponseData' s l (B.pack "")

readResponseData' :: SerialPort -> Int -> B.ByteString -> IO B.ByteString
readResponseData' _ 0 d = return d
readResponseData' s l d = do
  item <- recv s 1
  readResponseData' s (l - 1) (d <> item)

readResponse :: SerialPort -> IO B.ByteString
readResponse s = do
  recv s 1 >>= print
  recv s 1 >>= print
  length1 <- recv s 1
  length2 <- recv s 1
  length <- return $ (formatResponseLength length1 length2) - 4
  print length

  -- Command
  recv s 1 >>= print
  recv s 1 >>= print

  -- Status
  recv s 1 >>= print

  respData <- readResponseData s length
  -- print respData
  respData <- return $ hex respData
  respData <- return $ (B.pack $ take (length * 2 - B.length respData) $ repeat '0') <> respData

  recv s 1 >>= print
  return respData
