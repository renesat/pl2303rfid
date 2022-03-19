{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : System.Hardware.PL2303Rfid.Core
Copyright   : (c) Smolyakov Ivan, 2021
License     : MIT
Maintainer  : smol.ivan97@gmail.com
Stability   : experimental
-}
module System.Hardware.PL2303Rfid.Core
  ( -- * PL2303Rfid types
    -- ** Command
    Command(..)
  , encodeCommand
  , decodeCommand
    -- ** Status
  , Status(..)
  , encodeStatus
  , decodeStatus
    -- ** Color
  , Color(..)
  , encodeColor
  , decodeColor
    -- ** WriteLock
  , WriteLock(..)
  , encodeWriteLock
  , decodeWriteLock
    -- ** Request
  , Request(..)
  , encodeRequest
  , decodeRequest
    -- ** Response
  , Response(..)
  , encodeResponse
  , decodeResponse
    -- ** Token
  , Token
  , toToken
  , fromToken
  , toHex
  , fromHex
    -- * Support functions
  , dataChecksum
  , encodeLength
  , decodeLength
    -- * IO
    -- ** IO core
  , sendRequest
  , recvResponse
    -- ** IO Uitls
  , doInfo
  , doBeep
  , doLedColor
  , doRead
  , doWrite
  , doWrite2
  , doWrite3
  ) where

import           Control.Monad                  ( when )
import           Control.Monad.Catch            ( Exception
                                                , MonadThrow
                                                , throwM
                                                )
import           Data.Attoparsec.ByteString.Char8
                                               as AP
import           Data.Bits                      ( shiftL
                                                , shiftR
                                                , xor
                                                )
import qualified Data.ByteString.Char8         as B
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Hex                       ( hex
                                                , unhex
                                                )
import           System.Hardware.Serialport     ( SerialPort(..)
                                                , recv
                                                , send
                                                )

---------------
-- Exception --
---------------

-- | TODO
data TokenLengthException = TokenLengthException Int

instance Exception TokenLengthException

instance Show TokenLengthException where
  show (TokenLengthException l) =
    concat ["Token length must been 5 but received ", show l]

-- | TODO
data UnhexException = UnhexException String

instance Exception UnhexException

instance Show UnhexException where
  show (UnhexException err) = concat ["Not converted to hex: ", err]

-- | TODO
data ParseCommandException = ParseCommandException String

instance Exception ParseCommandException

instance Show ParseCommandException where
  show (ParseCommandException err) = concat ["Parse command error: ", err]

-- | TODO
data ParseStatusException = ParseStatusException String

instance Exception ParseStatusException

instance Show ParseStatusException where
  show (ParseStatusException err) = concat ["Parse status error: ", err]

-- | TODO
data ParseWriteLockException = ParseWriteLockException String

instance Exception ParseWriteLockException

instance Show ParseWriteLockException where
  show (ParseWriteLockException err) = concat ["Parse write lock error: ", err]

-- | TODO
data ParseResponseException = ParseResponseException String

instance Exception ParseResponseException

instance Show ParseResponseException where
  show (ParseResponseException err) = concat ["Parse response error: ", err]

-- | TODO
data ParseRequestException = ParseRequestException String

instance Exception ParseRequestException

instance Show ParseRequestException where
  show (ParseRequestException err) = concat ["Parse request error: ", err]

-- | TODO
data ParseColorException = ParseColorException String

instance Exception ParseColorException

instance Show ParseColorException where
  show (ParseColorException err) = concat ["Parse color error: ", err]

 -- | TODO
data NotCompatibleTokenException = NotCompatibleTokenException

instance Exception NotCompatibleTokenException

instance Show NotCompatibleTokenException where
  show NotCompatibleTokenException = "Readonly/not compatible token"



-----------
-- Token --
-----------

-- | Token type. Must have a length of 5.
data Token = Token {-# UNPACK #-} !B.ByteString
  deriving (Read, Eq)

instance Show Token where
  show token = concat ["Token ", "\"0x", toHex token, "\""]

-- | Create Token from bytestring.
toToken :: (MonadThrow m) => B.ByteString -> m Token
toToken s = if l == 5
  then return $ Token s
  else throwM $ TokenLengthException l
  where l = B.length s

-- | Token to bytestring.
fromToken :: Token -> B.ByteString
fromToken (Token s) = s

-- | Convert token to hex string.
toHex :: Token -> String
toHex = B.unpack . hex . fromToken

-- | Convert hex string to Token.
fromHex :: (MonadThrow m) => String -> m Token
fromHex hs = case eitherString of
  Left  err -> throwM $ UnhexException err
  Right s   -> toToken s
  where eitherString = unhex $ B.pack hs

-------------
-- Command --
-------------

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
    Info     -> [1, 2] -- 0x0102
    Beep     -> [1, 3] -- 0x0103
    LedColor -> [1, 4] -- 0x0104
    Read     -> [1, 12] -- 0x010C
    Write2   -> [2, 12] -- 0x020C
    Write3   -> [3, 12] -- 0x030C

commandParser :: Parser Command
commandParser = do
  code <- return . decodeCode =<< AP.take 2
  case code of
    [1, 2 ] -> return Info     -- 0x0102
    [1, 3 ] -> return Beep     -- 0x0103
    [1, 4 ] -> return LedColor -- 0x0104
    [1, 12] -> return Read     -- 0x010C
    [2, 12] -> return Write2   -- 0x020C
    [3, 12] -> return Write3   -- 0x030C
    _       -> fail "Not correct command code"

-- | Convert bytestring to command
decodeCommand :: (MonadThrow m) => B.ByteString -> m Command
decodeCommand = decodeWrapper commandParser ParseCommandException

------------
-- Status --
------------

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

statusParser :: Parser Status
statusParser = do
  code <- return . ord =<< anyChar
  case code of
    0 -> return Ok         -- 0x0102
    1 -> return Error      -- 0x0103
    2 -> return ReadError1 -- 0x0104
    3 -> return ReadError2 -- 0x010C
    _ -> fail "Not correct status code"

decodeStatus :: (MonadThrow m) => B.ByteString -> m Status
decodeStatus = decodeWrapper statusParser ParseStatusException

---------------
-- WriteLock --
---------------

{-|
  Type for lock or not lock token after
  write.

  WriteLock conformity:

  * 'DefaultLock' -> 0x00

  * 'Lock' -> 0x01

-}
data WriteLock
  = DefultLock
  | Lock
  deriving (Read, Show, Eq)


encodeWriteLock :: WriteLock -> B.ByteString
encodeWriteLock lock = B.singleton $ chr code
 where
  code = case lock of
    DefultLock -> 0 -- 0x00
    Lock       -> 1 -- 0x01

writeLockParser :: Parser WriteLock
writeLockParser = do
  code <- return . ord =<< anyChar
  case code of
    0 -> return DefultLock -- 0x00
    1 -> return Lock       -- 0x01
    _ -> fail "Not correct lock code"

decodeWriteLock :: (MonadThrow m) => B.ByteString -> m WriteLock
decodeWriteLock = decodeWrapper writeLockParser ParseWriteLockException

--------------
-- LedColor --
--------------

{-|
  Color for LedColor command.

  Color conformity:

  * 'NoneColor' -> 0x00

  * 'RedColor' -> 0x01

  * 'GreenColor' -> 0x02

-}
data Color
  = RedColor
  | GreenColor
  | NoneColor
  deriving (Read, Show, Eq)


encodeColor :: Color -> B.ByteString
encodeColor color = B.singleton $ chr code
 where
  code = case color of
    NoneColor  -> 0 -- 0x00
    RedColor   -> 1 -- 0x01
    GreenColor -> 2 -- 0x02

colorParser :: Parser Color
colorParser = do
  code <- ord <$> anyChar
  case code of
    0 -> return NoneColor  -- 0x00
    1 -> return RedColor   -- 0x01
    2 -> return GreenColor -- 0x02
    _ -> fail "Not correct color code"

decodeColor :: (MonadThrow m) => B.ByteString -> m Color
decodeColor = decodeWrapper colorParser ParseColorException

-------------
-- Request --
-------------

-- | Request type
data Request = Request
  { requestCommand :: Command
  , requestBody    :: B.ByteString
  }
  deriving (Read, Show, Eq)

encodeRequest :: Request -> B.ByteString
encodeRequest req =
  reqHead <> reqLength <> reqCommand <> reqData <> reqChecksum
 where
  reqHead     = encodeCode [170, 221] -- 0xAA 0xDD
  reqCommand  = encodeCommand $ requestCommand req
  reqData     = requestBody req
  dataLength  = B.length reqData + 3
  reqLength   = encodeLength dataLength
  reqChecksum = B.singleton . chr $ dataChecksum (reqCommand <> reqData)

requestParser :: Parser Request
requestParser = do
  _startCode <-
    (string $ encodeCode [170, 221]) <?> "Not correct start code (0xAA 0xDD)"

  reqLength    <- return . decodeLength =<< AP.take 2
  reqCommand   <- commandParser
  reqBody      <- AP.take (reqLength - 3)

  realChecksum <- return $ chr $ dataChecksum
    (encodeCommand reqCommand <> reqBody)
  _checksum <- char realChecksum <?> "Not correct checksum"

  return $ Request reqCommand reqBody

decodeRequest :: (MonadThrow m) => B.ByteString -> m Request
decodeRequest = decodeWrapper requestParser ParseRequestException

--------------
-- Response --
--------------

-- | Response type
data Response = Response
  { responseCommand :: Command
  , responseStatus  :: Status
  , responseBody    :: B.ByteString
  }
  deriving (Read, Show, Eq)

encodeResponse :: Response -> B.ByteString
encodeResponse resp =
  respHead
    <> respLength
    <> respCommand
    <> respStatus
    <> respData
    <> respChecksum
 where
  respHead    = encodeCode [170, 221] -- 0xAA 0xDD
  respCommand = encodeCommand $ responseCommand resp
  respStatus  = encodeStatus $ responseStatus resp
  respData    = responseBody resp
  dataLength  = (B.length respData) + 4
  respLength  = encodeLength dataLength
  respChecksum =
    B.singleton . chr $ dataChecksum (respCommand <> respStatus <> respData)

responseParser :: Parser Response
responseParser = do
  _startCode <-
    (string $ encodeCode [170, 221]) <?> "Not correct start code (0xAA 0xDD)"

  respLength   <- return . decodeLength =<< AP.take 2
  respCommand  <- commandParser
  respStatus   <- statusParser
  respBody     <- AP.take (respLength - 4)

  realChecksum <- return $ chr $ dataChecksum
    (encodeCommand respCommand <> encodeStatus respStatus <> respBody)
  _checksum <- char realChecksum <?> "Not correct checksum"

  return $ Response respCommand respStatus respBody

decodeResponse :: (MonadThrow m) => B.ByteString -> m Response
decodeResponse = decodeWrapper responseParser ParseResponseException

---------------------
-- Core IO methods --
---------------------

sendRequest :: SerialPort -> Request -> IO Int
sendRequest sp req = send sp $ encodeRequest req

recvResponseBody :: SerialPort -> Int -> B.ByteString -> IO B.ByteString
recvResponseBody _  0 b = return b
recvResponseBody sp i b = do
  byte <- recv sp 1
  recvResponseBody sp (i - 1) (b <> byte)

recvResponse :: SerialPort -> IO Response
recvResponse sp = do
  -- TODO: rewrite this
  let waitHead = do
        head1 <- recv sp 1 -- 0xAA
        head2 <- if head1 == encodeCode [170] then recv sp 1 else return ""
        if head1 == encodeCode [170] && head2 == encodeCode [221]
          then return (head1, head2)
          else waitHead

  (head1, head2) <- waitHead

  len1           <- recv sp 1
  len2           <- recv sp 1
  len            <- return $ decodeLength (len1 <> len2)

  body           <- recvResponseBody sp len ""

  decodeResponse (head1 <> head2 <> len1 <> len2 <> body)

--------------
-- IO utils --
--------------

doCommand :: Command -> B.ByteString -> SerialPort -> IO Response
doCommand command body sp = sendRequest sp req >> recvResponse sp
  where req = Request command body


doInfo :: SerialPort -> IO B.ByteString
doInfo sp = responseBody <$> doCommand Info "" sp

doBeep :: Char -> SerialPort -> IO ()
doBeep beepLength sp = doCommand Beep (B.singleton beepLength) sp >> return ()

doLedColor :: Color -> SerialPort -> IO ()
doLedColor color sp = doCommand LedColor (encodeColor color) sp >> return ()

doRead :: SerialPort -> IO Token
doRead sp = toToken . responseBody =<< doCommand Read "" sp

doWrite :: WriteLock -> Token -> SerialPort -> IO ()
doWrite lock token sp = do
  newToken <- doCommand Write2 (encodeWriteLock lock <> fromToken token) sp
    >> doRead sp
  when (newToken == token) $ return ()

  newToken <- doCommand Write3 (encodeWriteLock lock <> fromToken token) sp
    >> doRead sp
  when (newToken == token) $ return ()

  throwM NotCompatibleTokenException


doWrite2 :: WriteLock -> Token -> SerialPort -> IO ()
doWrite2 lock token sp = do
  newToken <- doCommand Write2 (encodeWriteLock lock <> fromToken token) sp
    >> doRead sp
  when (newToken == token) $ return ()

  throwM NotCompatibleTokenException

doWrite3 :: WriteLock -> Token -> SerialPort -> IO ()
doWrite3 lock token sp = do
  newToken <- doCommand Write3 (encodeWriteLock lock <> fromToken token) sp
    >> doRead sp
  when (newToken == token) $ return ()

  throwM NotCompatibleTokenException

-----------------------
-- Support functions --
-----------------------

{-Requests must contain the checksum. Checksum is the xor of all bytes of data.
-}
dataChecksum :: B.ByteString -> Int
dataChecksum s = checksum
 where
  bytes    = decodeCode s
  checksum = foldr xor 0 bytes

{-Encode int to ByteString with len 2
-}
encodeLength :: Int -> B.ByteString
encodeLength l = encodeCode [byte1, byte2]
 where
  byte1 = shiftR l 8
  byte2 = xor (shiftL byte1 8) l

{-Convert ByteString with length 2 to int
-}
decodeLength :: B.ByteString -> Int
decodeLength l = byte1 * 256 + byte2 where [byte1, byte2] = decodeCode l


-------------------
-- Commonn utils --
-------------------

-- | Conver ByteString to list of bytes
encodeCode :: [Int] -> B.ByteString
encodeCode = B.pack . map chr

-- | Conver list of bytes to ByteString
decodeCode :: B.ByteString -> [Int]
decodeCode = map ord . B.unpack

-- | TODO
decodeWrapper
  :: (MonadThrow m, Exception e)
  => Parser a
  -> (String -> e)
  -> B.ByteString
  -> m a
decodeWrapper parser errInit bs = case parseOnly parser bs of
  Left  err     -> throwM $ errInit err
  Right command -> return command
