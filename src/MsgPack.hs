{-# LANGUAGE PatternGuards #-}

module MsgPack (
    Object(..), ObjectMap
  , pack, ToObject(..)
  , unpack, FromObject(..)
  , getObject
  , putObject

  , destObjectNil
  , destObjectBool
  , destObjectFloat
  , destObjectDouble
  , destObjectInt
  , destObjectRaw
  , destObjectArray
  , destObjectMap
  ) where

import Control.Applicative ((<$>),(<*>),(<|>))
import Control.Arrow ((***))
import Control.Monad (guard,replicateM)
import Data.Bits
import Data.Char (chr,ord)
import Data.Int
import Data.Maybe (mapMaybe)
import Data.Serialize
import Data.Word
import Numeric (showHex)
import qualified Data.ByteString as S
import qualified Data.Map as Map


-- Message Pack Messages ----------------------------------------------------

type ObjectMap = [(Object,Object)]

data Object
  = ObjectNil
  | ObjectBool Bool
  | ObjectFloat Float
  | ObjectDouble Double
  | ObjectInt Integer
  | ObjectRaw S.ByteString
  | ObjectArray [Object]
  | ObjectMap ObjectMap
    deriving (Show,Eq,Ord)


destObjectNil :: Object -> Maybe ()
destObjectNil obj = case obj of
  ObjectNil -> Just ()
  _         -> Nothing

destObjectBool :: Object -> Maybe Bool
destObjectBool obj = case obj of
  ObjectBool b -> Just b
  _            -> Nothing

destObjectInt :: Object -> Maybe Integer
destObjectInt obj = case obj of
  ObjectInt i -> Just i
  _           -> Nothing

destObjectFloat :: Object -> Maybe Float
destObjectFloat obj = case obj of
  ObjectFloat f -> Just f
  _             -> Nothing

destObjectDouble :: Object -> Maybe Double
destObjectDouble obj = case obj of
  ObjectDouble f -> Just f
  _              -> Nothing

-- | Destruct an array.
destObjectArray :: Object -> Maybe [Object]
destObjectArray obj = case obj of
  ObjectArray os -> Just os
  _              -> Nothing

-- | Destruct a map
destObjectMap :: Object -> Maybe [(Object,Object)]
destObjectMap obj = case obj of
  ObjectMap m -> Just m
  _           -> Nothing

-- | Destruct raw bytes.
destObjectRaw :: Object -> Maybe S.ByteString
destObjectRaw obj = case obj of
  ObjectRaw bytes -> Just bytes
  _               -> Nothing


-- Encoding --------------------------------------------------------------------

class ToObject a where
  toObject     ::  a  -> Object
  toObjectList :: [a] -> Object
  toObjectList  = ObjectArray . map toObject

instance ToObject Object where
  toObject = id

instance ToObject () where
  toObject _ = ObjectNil

instance ToObject Bool where
  toObject = ObjectBool

instance ToObject Char where
  toObject     = toObject . ord
  toObjectList = toObject . S.pack . map (toEnum . fromEnum)

instance ToObject Int where
  toObject = mkObjectInt

instance ToObject Int32 where
  toObject = mkObjectInt

instance ToObject Int64 where
  toObject = mkObjectInt

instance ToObject Float where
  toObject = ObjectFloat

instance ToObject Double where
  toObject = ObjectDouble

instance ToObject a => ToObject [a] where
  toObject = toObjectList

instance (Ord k, ToObject k, ToObject a) => ToObject (Map.Map k a) where
  toObject = ObjectMap . map (toObject *** toObject) . Map.toList

instance ToObject S.ByteString where
  toObject = ObjectRaw



-- Decoding --------------------------------------------------------------------

class FromObject a where
  fromObject     :: Object -> Maybe  a
  fromObjectList :: Object -> Maybe [a]
  defaultVal     :: Maybe a
  defaultVal = Nothing
  fromObjectList obj = do
    os <- destObjectArray obj
    mapM fromObject os

instance FromObject Object where
  fromObject = Just

instance FromObject () where
  fromObject = destObjectNil

instance FromObject Char where
  fromObject obj     = chr . fromInteger <$> destObjectInt obj
  fromObjectList obj = do
    bytes <- fromObject obj
    return (map (toEnum . fromEnum) (S.unpack bytes))

instance FromObject Bool where
  fromObject = destObjectBool

instance FromObject Int where
  fromObject obj = (fromInteger <$> destObjectInt obj) <|>
    (fromInteger . read <$> fromObject obj)

instance FromObject Int32 where
  fromObject obj = fromInteger <$> destObjectInt obj

instance FromObject Int64 where
  fromObject obj = fromInteger <$> destObjectInt obj

instance FromObject Integer where
  fromObject = destObjectInt

instance FromObject Float where
  fromObject = destObjectFloat

instance FromObject Double where
  fromObject = destObjectDouble

instance FromObject a => FromObject [a] where
  fromObject = fromObjectList

instance (Ord k, FromObject k, FromObject a)
      => FromObject (Map.Map k a) where
  fromObject obj = do
    objPairs <- destObjectMap obj
    return $ Map.fromList $ mapMaybe parse objPairs
    where
    parse (k,a) = (,) <$> fromObject k <*> fromObject a


instance FromObject S.ByteString where
  fromObject = destObjectRaw


-- Parsing Function ------------------------------------------------------------

unpack :: S.ByteString -> Either String Object
unpack  = runGet getObject

mkObjectInt :: Integral a => a -> Object
mkObjectInt  = ObjectInt . fromIntegral

getObject :: Get Object
getObject  = do
  tag <- getWord8
  case tag of
    0xc0 -> return ObjectNil
    0xc2 -> return (ObjectBool False)
    0xc3 -> return (ObjectBool True)
    0xca -> ObjectFloat  <$> getFloat32be
    0xcb -> ObjectDouble <$> getFloat64be
    0xcc -> mkObjectInt  <$> getWord8
    0xcd -> mkObjectInt  <$> getWord16be
    0xce -> mkObjectInt  <$> getWord32be
    0xcf -> mkObjectInt  <$> getWord64be
    0xd0 -> mkObjectInt  <$> getSint8
    0xd1 -> mkObjectInt  <$> getSint16
    0xd2 -> mkObjectInt  <$> getSint32
    0xd3 -> mkObjectInt  <$> getSint64
    0xda -> ObjectRaw    <$> getRaw16
    0xdb -> ObjectRaw    <$> getRaw32
    0xdc -> ObjectArray  <$> getArray16
    0xdd -> ObjectArray  <$> getArray32
    0xde -> ObjectMap    <$> getMap16
    0xdf -> ObjectMap    <$> getMap32
    _ | Just n   <- positiveFixed tag -> return (mkObjectInt n)
      | Just n   <- negativeFixed tag -> return (mkObjectInt n)
      | Just len <- fixedMap tag      -> ObjectMap   <$> getFixMap len
      | Just len <- fixedArray tag    -> ObjectArray <$> getFixArray len
      | Just len <- fixedRaw tag      -> ObjectRaw   <$> getFixRaw len
      | otherwise                     -> fail ("reserved tag " ++ showHex tag "")



positiveFixed :: Word8 -> Maybe Word8
positiveFixed n = do
  guard (not (testBit n 7))
  return (fromIntegral n)

negativeFixed :: Word8 -> Maybe Int8
negativeFixed n = do
  guard (n .&. 0xe0 == 0xe0)
  return (fromIntegral n)

fixedMap :: Word8 -> Maybe Int
fixedMap n = do
  guard (n .&. 0xf0 == 0x80)
  return (fromIntegral (n .&. 0xf))

fixedArray :: Word8 -> Maybe Int
fixedArray n = do
  guard (n .&. 0xf0 == 0x90)
  return (fromIntegral (n .&. 0xf))

fixedRaw :: Word8 -> Maybe Int
fixedRaw n = do
  guard (n .&. 0xe0 == 0xa0)
  return (fromIntegral (n .&. 0x1f))

getSint8 :: Get Int8
getSint8  = fromIntegral <$> getWord8

getSint16 :: Get Int16
getSint16  = fromIntegral <$> getWord16be

getSint32 :: Get Int32
getSint32  = fromIntegral <$> getWord32be

getSint64 :: Get Int64
getSint64  = fromIntegral <$> getWord64be

getSized :: Integral len => Get len -> (Int -> Get a) -> Get a
getSized getLen parse = do
  len <- getLen
  parse (fromIntegral len)

getFixRaw :: Int -> Get S.ByteString
getFixRaw  = getBytes

getRaw16 :: Get S.ByteString
getRaw16  = getSized getWord16be getBytes

getRaw32 :: Get S.ByteString
getRaw32  = getSized getWord32be getBytes

getFixArray :: Int -> Get [Object]
getFixArray len = replicateM len getObject

getArray16 :: Get [Object]
getArray16  = getSized getWord16be (flip replicateM getObject)

getArray32 :: Get [Object]
getArray32  = getSized getWord32be (flip replicateM getObject)

getMapEntry :: Get (Object,Object)
getMapEntry  = (,) <$> getObject <*> getObject

getFixMap :: Int -> Get ObjectMap
getFixMap len = replicateM len getMapEntry

getMap16 :: Get ObjectMap
getMap16  = getSized getWord16be (\ len -> replicateM len getMapEntry)

getMap32 :: Get ObjectMap
getMap32  = getSized getWord32be (\ len -> replicateM len getMapEntry)


-- Rendering Functions ---------------------------------------------------------

pack :: Object -> S.ByteString
pack  = runPut . putObject

putObject :: Putter Object
putObject msg = case msg of
  ObjectNil       -> putObjectNil
  ObjectBool b    -> putObjectBool b
  ObjectInt n     -> putObjectInt n
  ObjectFloat n   -> putObjectFloat n
  ObjectDouble n  -> putObjectDouble n
  ObjectRaw bytes -> putObjectRaw bytes
  ObjectArray ms  -> putObjectArray ms
  ObjectMap m     -> putObjectMap m

putObjectNil :: Put
putObjectNil  = putWord8 0xc0

putObjectBool :: Putter Bool
putObjectBool b
  | b         = putWord8 0xc2
  | otherwise = putWord8 0xc3

putObjectInt :: Putter Integer
putObjectInt i
  | within 0     0x7f   = putWord8 (fromInteger i)
  | within (-32) (-1)   = putWord8 (fromInteger i)

  | within 0 0xff       = putWord8 0xcc >> putWord8    (fromInteger i)
  | within 0 0xffff     = putWord8 0xcd >> putWord16be (fromInteger i)
  | within 0 0xffffffff = putWord8 0xce >> putWord32be (fromInteger i)
  | i >= 0              = putWord8 0xcf >> putWord64be (fromInteger i)

  | i >= -0x80          = putWord8 0xd0 >> putWord8    (fromInteger i)
  | i >= -0x8000        = putWord8 0xd1 >> putWord16be (fromInteger i)
  | i >= -0x80000000    = putWord8 0xd2 >> putWord32be (fromInteger i)
  | otherwise           = putWord8 0xd3 >> putWord64be (fromInteger i)
  where
  within lower upper = i >= lower && i <= upper

putObjectFloat :: Putter Float
putObjectFloat n = do
  putWord8 0xca
  putFloat32be n

putObjectDouble :: Putter Double
putObjectDouble n = do
  putWord8 0xcb
  putFloat64be n

putObjectRaw :: Putter S.ByteString
putObjectRaw bytes = do
  putRawHeader (S.length bytes)
  putByteString bytes

putRawHeader :: Putter Int
putRawHeader len
  | len <= 31     = putWord8 (0xa0 .|. fromIntegral len)
  | len <= 0xffff = putWord8 0xda >> putWord16be (fromIntegral len)
  | otherwise     = putWord8 0xdb >> putWord32be (fromIntegral len)

putObjectArray :: Putter [Object]
putObjectArray ms = do
  putArrayHeader (length ms)
  mapM_ putObject ms

putArrayHeader :: Putter Int
putArrayHeader len
  | len <= 15     = putWord8 (0x90 .|. fromIntegral len)
  | len <= 0xffff = putWord8 0xdc >> putWord16be (fromIntegral len)
  | otherwise     = putWord8 0xdd >> putWord32be (fromIntegral len)

putObjectMap :: Putter ObjectMap
putObjectMap m = do
  putMapHeader (length m)
  mapM_ (\ (k,v) -> putObject k >> putObject v) m

putMapHeader :: Putter Int
putMapHeader len
  | len <= 15     = putWord8 (0x80 .|. fromIntegral len)
  | len <= 0xffff = putWord8 0xde >> putWord16be (fromIntegral len)
  | otherwise     = putWord8 0xdf >> putWord32be (fromIntegral len)
