module Tests.MsgPack where

import MsgPack

import Data.Int (Int8,Int64)
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.ByteString as S

msgPackTests :: Test
msgPackTests  = testGroup "msgpack"
  [ testProperty "prop_encodeDecode_Int8"  prop_encodeDecode_Int8
  , testProperty "prop_encodeDecode_Int64" prop_encodeDecode_Int64
  , testProperty "prop_encodeDecode"       prop_encodeDecode
  ]


object :: Int -> Gen Object
object depth
  | depth == 0 = frequency  cons
  | otherwise  = frequency (cons ++ recCons)
  where
  recurse        = object (depth - 1)

  cons =
    [ (10, return ObjectNil)
    , (10, ObjectBool   `fmap` arbitrary)
    , (10, ObjectFloat  `fmap` arbitrary)
    , (10, ObjectDouble `fmap` arbitrary)
    , (10, objectInt (0 :: Int64))
    , (5,  (ObjectRaw . S.pack) `fmap` resize 64 (listOf arbitraryBoundedRandom))
    ]

  recCons =
    [ (5, ObjectArray `fmap` resize 64 (listOf recurse))
    , (5, ObjectMap   `fmap` resize 64 (listOf objectMapentry))
    ]

  objectMapentry = do
    k <- recurse
    a <- recurse
    return (k,a)

objectInt :: (Bounded a, Integral a) => a -> Gen Object
objectInt sig = ObjectInt `fmap` choose (lo,hi)
  where
  lo = toInteger (minBound `asTypeOf` sig)
  hi = toInteger (maxBound `asTypeOf` sig)

prop_encodeDecode = forAll (object 3) $ \ obj ->
  unpack (pack obj) == Right obj

prop_encodeDecode_Int8 = forAll (objectInt (0 :: Int8)) $ \ obj ->
  unpack (pack obj) == Right obj

prop_encodeDecode_Int64 = forAll (objectInt (0 :: Int64)) $ \ obj ->
  unpack (pack obj) == Right obj
