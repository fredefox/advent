{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language BinaryLiterals #-}
module Main (main, toByteString, fromAsciiLittleEndian, fromAscii, toHex) where

import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Bits
import Debug.Trace
import qualified Data.Binary.Get as Get
import Control.Monad.State
import Data.Foldable
import Data.Char
import Control.Applicative
import Control.Monad.Except
import Test.QuickCheck
import GHC.Generics (Generic)

-- | Convert an ascii-representation of a little endian binary string
-- to 'Byte'String'.
toByteString :: String -> ByteString
toByteString = ByteString.pack . fmap word . chunksOf 8
  where
  word :: String -> Word8
  word xs = sum $ zipWith go (reverse [0..7]) xs
  go i = \case
    '1' -> bit i
    _   -> zeroBits

fromAsciiLittleEndian :: forall a . Bits a => String -> a
fromAsciiLittleEndian = fromAscii . reverse

fromAscii :: forall a . Bits a => String -> a
fromAscii xs = foldl' (.|.) zeroBits $ zipWith go [0..] xs
  where
  go i = \case
    '1' -> bit i
    _   -> zeroBits

toHex :: [Bool] -> String
toHex = fmap (c . byte) . chunksOf 4
  where
  byte :: [Bool] -> Int
  byte = foldl' (\acc b -> acc * 2 + fromEnum b) 0
  c = intToDigit

fromHex :: String -> [Bool]
fromHex = foldMap go
  where
  go :: Char -> [Bool]
  go = bits . b
  b :: Char -> Int
  b = digitToInt
  bits :: Int -> [Bool]
  bits n = testBit n <$> reverse [0..3]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
  [] -> []
  xs -> a : chunksOf n b
    where
    (a, b) = splitAt n xs

class Serialize a where
  serialize :: a -> [Bool]

data Packet = Packet
  { version :: Word8
  , packetType :: Word8
  , payload :: Payload
  }

deriving stock instance Eq Packet
deriving stock instance Show Packet

instance Binary Packet where
  get :: Get Packet
  get = do
    b <- Get.getByteString 3
    traceShow b undefined
  put = undefined

deriving stock instance Generic Packet

instance Arbitrary Packet where
  arbitrary = Packet <$> chooseBoundedIntegral (0, 0b111) <*> chooseBoundedIntegral (0, 0b111) <*> arbitrary

instance Serialize Packet where
  serialize Packet{version, packetType, payload}
    =  takeBits 3 version
    <> takeBits 3 packetType
    <> serialize payload

takeBits :: Bits a => Int -> a -> [Bool]
takeBits n a = testBit a <$> (reverse [0..pred n])

data Payload = Literal [Word8] | PayloadOperator Operator

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = \case
  [] -> Nothing
  xs -> Just (init xs, last xs)

instance Serialize Payload where
  serialize = \case
    Literal xs -> case unsnoc $ takeBits 4 <$> xs of
      Nothing -> []
      Just (xss, x) -> (join $ (True : ) <$> xss) <> (False : x)
    PayloadOperator op -> serialize op

deriving stock instance Eq Payload
deriving stock instance Show Payload

instance Binary Payload where
  get :: Get Payload
  get = undefined
  put = undefined

deriving stock instance Generic Payload

instance Arbitrary Payload where
  arbitrary = oneof [Literal <$> listOf1 (chooseBoundedIntegral (0, 0b1111)), PayloadOperator <$> arbitrary]

data Operator = Operator {labelI :: Bool, labelL :: Int, subPackets :: [Packet] }

deriving stock instance Eq Operator
deriving stock instance Show Operator

instance Arbitrary Operator where
  arbitrary = do
    l <- chooseInt (0, 3)
    Operator True l <$> vector @Packet l

instance Serialize Operator where
  serialize Operator { labelI, labelL, subPackets }
    =  takeBits 1 labelI
    <> takeBits (if labelI then 13 else 15) labelL
    <> foldMap serialize subPackets

type S a = ExceptT String (State [Bool]) a

fromBits :: Bits a => [Bool] -> a
fromBits xs = foldl' (.|.) zeroBits $ zipWith go [0..] (reverse xs)
  where
  go i = \case
    True -> bit i
    _   -> zeroBits

readN :: Bits a => Int -> S a
readN n = fromBits <$> consumeN n

consumeN :: Int -> S [Bool]
consumeN n = do
  xs <- Control.Monad.State.get
  if lengthAtLeast n xs then state (splitAt n) else throwError "Unexpected end of input"
-- consumeN n = do
--   (s0, s1) <- getN n
--   Control.Monad.State.put s1
--   pure s0
  
getN :: Int -> S ([Bool], [Bool])
getN n = do
  xs <- Control.Monad.State.get
  if lengthAtLeast n xs then pure (splitAt n xs) else throwError "Unexpected end of input"

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast n xs = not $ null $ drop (pred n) xs

rPacket :: S Packet
rPacket = do
  version <- readN 3
  -- xs <- Control.Monad.State.get
  -- version <- traceShow xs $ readN maxBound
  packetType <- readN 3
  payload <- rPayload packetType
  pure $ Packet { version, packetType, payload }

rPayload :: Word8 -> S Payload
rPayload = \case
  4 -> Literal <$> rLiteral
  _ -> PayloadOperator <$> rOperator

-- TODO
rOperator :: S Operator
rOperator = do
  i <- readN @Bool 1
  if i
  then
    do
      l <- readN 11
      packets <- repeatM l rPacket
      pure $ Operator i l packets
  else
    do
      l <- readN 15
      (s0, s1) <- getN l
      Control.Monad.State.put s0
      packets <- many rPacket
      Control.Monad.State.put s1
      pure $ Operator i l packets

repeatM :: Monad m => Int -> m a -> m [a]
repeatM 0 _ = pure mempty
repeatM n m = (:) <$> m <*> repeatM (pred n) m

rLiteral :: S [Word8]
rLiteral = do
  b <- readN @Bool 1
  n <- readN @Word8 4
  (n:) <$> if b then rLiteral else pure []

versions :: Packet -> [Word8]
versions Packet { version, payload } = version : vs
  where
  vs = case payload of
    Literal{} -> []
    PayloadOperator (Operator _ _ ps) -> foldMap versions ps

run :: [Bool] -> Either String Packet
run x = (`evalState` x) $ runExceptT rPacket

runHex :: String -> Either String Packet
runHex = run . fromHex

main :: IO ()
main = quickCheck prop
-- main = do
--   xs <- lines <$> getContents
--   let ks = runHex <$> xs
--   traverse_ print ks
--   traverse_ (print . fmap (sum . versions)) ks

propHex :: Property
propHex = forAll g (\x -> toHex (fromHex x) == x)
  where
  g = listOf $ intToDigit <$> chooseInt (0, 0xf)

prop :: Packet -> Bool
prop p = Right p == q
  where
  bs :: [Bool]
  bs = serialize p
  q :: Either String Packet
  q = run bs

-- 01100001 00110001 00110010
example :: Packet
example = Packet {version = 5, packetType = 0, payload = Literal [10]}

exampleBin :: [Bool]
exampleBin = serialize example

exampleHex :: String
exampleHex = toHex exampleBin

unit :: Bool
unit = run exampleBin == Right example
