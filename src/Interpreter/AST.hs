{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter.AST (
	Opcode(..),
	Modifier(..),
	Mode(..),
	Instruction(..),
	Warior,
	Address
) where

import Numeric.Natural
import Data.Ix			(Ix)
import Data.Monoid
import System.Random 	(Random, RandomGen, genRange, next, randomR)

data Opcode   = DAT
              | MOV
              | ADD
              | SUB
              | MUL
              | DIV
              | MOD
              | JMP
              | JMZ
              | JMN
              | DJN
              | CMP
              | SLT
              | SPL
              deriving (Eq)

data Modifier = A
              | B
              | AB
              | BA
              | F
              | X
              | I
              deriving (Eq)   

data Mode     = IMMEDIATE
              | DIRECT
              | INDIRECT
              | DECREMENT
              | INCREMENT
               deriving (Eq)


randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f 1 0 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q).  Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more random values will be generated
       -- than the minimum
       q = 1000
       k = h - l + 1
       magtgt = k * q

       -- generate random values until we exceed the target magnitude 
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = (v * b + (fromIntegral x - fromIntegral genlo))

instance Random Natural    where randomR = randomIvalIntegral

type Warior  = Natural
newtype Address = Address { runAddress :: Natural } deriving (Eq, Integral, Random, Num, Ord, Real, Enum, Ix)

instance Monoid Address where
    mempty  = Address 0
    mappend = (+)

data Instruction = Instruction { _opcode   :: Opcode
                               , _modifier :: Modifier
                               , _aMode    :: Mode
                               , _aNumber  :: Address
                               , _bMode    :: Mode
                               , _bNumber  :: Address
                               } deriving (Eq)