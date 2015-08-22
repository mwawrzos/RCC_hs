module Interpreter.Queue (
	module X,
	Queue(..),
	put,
	pop
) where

import Data.Functor as X (fmap, (<$>))
import Control.Applicative ((<*>))

data Queue a = Queue [a] [a]

instance Functor Queue where
    fmap f (Queue a b) = Queue (f <$> a) (f <$> b)

queue :: Queue a -> Queue a
queue (Queue [] r) = Queue (reverse r) []
queue q            = q

put :: a -> Queue a -> Queue a
put a (Queue f r) = queue $ Queue f (a : r)

qHead :: Queue a -> a
qHead (Queue (a : f) _) = a

qTail :: Queue a -> Queue a
qTail (Queue (a : f) r) = queue $ Queue f r

pop :: Queue a -> (a, Queue a)
pop = (,) <$> qHead <*> qTail