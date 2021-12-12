{-# LANGUAGE RankNTypes #-}

module Basics where

-- This is a sum type. A value of type "IntResult" can either be
-- a IntSuccess containing a Int, or a IntFailure.
--
-- This will be used to identify operations that fail (for example, because
-- of a divide by 0 operation).
data IntResult
  = IntSuccess Int
  | IntFailure
  deriving (Show, Eq)

-- | This is a generic (parametric) type. It represents something that can
-- either fail, or succeed with a value of type "a".
data Result a
  = Success a
  | Failure
  deriving (Show, Eq)

-- | Represent the kind of expressions one can compute on a pocket calculator.
data Operation
  = Add Operation Operation
  | Sub Operation Operation
  | Mul Operation Operation
  | Div Operation Operation
  | Value Int
  deriving (Show, Eq)

-- | A (linked) list is either empty, or contains an element (head) and a list (tail).
data List a
  = Empty
  | Cons a (List a)
  deriving (Show)

data Solution = Solution
  { _add :: Int -> Int -> Int,
    _divide :: Int -> Int -> Int,
    _divide' :: Int -> Int -> IntResult,
    _divide'' :: Int -> Int -> Result Int,
    _eval :: Operation -> Int,
    _eval' :: Operation -> Result Int,
    _listHead :: forall a. List a -> Result a,
    _listTail :: forall a. List a -> Result (List a),
    _listSum :: List Int -> Int,
    _listEq :: forall a. Eq a => List a -> List a -> Bool,
    _toList :: forall a. List a -> [a],
    _lmap :: forall a b. (a -> b) -> List a -> List b,
    _ltraverse :: forall f a b. Applicative f => (a -> f b) -> List a -> f (List b)
  }