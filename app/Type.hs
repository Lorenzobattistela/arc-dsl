{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Type where

import Prelude hiding (subtract, even, flip, repeat, Integer)
import Data.Set (Set)

type Boolean = Bool
type Integer = Int
type IntegerTuple = (Integer, Integer)

data Numerical = Single Integer | Pair IntegerTuple
  deriving (Show, Eq)

type IntegerSet = Set Integer

-- Grid is represented as a list of lists of Integers (instead of a tuple of tuples)
type Grid = [[Integer]]

-- A Cell is a tuple of an Integer and an IntegerTuple.
type Cell = (Integer, IntegerTuple)

-- An Object is an immutable set of Cells.
type Object = Set Cell

-- Objects is a set of Object.
type Objects = Set Object

-- Indices is an immutable set of IntegerTuple.
type Indices = Set IntegerTuple

-- IndicesSet is an immutable set of Indices.
type IndicesSet = Set Indices

-- A Patch is either an Object or a set of Indices.
data Patch = Obj Object | Ind Indices
  deriving (Show, Eq)

-- An Element is either an Object or a Grid.
data Element = ElemObject Object | ElemGrid Grid
  deriving (Show, Eq)

-- A Piece is either a Grid or a Patch.
data Piece = PGrid Grid | PPatch Patch
  deriving (Show, Eq)

-- TupleTuple is translated as a list of lists of Integers.
type TupleTuple = [[Integer]]

-- A container of containers (we use lists here)
type ContainerContainer a = [[a]]


