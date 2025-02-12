{-# LANGUAGE DeriveGeneric #-}

module Type where

import Prelude hiding (Integer)

import qualified Data.Set as Set
import GHC.Generics (Generic)

-- Boolean corresponds to Python's bool.
type Boolean = Bool

-- For Integer we choose Haskell's Int (note: Haskell also has an unbounded Integer type).
type Integer = Int

-- A tuple of two Integers.
type IntegerTuple = (Integer, Integer)

-- A Numerical is either an Integer or an IntegerTuple.
data Numerical
  = NumInteger Integer
  | NumTuple IntegerTuple
  deriving (Show, Eq, Ord, Generic)

-- An IntegerSet is a (frozen) set of Integers.
type IntegerSet = Set.Set Integer

-- A Grid is a tuple of tuples of Integer.
-- In Haskell we use a list of lists to represent a 2D immutable structure.
type Grid = [[Integer]]

-- A Cell is a tuple where the first element is an Integer and
-- the second element is an IntegerTuple.
type Cell = (Integer, IntegerTuple)

-- An Object is a frozen set of Cells.
type Object = Set.Set Cell

-- Objects is a frozen set of Object.
type Objects = Set.Set Object

-- Indices is a frozen set of IntegerTuple.
type Indices = Set.Set IntegerTuple

-- IndicesSet is a frozen set of Indices.
type IndicesSet = Set.Set Indices

-- A Patch is either an Object or a set of Indices.
data Patch
  = PatchObject Object
  | PatchIndices Indices
  deriving (Show, Eq, Ord, Generic)

-- An Element is either an Object or a Grid.
data Element
  = ElementObject Object
  | ElementGrid Grid
  deriving (Show, Eq, Ord, Generic)

-- A Piece is either a Grid or a Patch.
data Piece
  = PieceGrid Grid
  | PiecePatch Patch
  deriving (Show, Eq, Ord, Generic)

-- TupleTuple is meant to be a tuple of tuples.
-- In Haskell we define a generic alias for a nested list structure.
type TupleTuple a = [[a]]

-- ContainerContainer is similarly a container (e.g. list) of containers.
type ContainerContainer a = [[a]]
