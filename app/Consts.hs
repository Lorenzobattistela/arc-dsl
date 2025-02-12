module Consts where

--------------------------------------------------------------------------------
-- Boolean Constants
--------------------------------------------------------------------------------

f :: Bool
f = False

t :: Bool
t = True

--------------------------------------------------------------------------------
-- Numeric Constants
--------------------------------------------------------------------------------

zero :: Integer
zero = 0

one :: Integer
one = 1

two :: Integer
two = 2

three :: Integer
three = 3

four :: Integer
four = 4

five :: Integer
five = 5

six :: Integer
six = 6

seven :: Integer
seven = 7

eight :: Integer
eight = 8

nine :: Integer
nine = 9

ten :: Integer
ten = 10

negOne :: Integer
negOne = -1

negTwo :: Integer
negTwo = -2

--------------------------------------------------------------------------------
-- Directional and Coordinate Constants
--------------------------------------------------------------------------------

-- | Directional tuples (represented as a pair of Integers)
down :: (Integer, Integer)
down = (1, 0)

right :: (Integer, Integer)
right = (0, 1)

up :: (Integer, Integer)
up = (-1, 0)

left :: (Integer, Integer)
left = (0, -1)

-- | Other coordinate constants
origin :: (Integer, Integer)
origin = (0, 0)

unity :: (Integer, Integer)
unity = (1, 1)

negUnity :: (Integer, Integer)
negUnity = (-1, -1)

upRight :: (Integer, Integer)
upRight = (-1, 1)

downLeft :: (Integer, Integer)
downLeft = (1, -1)

zeroByTwo :: (Integer, Integer)
zeroByTwo = (0, 2)

twoByZero :: (Integer, Integer)
twoByZero = (2, 0)

twoByTwo :: (Integer, Integer)
twoByTwo = (2, 2)

threeByThree :: (Integer, Integer)
threeByThree = (3, 3)
