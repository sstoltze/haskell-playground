{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- Needed for Transforms G G G G G G
{-# LANGUAGE FlexibleInstances #-} -- Needed for the general Transforms
{-# LANGUAGE FunctionalDependencies #-} -- Needed for the dependency in And
{-# LANGUAGE TypeOperators #-} -- Needed for :::
import Prelude hiding (all, flip, map, filter, and)

-- Colours are atomic
data R -- Red
data G -- Green
data B -- Blue
data W -- White
-- No constructors means the only value of each of these types are undefined::R, ...

data Cube u f r b l d
-- This is not a concrete type, check with :kind R vs :kind Cube in repl
-- Cube needs more types specified to be concrete

-- To introduce the actual cubes in our puzzle, we use type aliases
type CubeRed = Cube R R R R R R -- A red cube
type CubeBlue = Cube B B B B B B -- A blue cube
-- The cubes from the puzzle:
type Cube1 = Cube B G W G B R
type Cube2 = Cube W G B W R R
type Cube3 = Cube G W R B R R
type Cube4 = Cube B R G G W W

-- Now we want to introduce functions on our cubes, namely rot, twist and flip. For types, 'class' introduces functions
class Transforms u f r b l d where
  rot :: Cube  u f r b l d -> Cube u r b l f d
  twist :: Cube u f r b l d -> Cube f r u l d b
  flip :: Cube u f r b l d -> Cube d l b r f u

-- Define it for green cubes
instance Transforms G G G G G G where
  rot = undefined
  twist = undefined
  flip = undefined

-- Define it for all cubes
instance Transforms u f r b l d where
  rot = undefined
  twist = undefined
  flip = undefined
-- Check the result by :t twist (flip (rot (undefined::Cube1)))

-- Booleans
data True
data False
-- Boolean functions
class And b1 b2 b | b1 b2 -> b where -- The | indicates dependency
  and :: b1 -> b2 -> b
-- Define And by listing the truth table
instance And True True True where and = undefined
instance And True False False where and = undefined
instance And False True False  where and = undefined
instance And False False False where and = undefined
-- Logic programming!

-- Lists
data Nil
data Cons x xs
data x ::: xs -- Infix type-level operators start with :
infixr 5 :::
