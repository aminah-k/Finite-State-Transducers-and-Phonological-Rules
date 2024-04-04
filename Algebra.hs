{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Algebra where

import Prelude hiding (Monoid(..), Semigroup(..))

-- some type synonyms, just for clarity in particular contexts
type Weight = Float
type Prob = Double
type Count = Int
data Cost = Fin Int | Inf deriving (Eq, Show)


-- a type `w` is a monoid if there are definitions for `idty` and `(<>)`
-- satisfying these equations:
--   1. idty <> x == x
--   2. x <> idty == x
--   3. (x <> y) <> z == x <> (y <> z)
class Monoid w where
  (<>) :: w -> w -> w
  idty :: w

instance Monoid Bool where
  (<>) = (&&)
  idty = True

instance Monoid Weight where
  (<>) = (*)
  idty = 1

instance Monoid Prob where
  (<>) = (*)
  idty = 1

instance Monoid Count where
  (<>) = (*)
  idty = 1

instance Monoid [a] where
  (<>) = (++)
  idty = []


-- a monoid `w` is a semiring if there is a function `summarize` that
-- `(<>)` distributes over; that is:
--   1. summarize [x, y, ..., z] <> k == summarize [x <> k, y <> k, ..., z <> k]
--   2. k <> summarize [x, y, ..., z] == summarize [k <> x, k <> y, ..., k <> z]
class Monoid w => Semiring w where
  summarize :: [w] -> w

instance Semiring Bool where
  summarize []     = False
  summarize (b:bs) = b || summarize bs

instance Semiring Weight where
  summarize []     = 0
  summarize (w:ws) = max w (summarize ws)

instance Semiring Prob where
  summarize []     = 0
  summarize (p:ps) = p + summarize ps

instance Semiring Count where
  summarize []     = 0
  summarize (n:ns) = n + summarize ns


------------------------------------------------------------------------
-- here we define a few functions that can work with any monoid/semiring
------------------------------------------------------------------------

-- generalize (<>) from an operator on two values to a function
-- on a whole list of values:
-- `aggregate` is to `(<>)` as `product` is to `(*)`, or
-- `and` is to `(&&)`, or `concat` is to `(++)`, etc.
aggregate :: Monoid a => [a] -> a
aggregate []     = idty
aggregate (a:as) = a <> aggregate as

-- specialize `summarize` from an operator on a whole list of values
-- to a binary operator on two values
-- `(<+>)` is to `summarize` as `(+)` is to `sum`, or
-- `(||)` is to `or`, or `max` is to `maximum`, etc.
(<+>) :: Semiring w => w -> w -> w
a <+> b = summarize [a,b]

-- just a little auxiliary function for removing a key from a dictionary
remove :: Eq c => c -> [(c, w)] -> [(c, w)]
remove c []             = []
remove c ((d,v) : tail) = if c == d then tail else (d,v) : remove c tail

-- merge two dictionaries, combining any overlapping keys by using the
-- appropriate semiring action `<+>` to summarize their associated values
merge :: (Eq c, Semiring w) => [(c, w)] -> [(c, w)] -> [(c, w)]
merge []             dict = dict
merge ((c,i) : tail) dict = case lookup c dict of
  -- if c isn't in dict, then go ahead and insert (c,i) into it
  Nothing -> merge tail ((c, i) : dict)
  -- otherwise, summarize the new value `i` and the existing value `j`
  -- and replace the old entry for the key `c` with the new one
  Just j  -> merge tail ((c, i <+> j) : remove c dict)

-- eliminate duplicated keys in a list by summarizing all the associated
-- values of the duplicated keys
compress :: (Eq c, Semiring w) => [(c, w)] -> [(c, w)]
compress ls = merge ls []
