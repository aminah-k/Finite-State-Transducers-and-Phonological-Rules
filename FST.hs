------------------------------------------------------------------------
-- When you import this module into ghci, the things that you will bring
-- into scope are the things in the parentheses after "module FST" below
------------------------------------------------------------------------

module FST where

import Prelude hiding (return, Monoid(..), Semigroup(..))
import Control.Arrow (second)
import Algebra

-- onto the definitions

------------------------------------------------------------------------
-- some little functions from class to compose lists and/or list-building
-- functions
------------------------------------------------------------------------

return :: a -> [a]
return = \a -> [a]

(>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
f >=> g = \a -> [c | b <- f a, c <- g b ]

------------------------------------------------------------------------
-- to build an FSA, you need
-- a set of states
-- a set of input characters
-- an initial set of tagged states
-- a final set of tagged
-- a set of legitimate transitions, each consuming an input character
------------------------------------------------------------------------
type State = Int
type Transition = (State, Char, State)

data FSA =
  FSAWith [State] [Char] [State] [State] [Transition]
  --         Q     Sigma    I       F        Delta


------------------------------------------------------------------------
-- to build an FST, you need:
-- an initial set of tagged states
-- a final set of tagged
-- a set of legitimate transitions, consuming input and producing output
------------------------------------------------------------------------

type Arc c w = (State, (c, w), State) -- type synonyms (abbreviations)

data FST c w =
  FSTWith [(State, w)] [(State, w)] [Arc c w]
  --           I            F         Delta

------------------------------------------------------------------------
-- a convenience function print the FST out in a slightly more readable
-- format in ghci
------------------------------------------------------------------------

showFST (FSTWith initials finals delta) = do
  putStrLn $ "initial:     " ++ show initials
  putStrLn $ "final:       " ++ show finals
  putStrLn $ "transitions: " ++ show delta

------------------------------------------------------------------------
-- convenience functions to extract the components of a machine
------------------------------------------------------------------------


starts (FSTWith i f d) = i
finals (FSTWith i f d) = f
ttable (FSTWith i f d) = d


------------------------------------------------------------------------
-- functions from class to move through a machine while consuming
-- a string, merging convergent paths as we go
------------------------------------------------------------------------

step :: (Eq c, Monoid w) => FST c w -> c -> (State, w) -> [(State, w)]
step fst c =
  \(q,w1) -> [(t, w1 <> w2) | (s,(x,w2),t) <- ttable fst, s == q, x == c]

walk :: (Eq c, Monoid w) => FST c w -> [c] -> (State, w) -> [(State, w)]
walk fst []     = return
walk fst (c:cs) = step fst c >=> walk fst cs


------------------------------------------------------------------------
-- functions to compute all the ways of converting an input to an output
-- by walking through a transducer
------------------------------------------------------------------------

enter :: FST c w -> () -> [(State, w)]
enter fst = \() -> starts fst

ambit :: (Eq c, Monoid w) => FST c w -> [c] -> [(State, w)]
ambit fst u = run ()
  where run = enter fst >=> walk fst u

transduce :: (Eq c, Monoid w) => FST c w -> [c] -> [w]
transduce fst u =
  [ v <> wf | (qn, v) <- ambit fst u, (qf, wf) <- finals fst, qf == qn ]

reduce :: (Eq c, Semiring w) => FST c w -> [c] -> w
reduce m u = summarize (transduce m u)


------------------------------------------------------------------------
-- versions of the above functions that take advantage of the semiring
-- laws in the output type to compress walks at each step, rather than
-- holding onto converging paths
------------------------------------------------------------------------

walk' :: (Eq c, Semiring w) => FST c w -> [c] -> (State, w) -> [(State, w)]
walk' fst []     = return
walk' fst (c:cs) = compress . go
  where go = step fst c >=> walk' fst cs

ambit' ::(Eq c, Semiring w) => FST c w -> [c] -> [(State, w)]
ambit' fst u = run ()
  where run = enter fst >=> walk' fst u

transduce' :: (Eq c, Semiring w) => FST c w -> [c] -> [w]
transduce' fst u =
  [ v <> w | (qn, v) <- ambit' fst u, (qf, w) <- finals fst, qf == qn ]

reduce' :: (Eq c, Semiring w) => FST c w -> [c] -> w
reduce' m u = summarize (transduce' m u)


------------------------------------------------------------------------
-- an example automaton
------------------------------------------------------------------------

fsa7 :: FSA
fsa7 =
  FSAWith [1,2,3]
          ['C','V']
          [1]
          [1]
          [(1,'V',1), (1,'C',2), (1,'V',3),
           (2,'V',1), (2,'V',3), (3,'C',1)]

------------------------------------------------------------------------
-- some example transducers
------------------------------------------------------------------------

fst7 :: FST Char Bool
fst7 =
  FSTWith [(1, True)]
          [(1, True)]
          [(1,('V',True),1), (1,('C',True),2), (1,('V',True),3),
           (2,('V',True),1), (2,('V',True),3), (3,('C',True),1)]

fst8 :: FST Char Weight
fst8 =
  FSTWith [(1, 1.0)] [(1, 1.0)]
          [(1,('V',0.9),1), (1,('C',1.0),2), (1,('V',0.9),3),
           (2,('V',1.0),1), (2,('V',1.0),3), (3,('C',0.8),1)]

fst25 :: FST Char Prob
fst25 =
  FSTWith [(1, 1.0)] [(1, 0.1)]
          [(1,('V',0.2),1), (1,('C',0.5),2), (1,('V',0.2),3),
           (2,('V',0.5),1), (2,('V',0.5),3), (3,('C',1.0),1)]

fst37 :: FST Char Cost
fst37 =
  FSTWith [(1, Fin 0)] [(1, Fin 0)]
          [(1,('V',Fin 1),1), (1,('C',Fin 0),2), (1,('V',Fin 1),3),
           (2,('V',Fin 0),1), (2,('V',Fin 0),3), (3,('C',Fin 2),1)]

data CV = C | V deriving (Eq,Show)
fstCVStrings :: FST CV String
fstCVStrings = FSTWith i f d
  where i = [(1, "")]
        f = [(1, "")]
        d = [ (1,(V,"V"),1), (1,(C,"eC"),2), (2,(V,"V"),1)
            , (2,(V,"V"),3), (1,(V,"V" ),3), (3,(C,"" ),1) ]
