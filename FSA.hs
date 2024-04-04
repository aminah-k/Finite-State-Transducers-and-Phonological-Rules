------------------------------------------------------------------------
-- When you import this module into ghci, the things that you will bring
-- into scope are the things in the parentheses after "module FSA" below
------------------------------------------------------------------------

module FSA (
  -- the type FSA and its constructor FSAWith, and the synonyms
  -- State for Int, and Transition for (State, Char, State)
  FSA(..), State, Transition, EpsFSA(..), EpsTransition
  -- convenience functions for extracting the relevant components
  -- of a transition
  , source, target, label
  -- the main functions that move through an FSA
  , step, walk
  -- the parser function that determines whether an FSA matches a string
  , accepts
  -- our list processing and relation composition operators
  , return, (>=>)
  -- dealing with epsilon transitions
  , epsClosure, unEps
  -- convenience functions
  , showFSA
) where

import Prelude hiding (return)
import Data.List (nub)


-- onto the definitions

------------------------------------------------------------------------
-- some little functions from class to compose lists and/or list-building
-- functions
------------------------------------------------------------------------

return :: a -> [a]
return = \a -> [a]

(>=>) :: (a -> [a]) -> (a -> [a]) -> (a -> [a])
f >=> g = \a -> concat (map g (f a))


------------------------------------------------------------------------
-- the type FSA defines the different forms a regular
-- expression can take
------------------------------------------------------------------------

type State = Int                       -- note that these are also just
type Transition = (State, Char, State) -- type synonyms (abbreviations)

source (r, c, s) = r  -- some convenience functions
target (r, c, s) = s  -- for working with transitions
label  (r, c, s) = c

data FSA =
  FSAWith [State] [Char] [State] [State] [Transition]
  --         Q     Sigma    I       F        Delta

------------------------------------------------------------------------
-- a convenience function print the FSA out in a slightly more readable
-- format in ghci
------------------------------------------------------------------------

showFSA (FSAWith states sigma initials finals delta) = do
  putStrLn $ "states:      " ++ show states
  putStrLn $ "alphabet:    " ++ show sigma
  putStrLn $ "initial:     " ++ show initials
  putStrLn $ "final:       " ++ show finals
  putStrLn $ "transitions: " ++ show delta

------------------------------------------------------------------------
-- functions from class to move through a machine while consuming
-- a string
------------------------------------------------------------------------

step :: [Transition] -> Char -> (State -> [State])
step delta x = \q -> [ target tr | tr <- delta, source tr == q, label tr == x ]

walk :: [Transition] -> String -> (State -> [State])
walk delta ""     = return
walk delta (c:cs) = step delta c >=> walk delta cs

------------------------------------------------------------------------
-- finally, a function to test whether a given FSA accepts/generates
-- a given string
------------------------------------------------------------------------

accepts :: FSA -> String -> Bool
accepts (FSAWith states syms initial final delta) string =
  or [ elem qn final | q0 <- initial, qn <- walk delta string q0 ]


------------------------------------------------------------------------
-- here, we define an FSA variant that allows "empty" transitions;
-- opportunities to move between states without consuming any characters
------------------------------------------------------------------------

type EpsTransition = (State, Maybe Char, State)
data EpsFSA =
  EpsWith [State] [Char] [State] [State] [EpsTransition]

------------------------------------------------------------------------
-- unEps is a function to convert one of these epsilon automoata to a
-- regular FSA: you'll need to use this in order to use any of the FSA
-- functions above (like `accepts` and `showFSA`), but you don't need to
-- understand it (though it's not complicated)
------------------------------------------------------------------------

epsClosure :: [EpsTransition] -> State -> [State]
epsClosure delta q =
    let outgoingEpsilons q' = filter (\(q1,x,q2) -> q1 == q' && x == Nothing) delta
        oneStepFrom q' = map (\(q1,x,q2) -> q2) (outgoingEpsilons q')
        update qs = nub (qs ++ (concat (map oneStepFrom qs)))
     in until (\qs -> update qs == qs) update [q]

unEps :: EpsFSA -> FSA
unEps (EpsWith states syms i f delta) =
  let validTransition (q1,x,q2) = or (map (\q' -> elem q' (epsClosure delta q1) && elem (q', Just x, q2) delta) states)
      newDelta = filter validTransition [(x,y,z) | x <- states, y <- syms, z <- states]
      canReachEnd q = or (map (\q' -> elem q' f) (epsClosure delta q))
      newEnds = filter canReachEnd states
   in FSAWith states syms i newEnds newDelta


data IntRule = LexRule Int String | BranchRule Int (Int, Int)
type IntCFG = [IntRule]
showIntRule (LexRule i s ) =
  "(" ++ show i ++ "," ++ s ++ ")"
showIntRule (BranchRule l (r1, r2) ) =
  "(" ++ show l ++ "," ++ show r1 ++ "," ++ show r2 ++ ")"
-- showIntCFG :: IntCFG -> IO ()
-- showIntCFG = sequence_ . map (putStrLn . showIntRule)
showIntCFG :: IntCFG -> String
showIntCFG [] = ""
showIntCFG (r:rs) = showIntRule r ++ "; " ++ showIntCFG rs
