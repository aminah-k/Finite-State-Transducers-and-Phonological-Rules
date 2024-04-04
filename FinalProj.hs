{-# LANGUAGE NoImplicitPrelude #-}


module FinalProj where 


-- The only bits of Haskell that will be available to you in this assignment
-- are the things you see imported here at the top. In particular, the only
-- pre-defined Haskell functions and types that you'll be able to use are the
-- ones you see in `import Prelude` block. DO NOT CHANGE ANYTHING IN THIS
-- IMPORT BLOCK AND DO NOT IMPORT ANY ADDITIONAL FUNCTIONS.

import FST     -- all the functions exported by our FST.hs file
import FSA
import Algebra -- all types and classes in the Algebra.hs file
import Prelude
  ( -- the basic types that we've seen for integers, booleans, characters, and strings
    Int, Bool(True, False), Char, String
    -- the type constructor Maybe (and its data constructors, Just and Nothing)
  , Maybe(..)
    -- some basic arithmetic operations and relations:
  , (+), (-), (*), (<), (>), (==), (>=), (<=), maximum, minimum, max, min
    -- some basic boolean operations:
  , (&&), (||), not
    -- some list inspection and processing functions
  , take, drop, or, and, any, all, map, filter, concat, elem, (++), (.)
    -- and some classes for showing and comparing things (don't worry about these)
  , Show, Eq, undefined
  )

-- FSA to check if FST output is valid
fsaCheck :: FSA.FSA
fsaCheck = FSA.FSAWith states syms i f (d1 ++ d2) where 
    states = [1,2,3,4,5,6,7,8,9,10,12,13,14,15,16]
    syms   = ['a', 'A', 'O', 'w', 'r', 't', 'T', 'e']
    i      = [1]
    f      = [14,4,9,10]
        -- rider --> "rAder", ride --> "rAd"
    d1  = [(1, 'r', 2), (2, 'A', 3), (3, 'd', 4), (3, 'T', 12), (12, 'e', 13), (13, 'r', 14)]
        -- write --> "wrait", writing --> "wrAt"
    d2  = [(1, 'w', 15), (15, 'r', 16), (16, 'O', 4), (4, 't', 10), (4, 'T', 7), (7, 'e', 8), (8, 'r', 9)]

-- Rule A
fstVowelRaising :: FST Char String 
fstVowelRaising = FSTWith i f d where
  i = [(1, "")] -- initial state
  f = [(1, ""), (2, "A")] -- accept states
  d = 
    -- 1 -> 1 for all letters in the alphabet except A (ɑɪ)
    [(1, (c,[c]), 1) | c <- ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','O','T'] ]  
    -- 1 -> 2 for when you see an A (ɑɪ) as input, delete it 
    ++ [(1, ('A', ""), 2)]             
    -- 2 -> 1 for all voiceless consonants, raise the vowel by making the 'A' (ɑɪ) into an 'O' (ʌɪ)
    ++ [(2, ('p', "Op"), 1)]++[(2, ('k', "Ok"), 1)]++[(2, ('t', "Ot"), 1)]++[(2, ('f', "Of"), 1)]++[(2, ('s', "Os"), 1)]++[(2, ('c', "Oc"), 1)]++[(2, ('h', "Ah"), 1)]
    -- 2 -> 1 for all voiced consonants, keep it unraised A (ɑɪ)
    ++ [(2, ('b', "Ab"), 1)]++[(2, ('d', "Ad"), 1)]++[(2, ('g', "Ag"), 1)]++[(2, ('z', "Az"), 1)]++[(2, ('v', "Av"), 1)]++[(2, ('x', "Ax"), 1)]
    ++ [(2, ('m', "Am"), 1)]++[(2, ('n', "An"), 1)]++[(2, ('l', "Al"), 1)]++[(2, ('r', "Ar"), 1)]++[(2, ('w', "Aw"), 1)]++ [(2, ('j', "Aj"), 1)]

-- Rule B
fstPlosiveToTap :: FST Char String
fstPlosiveToTap = FSTWith i f d where
  i = [(1, "")] -- initial state
  f = [(1, ""), (2, ""), (3, "t"), (4, "d")] -- accept states
  d = 
    -- 1 -> for all consonants
    [(1, (c,[c]), 1) | c <- ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z','T']]
    -- 1 -> 2 for all vowels
    ++ [(1, (c,[c]), 2) | c <- ['a','e','i','o','u','y','A','O']]
    -- 2 -> 3 for when you see a 't' as input, delete it
    ++ [(2, ('t', ""), 3)] 
    -- 3 -> 1 for when you see a vowel following the 't', change the 't' to a 'T' (ɾ)
    ++ [(3, (c,['T']++[c]), 1) | c <- ['a','e','i','o','u','y','A','O']]
    -- 3 -> 1 if you dont see a vowel, keep the t as it used to be
    ++ [(3, (c,['t']++[c]), 1) | c <- ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z','T']]
    -- 2 -> 4 for when you see a 'd' as input, delete it
    ++ [(2, ('d', ""), 4)] 
    -- 4 -> 1 for when you see a vowel following the 'd', change the 'd' to a 'T' (ɾ)
    ++ [(4, (c,['T']++[c]), 1) | c <- ['a','e','i','o','u','y','A','O']]
    -- 4 -> 1 if you dont see a vowel, keep the 'd' as it used to be
    ++ [(4, (c,['d']++[c]), 1) | c <- ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z','T']]

ruleA_ruleB :: String -> [String]
ruleA_ruleB = transduce fstVowelRaising FST.>=> transduce fstPlosiveToTap

-- incorrect order of rules
ruleB_ruleA :: String -> [String]
ruleB_ruleA = transduce fstPlosiveToTap FST.>=>  transduce fstVowelRaising 

