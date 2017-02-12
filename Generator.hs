{- |
Module      :  CharacterGen
Description :  Generate a character with stats based on a seed argument.
Copyright   :  (c) Ludvig Böklin
License     :  LGPLv3

Maintainer  :  ludvig.boklin@protonmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Generator
( genCharacter ) where

import System.Random
import Character

----------------
-- Some lists --

firstnames = [ "John"
             , "Jack"
             , "Robert"
             , "Bernard"
             , "Daniel"
             , "Sam"
             , "Nick"
             , "Fred"
             , "George"
             , "Ron"
             , "Luke"
             , "Han"
             ]

lastnames = [ "Shepard"
            , "O'Neill"
            , "Ford"
            , "Lowe"
            , "Jackson"
            , "Carter"
            , "Solo"
            , "Moonwalker"
            , "Weasel"
            , "Castley"
            ]

nicknames = [ "Ken"
            , "Ben"
            , "Cando"
            , "Feeler"
            , "Garn"
            , "Rufus"
            , "Abbott"
            , "Costello"
            , "Lick"
            , "Moonboy"
            , "The Pilot"
            ]

-------------------------
-- Generator functions --

genFullname :: [String] -> [String] -> [String] -> Int -> Fullname
genFullname fns lns nns seed = Fullname fn ln nn
  where
    index = head $ randomRs (1, length ns - 1) $ mkStdGen seed
    fn    = ns !! index !! 0
    ln    = ns !! index !! 1
    nn    = ns !! index !! 2
    ns    = [ [a, b, c]
            | a <- fns, b <- lns, c <- nns
            ]

-- | TODO: Make the values of the traits have limited deviation from 0.5
genTraits :: Int -> Traits
genTraits seed =
    Traits { fearlessness       = v !! 0
           , communication      = v !! 1
           , determination      = v !! 2
           , confidence         = v !! 3
           , reactionQuickness  = v !! 4
           , fineMotorSkills    = v !! 5
           , criticalThinking   = v !! 6
           , logicalReasoning   = v !! 7
           , patternRecognition = v !! 8
           , attention          = v !! 9
           , mentalEndurance    = v !! 10
           , discipline         = v !! 11
           , emotionalStability = v !! 12
           } where
               v = take 13 . randomRs (1, 99) $ mkStdGen seed

{-genSkills :: Double -> Skills-}
{-genSkills seed =-}
    {-Skills { aim              = n-}
           {-, awareness        = n-}
           {-, creativity       = n-}
           {-, experience       = n-}
           {-, levelheadedness  = n-}
           {-, patience         = n-}
           {-, planning         = n-}
           {-, reflex           = n-}
           {-, teamCoordination = n-}
           {-} where-}
               {-n   = rns !! 0-}
               {-rns = take 9 . randomRs (1, 99) $ mkStdGen seed-}

-- | TODO: change sks from taking a seed to tts and have that make sense
genCharacter :: Int -> Character
genCharacter seed =
    Character { fullname = ns
              , age = ag
              , description = dc
              , traits = ts
              , skills = ss
              } where
                  ns = genFullname firstnames lastnames nicknames seed
                  ag = head . randomRs (13, 36) $ mkStdGen seed
                  dc = (nickname ns) ++ " is a great person."
                  ts = genTraits seed
                  ss = skillsFromTraits ts
