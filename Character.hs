{- |
Module      :  Character
Description :  Generate a character with stats based on a seed argument.
Copyright   :  (c) Ludvig Böklin
License     :  LGPLv3

Maintainer  :  ludvig.boklin@protonmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module Character
( Character(..)
, Name(..)
, Fullname(..)
, Skill
, Skills(..)
, Traits(..)
, characterToString
, fullnameToString
, skillsToString
, skillP
, skillToInt
) where

import System.Random
import TermColors

data Character = Character
    { fullname    :: !Fullname
    , age         :: !Int
    , description :: !String
    , traits      :: !Traits
    , skills      :: !Skills
    } deriving (Show)

characterToString :: Character -> String
characterToString character = foldl (++) h [x ++ "\n" | x <- [n, a, d, s]]
  where
    cbc = colorBegin Cyan
    ce  = colorEnd
    cr  = colorWrap Red
    cy  = colorWrap Yellow
    cm  = colorWrap Magenta
    cc  = colorWrap Cyan
    p   = colorWrap Red "----------------------------\n"
    h   = cbc ++ p ++ p ++ (colorWrap Red "Generated Character Info:\n") ++ p
    ns  = fullname character
    fn  = (++) (cy "Firstname:   ") $ firstname ns
    ln  = (++) (cy "Lastname:    ") $ lastname ns
    nn  = (++) (cy "Nickname:    ") $ nickname ns
    n   = fn ++ "\n" ++ ln ++ "\n" ++ nn
    a   = (++) (cy "Age:         ") $ show $ age character
    d   = (++) (cy "Description: ") $ description character
    s   = (++) (p ++ p ++ cr ("Skills:\n" ++ p)) $ skillsToString $ skills character

-----------------
-- Name ---------

data Name = Firstname String
          | Lastname  String
          | Nickname  String
          deriving (Show, Eq, Ord)

data Fullname = Fullname
    { firstname :: !String
    , lastname  :: !String
    , nickname  :: !String
    } deriving (Show, Eq, Ord)

fullnameToString :: Fullname -> String
fullnameToString (Fullname fn ln nn) = foldr (++) "" ns
  where
    cy = colorWrap Yellow
    ns = [ (cy "Firstname:   ") ++ (show fn) ++ "\n"
         , (cy "Lastname:    ") ++ (show ln) ++ "\n"
         , (cy "Nickname:    ") ++ (show nn)
         ]

--      with the fields as value constructors.

data Skill = Aim
           | Awareness
           | Creativity
           | Experience
           | Levelheadedness
           | Patience
           | Planning
           | Reflex
           | TeamCoordination


data SkillP = SkillP Int deriving (Show, Read, Ord, Eq)

skillP :: Int -> SkillP
skillP n | 0 < n && n < 100 = SkillP n
        | otherwise         = error "Our universe can̈́'t work with that value."

skillToInt :: SkillP -> Int
skillToInt (SkillP n) = n

data Skills = Skills
    { aim              :: !SkillP
    , awareness        :: !SkillP
    , creativity       :: !SkillP
    , experience       :: !SkillP
    , levelHeadedness  :: !SkillP
    , patience         :: !SkillP
    , planning         :: !SkillP
    , reflex           :: !SkillP
    , teamCoordination :: !SkillP
    } deriving (Show, Read)

skillsToString :: Skills -> String
skillsToString (Skills ai lh cr re tc aw ex pl pa) = foldr (++) [] ss
  where sp = (.) show $ skillToInt
        ss = [ (colorWrap Yellow "Aim:               ") ++ sp ai ++ "%\n"
             , (colorWrap Yellow "Levelheadedness:   ") ++ sp lh ++ "%\n"
             , (colorWrap Yellow "Creativity:        ") ++ sp cr ++ "%\n"
             , (colorWrap Yellow "Reflex:            ") ++ sp re ++ "%\n"
             , (colorWrap Yellow "Team Coordination: ") ++ sp tc ++ "%\n"
             , (colorWrap Yellow "Awareness:         ") ++ sp aw ++ "%\n"
             , (colorWrap Yellow "Experience:        ") ++ sp ex ++ "%\n"
             , (colorWrap Yellow "Planning:          ") ++ sp pl ++ "%\n"
             , (colorWrap Yellow "Patience:          ") ++ sp pa ++ "%\n"
             ]

-- | TODO: Make a Trait enum,
--      with the fields as value constructors.

data Traits = Traits
    { fearlessness       :: !Int
    , communication      :: !Int
    , determination      :: !Int
    , confidence         :: !Int
    , reactionQuickness  :: !Int
    , fineMotorSkills    :: !Int
    , criticalThinking   :: !Int
    , logicalReasoning   :: !Int
    , patternRecognition :: !Int
    , attention          :: !Int
    , mentalEndurance    :: !Int
    , selfControl        :: !Int
    , emotionalStability :: !Int
    } deriving (Show, Read)

