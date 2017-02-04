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
, Traits(..)
, Skills
, characterToString
, fullnameToString
, skillsFromTraits
, skillsToString
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

data Trait = Attention
           | Communication
           | Confidence
           | CriticalThinking
           | Determination
           | EmotionalStability
           | FineMotorSkills
           | LogicalReasoning
           | MentalEndurance
           | PatternRecognition
           | ReactionQuickness
           | Discipline
           | Fearlessness
           deriving (Show, Ord, Eq, Enum)

data Traits = Traits
    { attention          :: !Double
    , communication      :: !Double
    , confidence         :: !Double
    , criticalThinking   :: !Double
    , determination      :: !Double
    , discipline         :: !Double
    , emotionalStability :: !Double
    , fearlessness       :: !Double
    , fineMotorSkills    :: !Double
    , logicalReasoning   :: !Double
    , mentalEndurance    :: !Double
    , patternRecognition :: !Double
    , reactionQuickness  :: !Double
    } deriving (Show, Read)

{-data Skill = Aim Int-}
           {-| Awareness Int-}
           {-| Creativity Int-}
           {-| Experience Int-}
           {-| Levelheadedness Int-}
           {-| Patience Int-}
           {-| Planning Int-}
           {-| Reflex Int-}
           {-| TeamCoordination Int-}
           {-deriving (Show, Ord, Eq, Enum)-}

data Skills = Skills
    { aim              :: !Double
    , awareness        :: !Double
    , creativity       :: !Double
    , experience       :: !Double
    , levelheadedness  :: !Double
    , patience         :: !Double
    , planning         :: !Double
    , reflex           :: !Double
    , teamCoordination :: !Double
    } deriving (Show, Read)

skillsFromTraits (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) =
    Skills ai aw cr ex lh pa pl re tc
  where ai = (/) (emo + fms + pat) 3
        aw = (/) (atn + dsp + log + pat + rea) 5
        cr = (/) (emo + fea + log + pat) 4
        ex = (/) (cnf + pat + men + dsp) 4
        lh = (/) (atn + dtm + dsp + emo + fea + log + pat) 7
        pa = (/) (cnf + dtm + dsp + emo + fea + men + log) 7
        pl = (/) (atn + cth + emo + fea + log + pat) 6
        re = (/) (atn + dtm + fea + fms + pat + rea) 6
        tc = (/) (com + cnf + cth + dtm + dsp) 5



skillsToString :: Skills -> String
skillsToString (Skills ai aw cr ex lh pa pl re tc) = foldr (++) [] ss
  where ss = [ (colorWrap Yellow "Aim:               ") ++ (show $ round $ ai) ++ "%\n"
             , (colorWrap Yellow "Awareness:         ") ++ (show $ round $ aw) ++ "%\n"
             , (colorWrap Yellow "Creativity:        ") ++ (show $ round $ cr) ++ "%\n"
             , (colorWrap Yellow "Experience:        ") ++ (show $ round $ ex) ++ "%\n"
             , (colorWrap Yellow "Levelheadedness:   ") ++ (show $ round $ lh) ++ "%\n"
             , (colorWrap Yellow "Patience:          ") ++ (show $ round $ pa) ++ "%\n"
             , (colorWrap Yellow "Planning:          ") ++ (show $ round $ pl) ++ "%\n"
             , (colorWrap Yellow "Reflex:            ") ++ (show $ round $ re) ++ "%\n"
             , (colorWrap Yellow "Team Coordination: ") ++ (show $ round $ tc) ++ "%\n"
             ]
