module Character
( Character(..)
, characterToString
, Name(..)
, Fullname(..)
, fullnameToString
, Skills(..)
, skillsToString
, Traits(..)
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
          | Lastname String
          | Nickname String
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

-- | TODO: Convert Skills data type into Skill,
--      with the fields as value constructors.

data Skills = Skills
    { aim              :: !Int
    , levelHeadedness  :: !Int
    , creativity       :: !Int
    , reflex           :: !Int
    , teamCoordination :: !Int
    , awareness        :: !Int
    , experience       :: !Int
    , planning         :: !Int
    , patience         :: !Int
    } deriving (Show, Read)

skillsToString :: Skills -> String
skillsToString (Skills ai lh cr re tc aw ex pl pa) = foldr (++) [] ss
  where ss = [ (colorWrap Yellow "Aim:               ") ++ (show ai) ++ "\n"
             , (colorWrap Yellow "Levelheadedness:   ") ++ (show lh) ++ "\n"
             , (colorWrap Yellow "Creativity:        ") ++ (show cr) ++ "\n"
             , (colorWrap Yellow "Reflex:            ") ++ (show re) ++ "\n"
             , (colorWrap Yellow "Team Coordination: ") ++ (show tc) ++ "\n"
             , (colorWrap Yellow "Awareness:         ") ++ (show aw) ++ "\n"
             , (colorWrap Yellow "Experience:        ") ++ (show ex) ++ "\n"
             , (colorWrap Yellow "Planning:          ") ++ (show pl) ++ "\n"
             , (colorWrap Yellow "Patience:          ") ++ (show pa) ++ "\n"
             ]

-- | TODO: Convert Traits data type into Trait,
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

