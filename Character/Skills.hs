-- | TODO: Convert Skills data type into Skill,
--      with the fields as value constructors.

module Character.Skills
( Skills(..)
, skillsToString
) where

import TermColors

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
