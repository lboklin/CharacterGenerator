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
, Fullname(..)
, Traits(..)
, Skill(..)
, Skills
, formattedCharacterSheet
, skillsFromTraits
) where

import TermColors

type SkillPoint = Int
type TraitPoint = Double

data Character = Character
    { fullname    :: !Fullname
    , age         :: !Int
    , description :: !String
    , traits      :: !Traits
    , skills      :: !Skills
    } deriving (Show)

data Name
    = Firstname String
    | Lastname String
    | Nickname String
    deriving (Show, Eq, Ord)

data Fullname = Fullname
    { firstname :: !String
    , lastname  :: !String
    , nickname  :: !String
    } deriving (Show, Eq, Ord)

data Trait
    = Attention
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
    { attention          :: !TraitPoint
    , communication      :: !TraitPoint
    , confidence         :: !TraitPoint
    , criticalThinking   :: !TraitPoint
    , determination      :: !TraitPoint
    , discipline         :: !TraitPoint
    , emotionalStability :: !TraitPoint
    , fearlessness       :: !TraitPoint
    , fineMotorSkills    :: !TraitPoint
    , logicalReasoning   :: !TraitPoint
    , mentalEndurance    :: !TraitPoint
    , patternRecognition :: !TraitPoint
    , reactionQuickness  :: !TraitPoint
    } deriving (Show, Read)

data Skill
    = Aim
    | Awareness
    | Creativity
    | Experience
    | Levelheadedness
    | Patience
    | Planning
    | Reflex
    | TeamCoordination
    deriving (Show, Read, Enum, Eq, Ord)

data Skills = Skills
    { aim              :: !SkillPoint
    , awareness        :: !SkillPoint
    , creativity       :: !SkillPoint
    , experience       :: !SkillPoint
    , levelheadedness  :: !SkillPoint
    , patience         :: !SkillPoint
    , planning         :: !SkillPoint
    , reflex           :: !SkillPoint
    , teamCoordination :: !SkillPoint
    } deriving (Show, Read)

-- | This is where the dependency math happens
-- TODO: Get help :|
skillPointFromDeps :: [TraitPoint] -> [TraitPoint] -> SkillPoint
skillPointFromDeps majors minors = round $ (+) avg $ (maAvg - miAvg) / 2.0
  where
    maCount = fromIntegral $ length majors
    miCount = fromIntegral $ length minors
    maTot = sum majors
    miTot = sum minors
    maAvg = maTot / maCount
    miAvg = miTot / miCount
    total = maTot + miTot
    count = maCount + miCount
    avg = total / count

skillsFromTraits :: Traits -> Skills
skillsFromTraits (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
    Skills ai aw cr ex lh pa pl re tc
  where
    ai = skillPointFromDeps [fms] [emo, pat]
    aw = skillPointFromDeps [atn, rea, pat] [dsp, lgc]
    cr = skillPointFromDeps [emo] [fea, lgc, pat]
    ex = skillPointFromDeps [pat, dsp] [cnf, men]
    lh = skillPointFromDeps [emo, fea, lgc] [atn, dtm, dsp, pat]
    pa = skillPointFromDeps [dtm, dsp, emo, fea, men] [cnf, lgc]
    pl = skillPointFromDeps [atn, emo, lgc, pat] [cth, fea]
    re = skillPointFromDeps [atn, fea, fms, rea] [dtm, pat]
    tc = skillPointFromDeps [com, cnf, dtm, dsp] [cth]

valueBar :: Color -> SkillPoint -> String
valueBar c n = filledF filledChar ++ nonfilledF nonfilledChar
  where filled        = n `div` 5
        nonfilled     = 20 - filled
        filledF       = formatWrap (c, c, Bold)
        nonfilledF    = formatWrap (Default, Cyan, None)
        filledChar    = replicate filled '\\'
        nonfilledChar = replicate nonfilled '\\'

-- formattedTraits :: Traits -> String
-- formattedTraits (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
--     concat ss
--   where cw = colorWrap Red
--         vb = valueBar Blue
--         ss = [ cw "Attention:           " ++ vb (round atn) ++ "\n"
--              , cw "Communication:       " ++ vb (round com) ++ "\n"
--              , cw "Confidence:          " ++ vb (round cnf) ++ "\n"
--              , cw "CriticalThinking:    " ++ vb (round cth) ++ "\n"
--              , cw "Determination:       " ++ vb (round dtm) ++ "\n"
--              , cw "EmotionalStability:  " ++ vb (round dsp) ++ "\n"
--              , cw "FineMotorSkills:     " ++ vb (round emo) ++ "\n"
--              , cw "LogicalReasoning:    " ++ vb (round fea) ++ "\n"
--              , cw "MentalEndurance:     " ++ vb (round fms) ++ "\n"
--              , cw "PatternRecognition:  " ++ vb (round lgc) ++ "\n"
--              , cw "ReactionQuickness:   " ++ vb (round men) ++ "\n"
--              , cw "Discipline:          " ++ vb (round pat) ++ "\n"
--              , cw "Fearlessness:        " ++ vb (round rea) ++ "\n"
--              ]

formattedSkillDeps :: Traits -> Skill -> [String]
formattedSkillDeps (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) skill =
    case skill of Aim              -> frmt [fmsS] ++ [emoS, patS]
                  Awareness        -> frmt [atnS, reaS, patS] ++ [dspS, lgcS]
                  Creativity       -> frmt [emoS] ++ [feaS, lgcS, patS]
                  Experience       -> frmt [patS, dspS] ++ [cnfS, menS]
                  Levelheadedness  -> frmt [emoS, feaS, lgcS] ++ [atnS, dtmS, dspS, patS]
                  Patience         -> frmt [dtmS, dspS, emoS, feaS, menS] ++ [cnfS, lgcS]
                  Planning         -> frmt [atnS, emoS, lgcS, patS] ++ [cthS, feaS]
                  Reflex           -> frmt [atnS, feaS, fmsS, reaS] ++ [dtmS, patS]
                  TeamCoordination -> frmt [comS, cnfS, dtmS, dspS] ++ [cthS]
  where vb   = valueBar Blue
        c    = Blue
        frmt = map (formatWrap (c, Default, Bold))
        atnS = (++) "Attention          " $ vb (round atn) ++ "\n"
        comS = (++) "Communication      " $ vb (round com) ++ "\n"
        cnfS = (++) "Confidence         " $ vb (round cnf) ++ "\n"
        cthS = (++) "CriticalThinking   " $ vb (round cth) ++ "\n"
        dtmS = (++) "Determination      " $ vb (round dtm) ++ "\n"
        dspS = (++) "Discipline         " $ vb (round dsp) ++ "\n"
        emoS = (++) "EmotionalStability " $ vb (round emo) ++ "\n"
        feaS = (++) "Fearlessness       " $ vb (round fea) ++ "\n"
        fmsS = (++) "FineMotorSkills    " $ vb (round fms) ++ "\n"
        lgcS = (++) "LogicalReasoning   " $ vb (round lgc) ++ "\n"
        menS = (++) "MentalEndurance    " $ vb (round men) ++ "\n"
        patS = (++) "PatternRecognition " $ vb (round pat) ++ "\n"
        reaS = (++) "ReactionQuickness  " $ vb (round rea) ++ "\n"

-- formattedSkills :: Skills -> String
-- formattedSkills (Skills ai aw cr ex lh pa pl re tc) = concat ss
  -- where cw = colorWrap Cyan
        -- vb = valueBar Blue
        -- ss = [ cw "Aim:               " ++ vb (round ai) ++ "\n"
             -- , cw "Awareness:         " ++ vb (round aw) ++ "\n"
             -- , cw "Creativity:        " ++ vb (round cr) ++ "\n"
             -- , cw "Experience:        " ++ vb (round ex) ++ "\n"
             -- , cw "Levelheadedness:   " ++ vb (round lh) ++ "\n"
             -- , cw "Patience:          " ++ vb (round pa) ++ "\n"
             -- , cw "Planning:          " ++ vb (round pl) ++ "\n"
             -- , cw "Reflex:            " ++ vb (round re) ++ "\n"
             -- , cw "Team Coordination: " ++ vb (round tc) ++ "\n"
             -- ]

formattedSkillsExtra :: Traits -> Skills -> String
formattedSkillsExtra trts (Skills ai aw cr ex lh pa pl re tc) = concat s
  where
    a = formatWrap (Yellow, Default, Bold)
    b = valueBar Yellow
    c = concatMap (formatWrap (Blue, Default, None))
    s =
        [ a "Aim:               "                      ++ b ai ++ "\n"
        , c (formattedSkillDeps trts Aim)              ++         "\n"
        , a "Awareness:         "                      ++ b aw ++ "\n"
        , c (formattedSkillDeps trts Awareness)        ++         "\n"
        , a "Creativity:        "                      ++ b cr ++ "\n"
        , c (formattedSkillDeps trts Creativity)       ++         "\n"
        , a "Experience:        "                      ++ b ex ++ "\n"
        , c (formattedSkillDeps trts Experience)       ++         "\n"
        , a "Levelheadedness:   "                      ++ b lh ++ "\n"
        , c (formattedSkillDeps trts Levelheadedness)  ++         "\n"
        , a "Patience:          "                      ++ b pa ++ "\n"
        , c (formattedSkillDeps trts Patience)         ++         "\n"
        , a "Planning:          "                      ++ b pl ++ "\n"
        , c (formattedSkillDeps trts Planning)         ++         "\n"
        , a "Reflex:            "                      ++ b re ++ "\n"
        , c (formattedSkillDeps trts Reflex)           ++         "\n"
        , a "Team Coordination: "                      ++ b tc ++ "\n"
        , c (formattedSkillDeps trts TeamCoordination) ++         "\n"
        ]

formattedCharacterSheet :: Character -> String
formattedCharacterSheet character =
    foldl
        (++)
        hd
        [ x ++ "\n"
        | x <- [fn, nn, ln, a, d, s] 
        ]
  where
    fb = formatWrap (Default, Default, Bold)
    cy = colorWrap Yellow
    hd = "\n" ++ fb "Generated Character Info:\n" ++ "\n"
    ns = fullname character
    sd = formattedSkillsExtra (traits character) $ skills character
    -- sk = formattedSkills $ skills character
    fn = (++) (cy "Firstname:   ") $ firstname ns
    nn = (++) (cy "Nickname:    ") $ nickname ns
    ln = (++) (cy "Lastname:    ") $ lastname ns
    a  = (++) (cy "Age:         ") $ show $ age character
    d  = (++) (cy "Description: ") $ description character
    s  = (++) ("\n" ++ fb ("Skills:\n" ++ "\n")) sd
    -- t = (++) ("\n" ++ fb ("Traits:\n" ++ "\n")) $ formattedTraits $ traits character
