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
, skillDeps
, skillsFromTraits
) where

import TermColors

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

-- | This is where the dependency math happens
-- TODO: Get help :|
majorMinor :: [Double] -> [Double] -> Double
majorMinor majors minors =
    (maTot + miTot) / fromIntegral (length (majors ++ minors))
  where
    maFactor = 1.5 -- Help, I math unwell
    maAdj = map (* maFactor) majors
    miAdj = map (/ maFactor) minors
    {-miAdj = minors-}
    maTot = sum maAdj
    miTot = sum miAdj

skillsFromTraits :: Traits -> Skills
skillsFromTraits (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
    Skills ai aw cr ex lh pa pl re tc
  where
    ai = majorMinor [fms] [emo, pat]
    aw = majorMinor [atn, rea, pat] [dsp, lgc]
    cr = majorMinor [emo] [fea, lgc, pat]
    ex = majorMinor [pat, dsp] [cnf, men]
    lh = majorMinor [emo, fea, lgc] [atn, dtm, dsp, pat]
    pa = majorMinor [dtm, dsp, emo, fea, men] [cnf, lgc]
    pl = majorMinor [atn, emo, lgc, pat] [cth, fea]
    re = majorMinor [atn, fea, fms, rea] [dtm, pat]
    tc = majorMinor [com, cnf, dtm, dsp] [cth]

skillDeps :: Traits -> Skill -> [String]
skillDeps (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) skill =
    case skill of
        Aim              -> [fmsS]                         ++ [emoS, patS]
        Awareness        -> [atnS, reaS, patS]             ++ [dspS, lgcS]
        Creativity       -> [emoS]                         ++ [feaS, lgcS, patS]
        Experience       -> [patS, dspS]                   ++ [cnfS, menS]
        Levelheadedness  -> [emoS, feaS, lgcS]             ++ [atnS, dtmS, dspS, patS]
        Patience         -> [dtmS, dspS, emoS, feaS, menS] ++ [cnfS, lgcS]
        Planning         -> [atnS, emoS, lgcS, patS]       ++ [cthS, feaS]
        Reflex           -> [atnS, feaS, fmsS, reaS]       ++ [dtmS, patS]
        TeamCoordination -> [comS, cnfS, dtmS, dspS]       ++ [cthS]
  where
    vb = valueBar Blue
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

valueBar :: Color -> Int -> String
valueBar c n = filledF filledChar ++ nonfilledF nonfilledChar
  where filled        = n `div` 5
        nonfilled     = 20 - filled
        filledF       = formatWrap (c, c, Bold)
        nonfilledF    = formatWrap (Default, Cyan, None)
        filledChar    = replicate filled '\\'
        nonfilledChar = replicate nonfilled '\\'

-- traitsToString :: Traits -> String
-- traitsToString (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
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

-- skillsToString :: Skills -> String
-- skillsToString (Skills ai aw cr ex lh pa pl re tc) = concat ss
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

skillsWithDepsToString :: Traits -> Skills -> String
skillsWithDepsToString trts (Skills ai aw cr ex lh pa pl re tc) = concat s
  where
    a = formatWrap (Yellow, Default, Bold)
    b = valueBar Yellow
    c = concatMap (formatWrap (Blue, Default, Italics))
    s =
        [ a "Aim:               "             ++ b (round ai) ++ "\n"
        , c (skillDeps trts Aim)              ++                 "\n"
        , a "Awareness:         "             ++ b (round aw) ++ "\n"
        , c (skillDeps trts Awareness)        ++                 "\n"
        , a "Creativity:        "             ++ b (round cr) ++ "\n"
        , c (skillDeps trts Creativity)       ++                 "\n"
        , a "Experience:        "             ++ b (round ex) ++ "\n"
        , c (skillDeps trts Experience)       ++                 "\n"
        , a "Levelheadedness:   "             ++ b (round lh) ++ "\n"
        , c (skillDeps trts Levelheadedness)  ++                 "\n"
        , a "Patience:          "             ++ b (round pa) ++ "\n"
        , c (skillDeps trts Patience)         ++                 "\n"
        , a "Planning:          "             ++ b (round pl) ++ "\n"
        , c (skillDeps trts Planning)         ++                 "\n"
        , a "Reflex:            "             ++ b (round re) ++ "\n"
        , c (skillDeps trts Reflex)           ++                 "\n"
        , a "Team Coordination: "             ++ b (round tc) ++ "\n"
        , c (skillDeps trts TeamCoordination) ++                 "\n"
        ]

formattedCharacterSheet :: Character -> String
formattedCharacterSheet character =
    foldl
        (++)
        hd
        [ x ++ "\n"
        | x <- [fn, nn, ln, a, d, s] ]
  where
    fb = formatWrap (Default, Default, Bold)
    cy = colorWrap Yellow
    hd = "\n" ++ fb "Generated Character Info:\n" ++ "\n"
    ns = fullname character
    sd = skillsWithDepsToString (traits character) $ skills character
    -- sk = skillsToString $ skills character
    fn = (++) (cy "Firstname:   ") $ firstname ns
    nn = (++) (cy "Nickname:    ") $ nickname ns
    ln = (++) (cy "Lastname:    ") $ lastname ns
    a = (++) (cy "Age:         ") $ show $ age character
    d = (++) (cy "Description: ") $ description character
    s = (++) ("\n" ++ fb ("Skills:\n" ++ "\n")) sd
    -- t = (++) ("\n" ++ fb ("Traits:\n" ++ "\n")) $ traitsToString $ traits character
