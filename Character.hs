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

import System.Random
import TermColors

data Character = Character
    { fullname    :: !Fullname
    , age         :: !Int
    , description :: !String
    , traits      :: !Traits
    , skills      :: !Skills
    } deriving (Show)

data Name = Firstname String
          | Lastname  String
          | Nickname  String
          deriving (Show, Eq, Ord)

data Fullname = Fullname
    { firstname :: !String
    , lastname  :: !String
    , nickname  :: !String
    } deriving (Show, Eq, Ord)

formattedFullname :: Fullname -> String
formattedFullname (Fullname fn ln nn) = foldr (++) "" ns
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

data Skill = Aim
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

skillDeps :: Traits -> Skill -> [String]
skillDeps (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) skill =
    case skill of Aim              -> [fmsS] ++ [emoS, patS]
                  Awareness        -> [atnS, reaS, patS] ++ [dspS, logS]
                  Creativity       -> [emoS] ++ [feaS, logS, patS]
                  Experience       -> [patS, dspS] ++ [cnfS, menS]
                  Levelheadedness  -> [emoS, feaS, logS] ++ [atnS, dtmS, dspS, patS]
                  Patience         -> [dtmS, dspS, emoS, feaS, menS] ++ [cnfS, logS]
                  Planning         -> [atnS, emoS, logS, patS] ++ [cthS, feaS]
                  Reflex           -> [atnS, feaS, fmsS, reaS] ++ [dtmS, patS]
                  TeamCoordination -> [comS, cnfS, dtmS, dspS] ++ [cthS]
  where vb   = valueBar Blue
        atnS = (++) "Attention          " $ (vb $ round atn) ++ "\n"
        comS = (++) "Communication      " $ (vb $ round com) ++ "\n"
        cnfS = (++) "Confidence         " $ (vb $ round cnf) ++ "\n"
        cthS = (++) "CriticalThinking   " $ (vb $ round cth) ++ "\n"
        dtmS = (++) "Determination      " $ (vb $ round dtm) ++ "\n"
        dspS = (++) "Discipline         " $ (vb $ round dsp) ++ "\n"
        emoS = (++) "EmotionalStability " $ (vb $ round emo) ++ "\n"
        feaS = (++) "Fearlessness       " $ (vb $ round fea) ++ "\n"
        fmsS = (++) "FineMotorSkills    " $ (vb $ round fms) ++ "\n"
        logS = (++) "LogicalReasoning   " $ (vb $ round log) ++ "\n"
        menS = (++) "MentalEndurance    " $ (vb $ round men) ++ "\n"
        patS = (++) "PatternRecognition " $ (vb $ round pat) ++ "\n"
        reaS = (++) "ReactionQuickness  " $ (vb $ round rea) ++ "\n"

-- | This is where the dependency math happens
-- TODO: Get help :|
majorMinor :: [Double] -> [Double] -> Double
majorMinor majors minors = (maTot + miTot) / (fromIntegral $ length (majors ++ minors))
  where maFactor = 1.5 -- Help, I math unwell
        maAdj = map (* maFactor) majors
        miAdj = map (/ maFactor) minors
        {-miAdj = minors-}
        maTot = foldr (+) 0 maAdj
        miTot = foldr (+) 0 miAdj

skillsFromTraits :: Traits -> Skills
skillsFromTraits (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) =
        Skills ai aw cr ex lh pa pl re tc
  where ai = majorMinor [ fms]                     [ emo, pat]
        aw = majorMinor [ atn, rea, pat]           [ dsp, log]
        cr = majorMinor [ emo]                     [ fea, log, pat]
        ex = majorMinor [ pat, dsp]                [ cnf, men]
        lh = majorMinor [ emo, fea, log]           [ atn, dtm, dsp, pat]
        pa = majorMinor [ dtm, dsp, emo, fea, men] [ cnf, log]
        pl = majorMinor [ atn, emo, log, pat]      [ cth, fea]
        re = majorMinor [ atn, fea, fms, rea]      [ dtm, pat]
        tc = majorMinor [ com, cnf, dtm, dsp]      [ cth]

valueBar :: Color -> Int -> String
valueBar c n = filledF filledChar ++ nonfilledF nonfilledChar
  where filled        = n `div` 5
        nonfilled     = 20 - filled
        filledF       = formatWrap (c, c, Bold)
        nonfilledF    = formatWrap (Default, Cyan, None)
        filledChar    = take filled $ repeat $  '\\'
        nonfilledChar = take nonfilled $ repeat $  '\\'

traitsToString :: Traits -> String
traitsToString (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) =
    foldr (++) [] ss
  where cw = colorWrap Red
        vb = valueBar Blue
        ss = [ (cw "Attention:           ") ++ (vb $ round atn) ++ "\n"
             , (cw "Communication:       ") ++ (vb $ round com) ++ "\n"
             , (cw "Confidence:          ") ++ (vb $ round cnf) ++ "\n"
             , (cw "CriticalThinking:    ") ++ (vb $ round cth) ++ "\n"
             , (cw "Determination:       ") ++ (vb $ round dtm) ++ "\n"
             , (cw "EmotionalStability:  ") ++ (vb $ round dsp) ++ "\n"
             , (cw "FineMotorSkills:     ") ++ (vb $ round emo) ++ "\n"
             , (cw "LogicalReasoning:    ") ++ (vb $ round fea) ++ "\n"
             , (cw "MentalEndurance:     ") ++ (vb $ round fms) ++ "\n"
             , (cw "PatternRecognition:  ") ++ (vb $ round log) ++ "\n"
             , (cw "ReactionQuickness:   ") ++ (vb $ round men) ++ "\n"
             , (cw "Discipline:          ") ++ (vb $ round pat) ++ "\n"
             , (cw "Fearlessness:        ") ++ (vb $ round rea) ++ "\n"
             ]

skillsToString :: Skills -> String
skillsToString (Skills ai aw cr ex lh pa pl re tc) = foldr (++) [] ss
  where cw = colorWrap Cyan
        vb = valueBar Blue
        ss = [ (cw "Aim:               ") ++ (vb $ round ai) ++ "\n"
             , (cw "Awareness:         ") ++ (vb $ round aw) ++ "\n"
             , (cw "Creativity:        ") ++ (vb $ round cr) ++ "\n"
             , (cw "Experience:        ") ++ (vb $ round ex) ++ "\n"
             , (cw "Levelheadedness:   ") ++ (vb $ round lh) ++ "\n"
             , (cw "Patience:          ") ++ (vb $ round pa) ++ "\n"
             , (cw "Planning:          ") ++ (vb $ round pl) ++ "\n"
             , (cw "Reflex:            ") ++ (vb $ round re) ++ "\n"
             , (cw "Team Coordination: ") ++ (vb $ round tc) ++ "\n"
             ]

skillsWithDepsToString :: Traits -> Skills -> String
skillsWithDepsToString traits (Skills ai aw cr ex lh pa pl re tc) = foldr (++) [] ss
  where cw = formatWrap (Yellow, Default, Bold)
        dp = skillDeps traits
        vb = valueBar Yellow
        fm = foldr (++) "" . map (formatWrap (Blue, Default, Italics))
        ss = [ (cw "Aim:               ") ++ (vb $ round ai) ++ "\n"
             , fm (dp Aim)                ++ "\n"
             , (cw "Awareness:         ") ++ (vb $ round aw) ++ "\n"
             , fm (dp Awareness)          ++ "\n"
             , (cw "Creativity:        ") ++ (vb $ round cr) ++ "\n"
             , fm (dp Creativity)         ++ "\n"
             , (cw "Experience:        ") ++ (vb $ round ex) ++ "\n"
             , fm (dp Experience)         ++ "\n"
             , (cw "Levelheadedness:   ") ++ (vb $ round lh) ++ "\n"
             , fm (dp Levelheadedness)    ++ "\n"
             , (cw "Patience:          ") ++ (vb $ round pa) ++ "\n"
             , fm (dp Patience)           ++ "\n"
             , (cw "Planning:          ") ++ (vb $ round pl) ++ "\n"
             , fm (dp Planning)           ++ "\n"
             , (cw "Reflex:            ") ++ (vb $ round re) ++ "\n"
             , fm (dp Reflex)             ++ "\n"
             , (cw "Team Coordination: ") ++ (vb $ round tc) ++ "\n"
             , fm (dp TeamCoordination)   ++ "\n"
             ]

formattedCharacterSheet :: Character -> String
formattedCharacterSheet character =
    foldl (++) hd [x ++ "\n" | x <- [fn, nn, ln, a, d, s]]
  where fb  = formatWrap (Default, Default, Bold)
        cy  = colorWrap Yellow
        hd  = "\n" ++ (fb "Generated Character Info:\n") ++ "\n"
        ns  = fullname character
        sd  = skillsWithDepsToString (traits character) $ skills character
        sk  = skillsToString $ skills character
        fn  = (++) (cy "Firstname:   ") $ firstname ns
        nn  = (++) (cy "Nickname:    ") $ nickname ns
        ln  = (++) (cy "Lastname:    ") $ lastname ns
        a   = (++) (cy "Age:         ") $ show $ age character
        d   = (++) (cy "Description: ") $ description character
        s   = (++) ("\n" ++ fb ("Skills:\n" ++ "\n")) $ sd
        t   = (++) ("\n" ++ fb ("Traits:\n" ++ "\n")) $ traitsToString $ traits character
