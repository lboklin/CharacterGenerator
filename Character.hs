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

import System.Random
import TermColors

type SkillPoint = Int
type TraitPoint = Double
type Majors     = Minors
type Minors     = [(Trait, SkillPoint)]

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
  where maFactor = 1.5 -- Help, I math unwell
        maCount  = fromIntegral $ length majors
        miCount  = fromIntegral $ length minors
        maTot    = foldr (+) 0 majors
        miTot    = foldr (+) 0 minors
        maAvg    = maTot / maCount
        miAvg    = miTot / miCount
        total    = maTot + miTot
        count    = maCount + miCount
        dev      = avg - 50.0
        avg      = total / count

skillsFromTraits :: Traits -> Skills
skillsFromTraits (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) =
    Skills ai aw cr ex lh pa pl re tc
  where ai = skillPointFromDeps [ fms]                     [ emo, pat]
        aw = skillPointFromDeps [ atn, rea, pat]           [ dsp, log]
        cr = skillPointFromDeps [ emo]                     [ fea, log, pat]
        ex = skillPointFromDeps [ pat, dsp]                [ cnf, men]
        lh = skillPointFromDeps [ emo, fea, log]           [ atn, dtm, dsp, pat]
        pa = skillPointFromDeps [ dtm, dsp, emo, fea, men] [ cnf, log]
        pl = skillPointFromDeps [ atn, emo, log, pat]      [ cth, fea]
        re = skillPointFromDeps [ atn, fea, fms, rea]      [ dtm, pat]
        tc = skillPointFromDeps [ com, cnf, dtm, dsp]      [ cth]

valueBar :: Color -> SkillPoint -> String
valueBar c n = filledF filledChar ++ nonfilledF nonfilledChar
  where filled        = n `div` 5
        nonfilled     = 20 - filled
        filledF       = formatWrap (c, c, Bold)
        nonfilledF    = formatWrap (Default, Cyan, None)
        filledChar    = take filled $ repeat $  '\\'
        nonfilledChar = take nonfilled $ repeat $  '\\'

formattedTraits :: Traits -> String
formattedTraits (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) =
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

formattedSkillDeps :: Traits -> Skill -> [String]
formattedSkillDeps (Traits atn com cnf cth dtm dsp emo fea fms log men pat rea) skill =
    case skill of Aim              -> frmt [fmsS] ++ [emoS, patS]
                  Awareness        -> frmt [atnS, reaS, patS] ++ [dspS, logS]
                  Creativity       -> frmt [emoS] ++ [feaS, logS, patS]
                  Experience       -> frmt [patS, dspS] ++ [cnfS, menS]
                  Levelheadedness  -> frmt [emoS, feaS, logS] ++ [atnS, dtmS, dspS, patS]
                  Patience         -> frmt [dtmS, dspS, emoS, feaS, menS] ++ [cnfS, logS]
                  Planning         -> frmt [atnS, emoS, logS, patS] ++ [cthS, feaS]
                  Reflex           -> frmt [atnS, feaS, fmsS, reaS] ++ [dtmS, patS]
                  TeamCoordination -> frmt [comS, cnfS, dtmS, dspS] ++ [cthS]
  where vb   = valueBar Blue
        c    = Blue
        frmt = map (formatWrap (c, Default, Bold))
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

formattedSkills :: Skills -> String
formattedSkills (Skills ai aw cr ex lh pa pl re tc) = foldr (++) [] ss
  where cw = colorWrap Cyan
        vb = valueBar Blue
        ss = [ (cw "Aim:               ") ++ (vb ai) ++ "\n"
             , (cw "Awareness:         ") ++ (vb aw) ++ "\n"
             , (cw "Creativity:        ") ++ (vb cr) ++ "\n"
             , (cw "Experience:        ") ++ (vb ex) ++ "\n"
             , (cw "Levelheadedness:   ") ++ (vb lh) ++ "\n"
             , (cw "Patience:          ") ++ (vb pa) ++ "\n"
             , (cw "Planning:          ") ++ (vb pl) ++ "\n"
             , (cw "Reflex:            ") ++ (vb re) ++ "\n"
             , (cw "Team Coordination: ") ++ (vb tc) ++ "\n"
             ]

formattedSkillsExtra :: Traits -> Skills -> String
formattedSkillsExtra traits (Skills ai aw cr ex lh pa pl re tc) = foldr (++) [] ss
  where cw = formatWrap (Yellow, Default, Bold)
        dp = formattedSkillDeps traits
        vb = valueBar Yellow
        fm = foldr (++) "" . map (formatWrap (Blue, Default, None))
        ss = [ (cw "Aim:               ") ++ (vb ai) ++ "\n"
             , fm (dp Aim)                ++ "\n"
             , (cw "Awareness:         ") ++ (vb aw) ++ "\n"
             , fm (dp Awareness)          ++ "\n"
             , (cw "Creativity:        ") ++ (vb cr) ++ "\n"
             , fm (dp Creativity)         ++ "\n"
             , (cw "Experience:        ") ++ (vb ex) ++ "\n"
             , fm (dp Experience)         ++ "\n"
             , (cw "Levelheadedness:   ") ++ (vb lh) ++ "\n"
             , fm (dp Levelheadedness)    ++ "\n"
             , (cw "Patience:          ") ++ (vb pa) ++ "\n"
             , fm (dp Patience)           ++ "\n"
             , (cw "Planning:          ") ++ (vb pl) ++ "\n"
             , fm (dp Planning)           ++ "\n"
             , (cw "Reflex:            ") ++ (vb re) ++ "\n"
             , fm (dp Reflex)             ++ "\n"
             , (cw "Team Coordination: ") ++ (vb tc) ++ "\n"
             , fm (dp TeamCoordination)   ++ "\n"
             ]

formattedFullname :: Fullname -> String
formattedFullname (Fullname fn ln nn) = foldr (++) "" ns
  where
    cy = colorWrap Yellow
    ns = [ (cy "Firstname:   ") ++ (show fn) ++ "\n"
         , (cy "Lastname:    ") ++ (show ln) ++ "\n"
         , (cy "Nickname:    ") ++ (show nn)
         ]

formattedCharacterSheet :: Character -> String
formattedCharacterSheet character =
    foldl (++) hd [x ++ "\n" | x <- [fn, nn, ln, a, d, s]]
  where fb  = formatWrap (Default, Default, Bold)
        cy  = colorWrap Yellow
        hd  = "\n" ++ (fb "Generated Character Info:\n") ++ "\n"
        ns  = fullname character
        sd  = formattedSkillsExtra (traits character) $ skills character
        sk  = formattedSkills $ skills character
        fn  = (++) (cy "Firstname:   ") $ firstname ns
        nn  = (++) (cy "Nickname:    ") $ nickname ns
        ln  = (++) (cy "Lastname:    ") $ lastname ns
        a   = (++) (cy "Age:         ") $ show $ age character
        d   = (++) (cy "Description: ") $ description character
        s   = (++) ("\n" ++ fb ("Skills:\n" ++ "\n")) $ sd
        t   = (++) ("\n" ++ fb ("Traits:\n" ++ "\n")) $ formattedTraits $ traits character
