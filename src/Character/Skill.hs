module Skill where

import Types
import Trait
import Stat

newtype SkillLevel = SkillLevel (Skill, StatPoints)

-- | This is where the dependency math happens
-- TODO: Get help :|
skillLevelFromTraits :: Skill -> Dependencies -> SkillLevel
skillLevelFromTraits majors minors = round $ (+) avg $ (maAvg - miAvg) / 2.0
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
    ai = skillLevelFromTraits [fms] [emo, pat]
    aw = skillLevelFromTraits [atn, rea, pat] [dsp, lgc]
    cr = skillLevelFromTraits [emo] [fea, lgc, pat]
    ex = skillLevelFromTraits [pat, dsp] [cnf, men]
    lh = skillLevelFromTraits [emo, fea, lgc] [atn, dtm, dsp, pat]
    pa = skillLevelFromTraits [dtm, dsp, emo, fea, men] [cnf, lgc]
    pl = skillLevelFromTraits [atn, emo, lgc, pat] [cth, fea]
    re = skillLevelFromTraits [atn, fea, fms, rea] [dtm, pat]
    tc = skillLevelFromTraits [com, cnf, dtm, dsp] [cth]

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


-- data SkillDeps = SkillDeps
--     { ai = skillLevelFromTraits [fms] [emo, pat]
--     , aw = skillLevelFromTraits [atn, rea, pat] [dsp, lgc]
--     , cr = skillLevelFromTraits [emo] [fea, lgc, pat]
--     , ex = skillLevelFromTraits [pat, dsp] [cnf, men]
--     , lh = skillLevelFromTraits [emo, fea, lgc] [atn, dtm, dsp, pat]
--     , pa = skillLevelFromTraits [dtm, dsp, emo, fea, men] [cnf, lgc]
--     , pl = skillLevelFromTraits [atn, emo, lgc, pat] [cth, fea]
--     , re = skillLevelFromTraits [atn, fea, fms, rea] [dtm, pat]
--     , tc = skillLevelFromTraits [com, cnf, dtm, dsp] [cth]
--     }

-- data SkillLevel
--     = Aim SkillPoint
--     | Awareness SkillPoint
--     | Creativity SkillPoint
--     | Experience SkillPoint
--     | Levelheadedness SkillPoint
--     | Patience SkillPoint
--     | Planning SkillPoint
--     | Reflex SkillPoint
--     | TeamCoordination SkillPoint

-- class Skill a where
--   skill :: [a] -> [a] -> b 

-- instance Show SkillLevel where
--   show (Aim x) = "Aim: " ++ show x ++ "%"
--   show (Awareness x) = "Awareness: " ++ show x ++ "%"
--   show (Creativity x) = "Creativity: " ++ show x ++ "%"
--   show (Experience x) = "Experience: " ++ show x ++ "%"
--   show (Levelheadedness x) = "Levelheadedness: " ++ show x ++ "%"
--   show (Patience x) = "Patience: " ++ show x ++ "%"
--   show (Planning x) = "Planning: " ++ show x ++ "%"
--   show (Reflex x) = "Reflex: " ++ show x ++ "%"
--   show (TeamCoordination x) = "Team Coordination: " ++ show x ++ "%"
