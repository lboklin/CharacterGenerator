module Stat where

import Types
-- import Skill

data Factor = Major | Minor

type StatPoints = Int

newtype Dependency = Dependency (Stat, Factor)
-- newtype Dependencies = Dependencies [(Dependency, StatPoints)]
-- newtype SkillLevel = SkillLevel (Skill, StatPoints)

class Stat a  where
    mkStat :: a -> [Dependency] -> [b] -> StatPoints

instance Stat Skill where
    mkStat :: Skill -> [Dependency] -> [Trait] -> StatPoints
    mkStat s ds ts = round $ (+) avg $ (maAvg - miAvg) / 2.0
      where
        maAvg = (sum majors) / (fromIntegral $ length majors)
        miAvg = (sum minors) / (fromIntegral $ length minors)
        total = (sum majors) + (sum minors)
        count = maCount + miCount
        avg = total / count

-- | This is where the dependency math happens
-- TODO: Get help :|
skillLevelFromTraits :: Skill -> [Dependency] -> SkillLevel
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
