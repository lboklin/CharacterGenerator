module Preferences where

import System.Random
import Character
import Types

genColor :: Int -> Color
genColor = toEnum

toAppreciation x
  | x >= (100 * 2) / 3 = High
  | x >= (100 / 3) = Medium
  | otherwise  = Low

-- data Subject
--   =  Color
--   |  Music
--   |  Food
--   |  Season
--   |  Tod
--   |  Game
--   |  Climate
--   |  Gender
--   |  Brand
--   |  LifeStyle
--   |  Day
--   |  Holiday

-- data Preference = Preference Subject Appreciation

data Preferences = Preferences
  { color     :: Preference
  , music     :: Preference
  , food      :: Preference
  , season    :: Preference
  , timeofday :: Preference
  , game      :: Preference
  , region    :: Preference
  , gender    :: Preference
  , brand     :: Preference
  , lifestyle :: Preference
  , day       :: Preference
  , holiday   :: Preference
  , gear      :: Preference
  }

musicGenres :: [String]
musicGenres = map ("Genre" ++) [1..20]

genRandElemFrom :: [String] -> Int -> String
genRandElemFrom xs seed = (!!) xs $ head $ randomRs (0, ln) $ mkStdGen seed
  where
    ln = length xs

foodCategories :: [String]
foodCategories = map ((++) "Category") [1..20]
genFoodCategories seed = (!!) foodCategories $ head $ randomRs (0, ln) $ mkStdGen seed
  where
    ln = length foodCategories

genPreferences :: Int -> Traits -> Preferences
genPreferences seed (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
  Preferences
  { color     = ColorPref (genColor $ rn !! 0) ap
  , music     = MusicPref (genRandElemFrom musicGenres seed) ap
  , food      = rn !! 0
  , season    = rn !! 0
  , timeofday = rn !! 0
  , game      = rn !! 0
  , region    = rn !! 0
  , gender    = rn !! 0
  , brand     = rn !! 0
  , lifestyle = rn !! 0
  , day       = rn !! 0
  , holiday   = rn !! 0
  , gear      = rn !! 0
  }
  where
    rn = take 13 . randomRs (1, 99) $ mkStdGen seed
    ap = toAppreciation $ rn !! 1
