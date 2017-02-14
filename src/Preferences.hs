module Preferences
( genPreferences) where

import Character
import System.Random

data Appreciation = High | Medium | Low deriving (Show, Eq, Ord, Enum)
data Preference   = ColorPref String Appreciation
                  | MusicPref String Appreciation
                  | FoodPref String Appreciation
                  | SeasonPref Season Appreciation
                  | TodPref TimeOfDay Appreciation
                  | GamePref Game Appreciation
                  | RegionPref Region Appreciation
                  | GenderPref Gender Appreciation
                  | BrandPref String Appreciation
                  | LifeStylePref String Appreciation
                  | DayPref Day Appreciation
                  | HolidayPref Holiday Appreciation
                  | GearPref Gear Appreciation

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

colorList = []

musicList = []

foodList = []

genPreferences :: Int -> Traits -> Preferences
genPreferences seed (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
    Preferences { color     = Preference rn !! 0
                , music     = rn !! 0
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
