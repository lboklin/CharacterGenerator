module Preferences where

import Character
import System.Random

data Color
  = Red
  | Blue
  | Green
  | Yellow
  | Black
  | Gray
  | White
  | Magenta
  | Orange
  | Purple
  | Cyan
  | Viridian
  | Pink
  | Beige
  | Brown
  deriving (Show, Eq, Enum)

genColor :: Int -> Color
genColor x = toEnum x

data Appreciation
  = High
  | Medium
  | Low
  deriving (Show, Eq, Ord, Enum)

toAppreciation x
  | x >= (100 * 2) / 3 = High
  | x >= (100 / 3) = Medium
  | otherwise  = Low

data Season
  = Winter
  | Spring
  | Summer
  | Autumn
  deriving (Show, Eq, Enum)

data TimeOfDay
  = Morning
  | Midday
  | Afternoon
  | Evening
  | Night
  deriving (Show, Eq, Ord, Enum)

data Climate
  = Arctic
  | Subarctic
  | Temperate
  | Tropical
  | Desert
  | Alpine
  deriving (Show, Eq, Enum)

data Gender
  = Male
  | Female
  deriving (Show, Eq, Enum)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq, Enum)

data Holiday
  = TaxReturnDay
  | NoPantsDay
  | ValentinesDay

data Preference
  =  ColorPref      Color      Appreciation
  |  MusicPref      String     Appreciation
  |  FoodPref       String     Appreciation
  |  SeasonPref     Season     Appreciation
  |  TodPref        TimeOfDay  Appreciation
  |  GamePref       String     Appreciation
  |  ClimatePref    Climate    Appreciation
  |  GenderPref     Gender     Appreciation
  |  BrandPref      String     Appreciation
  |  LifeStylePref  String     Appreciation
  |  DayPref        Day        Appreciation
  |  HolidayPref    Holiday    Appreciation

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

musicGenre :: [String]
musicGenre = map ((++) "Genre") [1..20]

foodCategories :: [String]
foodCategories = map ((++) "Category") [1..20]

genPreferences :: Int -> Traits -> Preferences
genPreferences seed (Traits atn com cnf cth dtm dsp emo fea fms lgc men pat rea) =
  Preferences
  { color     = ColorPref (genColor $ rn !! 0) $ toAppreciation $ rn !! 1
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
