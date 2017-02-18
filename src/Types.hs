module Types where

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

data Appreciation
  = High
  | Medium
  | Low
  deriving (Show, Eq, Ord, Enum)

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

data Name
    = Firstname String
    | Lastname String
    | Nickname String
    deriving (Show, Eq, Ord)

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
