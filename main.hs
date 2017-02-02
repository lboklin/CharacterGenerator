{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}
import System.Random
import Numeric.Decimal

main :: IO ()
main = putStrLn $ printRandomCharacter 1523

data Character = Character
    { fullName :: String
    , age :: Int
    , description :: String
    , traits :: Traits
    , skills :: Skills
    } deriving (Show, Read)

-- | As it stands now, this datatype is not used. See genFullName further down.
{-data Name = Name
{ firstName :: String
, nickName :: String
, lastName :: String
} deriving (Show)-}
data Traits = Traits
    { fearlessness :: Float
    , communication :: Float
    , determination :: Float
    , confidence :: Float
    , reactionQuickness :: Float
    , fineMotorSkills :: Float
    , criticalThinking :: Float
    , logicalReasoning :: Float
    , patternRecognition :: Float
    , attention :: Float
    , mentalEndurance :: Float
    , selfControl :: Float
    , emotionalStability :: Float
    } deriving (Show, Read)

data Skills = Skills
    { aim :: Float
    , levelHeadedness :: Float
    , creativity :: Float
    , reflex :: Float
    , teamCoordination :: Float
    , awareness :: Float
    , experience :: Float
    , planning :: Float
    , patience :: Float
    } deriving (Show, Read)

firstNames =
    [ "John"
    , "Jack"
    , "Robert"
    , "Bernard"
    , "Daniel"
    , "Sam"
    , "Nick"
    , "Fred"
    , "George"
    , "Ron"
    , "Luke"
    , "Han"
    ]

lastNames =
    [ "Shepard"
    , "O'Neill"
    , "Ford"
    , "Lowe"
    , "Jackson"
    , "Carter"
    , "Solo"
    , "Moonwalker"
    , "Weasel"
    , "Castley"
    ]

nickNames =
    [ "Ken"
    , "Ben"
    , "Cando"
    , "Feeler"
    , "Garn"
    , "Rufus"
    , "Abbott"
    , "Costello"
    , "Lick"
    , "Moonboy"
    , "Pilot"
    ]

-- | TODO: Have genFullName generate a Name instead of a String.
genFullName :: [String] -> [String] -> [String] -> Int -> String
genFullName fns nns lns seed = ns !! index
  where
    ns =
        [ a ++ ", " ++ b ++ ", " ++ c
        | a <- fns
        , b <- nns
        , c <- lns ]
    index = head . randomRs (1, length ns - 1) $ mkStdGen seed

-- | TODO: Make the values of the traits have limited deviation from 0.5
genTraits :: Int -> Traits
genTraits seed =
    Traits
    { fearlessness = v !! 0
    , communication = v !! 1
    , determination = v !! 2
    , confidence = v !! 3
    , reactionQuickness = v !! 4
    , fineMotorSkills = v !! 5
    , criticalThinking = v !! 6
    , logicalReasoning = v !! 7
    , patternRecognition = v !! 8
    , attention = v !! 9
    , mentalEndurance = v !! 10
    , selfControl = v !! 11
    , emotionalStability = v !! 12
    }
  where
    v = take 13 . randomRs (1, 99) $ mkStdGen seed

makeSkills :: Int -> Skills
makeSkills seed =
    Skills
    { aim = v !! 0
    , levelHeadedness = v !! 1
    , creativity = v !! 2
    , reflex = v !! 3
    , teamCoordination = v !! 4
    , awareness = v !! 5
    , experience = v !! 6
    , planning = v !! 7
    , patience = v !! 8
    }
  where
    v = take 9 . randomRs (1, 99) $ mkStdGen seed

-- | TODO: change sks from taking a seed to tts and have that make sense
{-genCharacter :: String -> Int -> Traits -> Int -> Character-}
{-genCharacter name age traits seed =-}
genCharacter :: Int -> Character
genCharacter seed =
    Character
    { fullName = gn
    , age = a
    , description = dsc
    , traits = tts
    , skills = sks
    }
  where
    gn = genFullName firstNames nickNames lastNames seed
    a = head . randomRs (13, 36) $ mkStdGen seed
    dsc = gn ++ " is a great person."
    tts = genTraits seed
    sks = genSkills seed

printRandomCharacter :: Int -> String
printRandomCharacter seed =
    foldr (++) "\n" [x ++ "\n" | x <- [n, a, d, s]]
  where
    char = genCharacter seed :: Character
    n = fullName char
    a = show $ age char
    d = description char
    s = show $ skills char
