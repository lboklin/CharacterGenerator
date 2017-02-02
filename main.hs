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

main :: IO ()
main = do
        putStr "Enter a seed number: "
        seed <- getLine
        putStrLn $ (++) "\n" $ printRandomCharacter $ read seed

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Gray
   deriving (Show, Read, Eq)

colorBegin color = "\ESC[" ++ c ++ "m\STX"
  where
    c = case color of Red     -> "31"
                      Green   -> "32"
                      Yellow  -> "33"
                      Blue    -> "34"
                      Magenta -> "35"
                      Cyan    -> "36"
                      Gray    -> "38"

colorEnd = "\ESC[m\STX"

color clr txt = (colorBegin clr) ++ txt ++ colorEnd

data Character = Character
    { fullName :: String
    , age :: Int
    , description :: String
    , traits :: Traits
    , skills :: Skills
    } deriving (Show, Read)

-- | As it stands now, this datatype is not used. See genNames further down.
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

firstNames = [ "John"
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
             , "Han" ]

lastNames = [ "Shepard"
            , "O'Neill"
            , "Ford"
            , "Lowe"
            , "Jackson"
            , "Carter"
            , "Solo"
            , "Moonwalker"
            , "Weasel"
            , "Castley" ]

nickNames = [ "Ken"
            , "Ben"
            , "Cando"
            , "Feeler"
            , "Garn"
            , "Rufus"
            , "Abbott"
            , "Costello"
            , "Lick"
            , "Moonboy"
            , "The Pilot" ]

-- | TODO: Have genNames generate a Name instead of a String.
genNames :: [String] -> [String] -> [String] -> Int -> String
genNames fns nns lns seed = ns !! index
  where
    index = head . randomRs (1, length ns - 1) $ mkStdGen seed
    ns = [ a ++ " \"" ++ b ++ "\" " ++ c
         | a <- fns , b <- nns , c <- lns
         ]

-- | TODO: Make the values of the traits have limited deviation from 0.5
genTraits :: Int -> Traits
genTraits seed = Traits { fearlessness = v !! 0
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
                        } where
                            v = take 13 . randomRs (1, 99) $ mkStdGen seed

genSkills :: Int -> Skills
genSkills seed = Skills { aim              = rns !! 0
                        , levelHeadedness  = rns !! 1
                        , creativity       = rns !! 2
                        , reflex           = rns !! 3
                        , teamCoordination = rns !! 4
                        , awareness        = rns !! 5
                        , experience       = rns !! 6
                        , planning         = rns !! 7
                        , patience         = rns !! 8
                        } where
                            rns = take 9 . randomRs (1, 99) $ mkStdGen seed

skillsToString :: Skills -> String
skillsToString (Skills ai lh cr re tc aw ex pl pa) = foldr (++) [] ss
  where ss = [ (color Yellow "Aim:               ") ++ (show ai) ++ "\n"
             , (color Yellow "Levelheadedness:   ") ++ (show lh) ++ "\n"
             , (color Yellow "Creativity:        ") ++ (show cr) ++ "\n"
             , (color Yellow "Reflex:            ") ++ (show re) ++ "\n"
             , (color Yellow "Team Coordination: ") ++ (show tc) ++ "\n"
             , (color Yellow "Awareness:         ") ++ (show aw) ++ "\n"
             , (color Yellow "Experience:        ") ++ (show ex) ++ "\n"
             , (color Yellow "Planning:          ") ++ (show pl) ++ "\n"
             , (color Yellow "Patience:          ") ++ (show pa) ++ "\n"
             ]

{-stringsToPrettyString :: [String] -> String-}
{-stringsToPrettyString [] = ""-}
{-stringsToPrettyString = foldr (++) []-}

-- | TODO: change sks from taking a seed to tts and have that make sense
{-genCharacter :: String -> Int -> Traits -> Int -> Character-}
{-genCharacter name age traits seed =-}
genCharacter :: Int -> Character
genCharacter seed = Character { fullName = gn
                              , age = ag
                              , description = dc
                              , traits = ts
                              , skills = ss
                              } where
                                  gn = genNames firstNames nickNames lastNames seed
                                  ag = head . randomRs (13, 36) $ mkStdGen seed
                                  dc = gn ++ " is a great person."
                                  ts = genTraits seed
                                  ss = genSkills seed

charSheetHeader :: String
charSheetHeader = color Red "Generated Character Info:\n"
charSheetSeparator = color Red "----------------------------\n"

printRandomCharacter :: Int -> String
printRandomCharacter seed = foldl (++) h [x ++ "\n" | x <- [n, a, d, s]]
  where
    cbc = colorBegin Cyan
    ce  = colorEnd
    cr  = color Red
    cy  = color Yellow
    cm  = color Magenta
    cc  = color Cyan
    c   = genCharacter seed :: Character
    p   = charSheetSeparator
    h   = cbc ++ p ++ p ++ charSheetHeader ++ p
    n   = (++) (cy "Name:        ") $ fullName c
    a   = (++) (cy "Age:         ") $ show $ age c
    d   = (++) (cy "Description: ") $ description c
    s   = (++) (p ++ p ++ cr ("Skills:\n" ++ p)) $ skillsToString $ skills c
