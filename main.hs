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
        putStrLn $ color Gray "Enter a seed number: "
        seed <- getLine
        putStrLn $ (++) "\n" $ characterToString $ read seed

-------------------
-- data types -----

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Gray
   deriving (Show, Read, Eq)

data Character = Character
    { fullname    :: Fullname
    , age         :: Int
    , description :: String
    , traits      :: Traits
    , skills      :: Skills
    } deriving (Show)

data Fullname = Fullname
    { firstname :: String
    , lastname  :: String
    , nickname  :: String
    } deriving (Show, Eq, Ord)

data Name = Firstname String
          | Lastname String
          | Nickname String
          deriving (Show, Eq, Ord)

data Traits = Traits
    { fearlessness       :: Int
    , communication      :: Int
    , determination      :: Int
    , confidence         :: Int
    , reactionQuickness  :: Int
    , fineMotorSkills    :: Int
    , criticalThinking   :: Int
    , logicalReasoning   :: Int
    , patternRecognition :: Int
    , attention          :: Int
    , mentalEndurance    :: Int
    , selfControl        :: Int
    , emotionalStability :: Int
    } deriving (Show, Read)

data Skills = Skills
    { aim              :: Int
    , levelHeadedness  :: Int
    , creativity       :: Int
    , reflex           :: Int
    , teamCoordination :: Int
    , awareness        :: Int
    , experience       :: Int
    , planning         :: Int
    , patience         :: Int
    } deriving (Show, Read)

------------
-- Colors --

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

----------------
-- Some lists --

firstnames = [ "John"
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

lastnames = [ "Shepard"
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

nicknames = [ "Ken"
            , "Ben"
            , "Cando"
            , "Feeler"
            , "Garn"
            , "Rufus"
            , "Abbott"
            , "Costello"
            , "Lick"
            , "Moonboy"
            , "The Pilot"
            ]

--------------------
-- Make-presentable-output functions

fullnameToString :: Fullname -> String
fullnameToString (Fullname fn ln nn) = foldr (++) "" ns
  where
    cy = color Yellow
    ns = [ (cy "Firstname:   ") ++ (show fn) ++ "\n"
         , (cy "Lastname:    ") ++ (show ln) ++ "\n"
         , (cy "Nickname:    ") ++ (show nn)
         ]

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

characterToString :: Int -> String
characterToString seed = foldl (++) h [x ++ "\n" | x <- [n, a, d, s]]
  where
    cbc = colorBegin Cyan
    ce  = colorEnd
    cr  = color Red
    cy  = color Yellow
    cm  = color Magenta
    cc  = color Cyan
    c   = genCharacter seed :: Character
    p   = color Red "----------------------------\n"
    h   = cbc ++ p ++ p ++ (color Red "Generated Character Info:\n") ++ p
    ns  = fullname c
    fn  = (++) (cy "Firstname:   ") $ firstname ns
    ln  = (++) (cy "Lastname:    ") $ lastname ns
    nn  = (++) (cy "Nickname:    ") $ nickname ns
    n   = fn ++ "\n" ++ ln ++ "\n" ++ nn
    a   = (++) (cy "Age:         ") $ show $ age c
    d   = (++) (cy "Description: ") $ description c
    s   = (++) (p ++ p ++ cr ("Skills:\n" ++ p)) $ skillsToString $ skills c

-------------------------
-- Generator functions --

-- | TODO: Have gennames generate a Name instead of a String.
-- | This takes a list of names and outputs the Names of a character
genFullname :: [String] -> [String] -> [String] -> Int -> Fullname
genFullname fns lns nns seed = Fullname fn ln nn
  where
    index = head $ randomRs (1, length ns - 1) $ mkStdGen seed
    fn    = ns !! index !! 0
    ln    = ns !! index !! 1
    nn    = ns !! index !! 2
    ns    = [ [a, b, c]
            | a <- fns, b <- lns, c <- nns
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


-- | TODO: change sks from taking a seed to tts and have that make sense
{-genCharacter :: String -> Int -> Traits -> Int -> Character-}
{-genCharacter name age traits seed =-}
genCharacter :: Int -> Character
genCharacter seed = Character { fullname = ns
                              , age = ag
                              , description = dc
                              , traits = ts
                              , skills = ss
                              } where
                                  ns = genFullname firstnames lastnames nicknames seed
                                  ag = head . randomRs (13, 36) $ mkStdGen seed
                                  dc = (nickname ns) ++ " is a great person."
                                  ts = genTraits seed
                                  ss = genSkills seed
