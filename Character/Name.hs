module Character.Name
( Name(..)
, Fullname(..)
, fullnameToString
) where

import TermColors

data Name = Firstname String
          | Lastname String
          | Nickname String
          deriving (Show, Eq, Ord)

data Fullname = Fullname
    { firstname :: !String
    , lastname  :: !String
    , nickname  :: !String
    } deriving (Show, Eq, Ord)

fullnameToString :: Fullname -> String
fullnameToString (Fullname fn ln nn) = foldr (++) "" ns
  where
    cy = colorWrap Yellow
    ns = [ (cy "Firstname:   ") ++ (show fn) ++ "\n"
         , (cy "Lastname:    ") ++ (show ln) ++ "\n"
         , (cy "Nickname:    ") ++ (show nn)
         ]
