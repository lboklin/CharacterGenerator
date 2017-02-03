{- |
Module      :  TermColors
Description :  Generate a character with stats based on a seed argument.
Copyright   :  (c) Ludvig Böklin
License     :  LGPLv3

Maintainer  :  ludvig.boklin@protonmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module TermColors
( Color(..)
, colorWrap
, colorBegin
, colorEnd
) where

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Gray
           deriving (Show, Read, Eq, Enum)

colorWrap c = (++ colorEnd) . (++) (colorBegin c)
{-colorWrap clr txt = (colorBegin clr) ++ txt ++ colorEnd-}

colorBegin c = "\ESC[" ++ cCode ++ "m\STX"
  where
    cCode = case c of Red     -> "31"
                      Green   -> "32"
                      Yellow  -> "33"
                      Blue    -> "34"
                      Magenta -> "35"
                      Cyan    -> "36"
                      Gray    -> "38"

colorEnd = "\ESC[m\STX"
