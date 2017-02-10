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
, Style(..)
, Format(..)
, colorWrap
, formatWrap
, formatBegin
, formatEnd
) where

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Gray | Default
           deriving (Show, Read, Eq, Enum)
data Style = None | Bold | Italics | Underline
           deriving (Show, Read, Eq, Enum)

data Format = Format (Color, Color, Style) deriving (Show)

color :: Color -> String
color c = case c of Red     -> "1"
                    Green   -> "2"
                    Yellow  -> "3"
                    Blue    -> "4"
                    Magenta -> "5"
                    Cyan    -> "6"
                    Gray    -> "8"
                    Default -> "9"

style :: Style -> String
style st = case st of None -> ""
                      Bold -> ";1"
                      Italics -> ";3"
                      Underline -> ";4"

colorWrap :: Color -> String -> String
colorWrap c s = formatBegin (c, Default, None) ++ s ++ formatEnd
{-colorWrap c = (.) ((++) formatEnd) $ formatBegin (c, Default, None)-}
{-colorWrap :: Color -> String-}
{-colorWrap c s = (++ formatEnd) . (++) (formatBegin (c, Default, None))-}

formatWrap :: (Color, Color, Style) -> String -> String
{-formatWrap f = (++ formatEnd) . (++) (formatBegin f)-}
formatWrap f s = formatBegin f ++ s ++ formatEnd

formatBegin :: (Color, Color, Style) -> String
formatBegin (c1, c2, c3) = "\ESC[" ++ fg ++ bg ++ st ++ "m\STX"
  where fg = ";3" ++ color c1
        bg = ";4" ++ color c2
        st = style c3

formatEnd :: String
formatEnd = "\ESC[m\STX"
