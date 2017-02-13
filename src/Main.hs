{- |
Module      :  Main
Description :  Generate a character with stats based on a seed argument.
Copyright   :  (c) Ludvig Böklin
License     :  LGPLv3

Maintainer  :  ludvig.boklin@protonmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}
module Main where

import TermColors
import Generator
import Character

main :: IO ()
main = do
        putStrLn $ colorWrap Gray "Enter a seed number: "
        seed <- getLine
        putStrLn $ (++) "\n" $ formattedCharacterSheet $ genCharacter $ read seed
