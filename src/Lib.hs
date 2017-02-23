{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where

import Foreign.C.String
import Foreign.C.Types
import Generator

foreign export ccall "c_newCharAsString"
  c_newCharAsString :: CInt -> IO CString

newCharAsString :: Integer -> String
newCharAsString = show . genCharacter . fromInteger

c_newCharAsString :: CInt -> IO CString
c_newCharAsString seed = do
  s <- newCString $ newCharAsString $ toInteger seed
  return s
