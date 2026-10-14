{-# Language ForeignFunctionInterface #-}
module Lib2Hs where

import Foreign.C.Types
import Foreign.Ptr

foreign export ccall lib2_parser_parse
  :: IO ()
