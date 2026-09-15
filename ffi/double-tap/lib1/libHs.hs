{-# Language ForeignFunctionInterface #-}
module Lib1.LibHs where

import Foreign.C.Types
import Foreign.Ptr

foreign export ccall lib1_execute :: CInt ->  Ptr CInt -> CSize -> CInt -> CInt

lib1_execute a _xs _count b = a + b
