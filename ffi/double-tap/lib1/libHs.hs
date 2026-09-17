{-# Language ForeignFunctionInterface #-}
module Lib1.LibHs where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array (peekArray)

foreign export ccall lib1_execute
    :: CInt -> Ptr CInt -> CSize -> CInt -> IO CInt

lib1_execute a xs count b = do
  let dbg@[hsA,hsB] = fromEnum <$> [a,b]
      hsCount   = fromEnum count --nb CSize vs CInt

  hsXs <- (fmap . fmap) fromEnum $ peekArray hsCount xs

  pure . toEnum $ execute hsA hsXs hsCount hsB

execute :: Int -> [Int] -> Int -> Int -> Int
execute a xs ct b =
  foldr (+) (b*ct) xs
