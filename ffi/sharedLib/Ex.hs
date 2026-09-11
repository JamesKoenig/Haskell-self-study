{-# Language ForeignFunctionInterface, OverloadedStrings #-}

module Example where

import Foreign.C.Types
import qualified Data.Text.IO as TIO

fooMatic :: IO ()
fooMatic = do
  TIO.putStrLn "hello world from HASKELL"

foreign export ccall fooMatic :: IO ()
