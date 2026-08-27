{-# Language CApiFFI #-}

foreign import capi "hello_c.h" hello :: IO ()

main :: IO ()
main = do
  putStrLn "hello from haskell!"
  hello
  putStrLn "back in haskell!"
