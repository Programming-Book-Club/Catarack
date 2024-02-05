module Main where

import qualified Catarack_lib (someFunc)
import Html (Html, append_, h1_, makeHTML, p_, text_)

myhtml :: Html
myhtml =
  makeHTML
    "My Title!"
    (h1_ (text_ "Hello, World!") `append_` p_ (text_ "This is April!"))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Catarack_lib.someFunc
