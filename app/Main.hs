module Main where

import qualified Catarack_lib (someFunc)

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

fullwrapBody :: String -> String
fullwrapBody context = html_ $ body_ context

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

makeHTML :: String -> String -> String
makeHTML title body = html_ $ head_ (title_ title) <> body_ body

myhtml :: String
myhtml = makeHTML "My title!" (h1_ "Hello, World!" <> p_ "This is April!")

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Catarack_lib.someFunc
