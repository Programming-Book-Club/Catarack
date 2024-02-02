module Main where

import qualified Catarack_lib (someFunc)

newtype Html = Html String

render :: Html -> String
render (Html page) = page

newtype HtmlElem = HtmlElem String

eUnwrap :: HtmlElem -> String
eUnwrap (HtmlElem s) = s

append_ :: HtmlElem -> HtmlElem -> HtmlElem
append_ (HtmlElem s1) (HtmlElem s2) = HtmlElem $ s1 <> s2

type Tag = String
type Title = String

el :: Tag -> HtmlElem -> HtmlElem
el tag content =
  HtmlElem ("<" <> tag <> ">") `append_` content `append_` HtmlElem ("</" <> tag <> ">")

html_ :: HtmlElem -> Html
html_ page = Html $ eUnwrap (el "html" page)

body_ :: HtmlElem -> HtmlElem
body_ = el "body"

head_ :: HtmlElem -> HtmlElem
head_ = el "head"

title_ :: Title -> HtmlElem
title_ title = el "title" (HtmlElem title)

p_ :: HtmlElem -> HtmlElem
p_ = el "p"

h1_ :: HtmlElem -> HtmlElem
h1_ = el "h1"

makeHTML :: Title -> HtmlElem -> Html
makeHTML title body = html_ $ head_ (title_ title) `append_` body_ body

myhtml :: Html
myhtml = makeHTML "My Title!" (HtmlElem "Hello, World!" `append_` HtmlElem "This is April!")

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Catarack_lib.someFunc
