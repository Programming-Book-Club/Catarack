module Html.Internal where

newtype Html = Html String
newtype HtmlElem = HtmlElem String

type Tag = String
type Title = String

text_ :: String -> HtmlElem
text_ = HtmlElem . escape

html_ :: HtmlElem -> Html
html_ page = Html $ eUnwrap (el "html" page)

body_ :: HtmlElem -> HtmlElem
body_ = el "body"

head_ :: HtmlElem -> HtmlElem
head_ = el "head"

title_ :: Title -> HtmlElem
title_ title = el "title" (HtmlElem title)

h1_ :: HtmlElem -> HtmlElem
h1_ = el "h1"

p_ :: HtmlElem -> HtmlElem
p_ = el "p"

makeHTML :: Title -> HtmlElem -> Html
makeHTML title body = html_ $ head_ (title_ title) `append_` body_ body
render :: Html -> String
render (Html page) = page

append_ :: HtmlElem -> HtmlElem -> HtmlElem
append_ (HtmlElem s1) (HtmlElem s2) = HtmlElem $ s1 <> s2

el :: Tag -> HtmlElem -> HtmlElem
el tag content =
  HtmlElem
    ("<" <> tag <> ">")
    `append_` content
    `append_` HtmlElem ("</" <> tag <> ">")

eUnwrap :: HtmlElem -> String
eUnwrap (HtmlElem s) = s

escape :: String -> String
escape =
  let predicate ch =
        case ch of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          c -> [c]
  in
    concatMap predicate
