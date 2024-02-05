module Html.Internal where

-- Type definintions
newtype Html =
  Html String

newtype HtmlElem =
  HtmlElem String

type Tag = String

type Title = String

-- Element level constructors
text_ :: String -> HtmlElem
text_ = HtmlElem . escape

textList_ :: [String] -> [HtmlElem]
textList_ = map text_

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

ul_ :: [HtmlElem] -> HtmlElem
ul_ = el "ul" . tagReducation "li"

ol_ :: [HtmlElem] -> HtmlElem
ol_ = el "ol" . tagReducation "li"

code_ :: String -> HtmlElem
code_ = el "pre" . text_

makeHTML :: Title -> HtmlElem -> Html
makeHTML title body = html_ $ head_ (title_ title) `append_` body_ body

-- Helper functions
render :: Html -> String
render (Html page) = page

preRender :: HtmlElem -> String
preRender (HtmlElem tag) = tag

append_ :: HtmlElem -> HtmlElem -> HtmlElem
append_ (HtmlElem s1) (HtmlElem s2) = HtmlElem $ s1 <> s2

unitHtml :: HtmlElem
unitHtml = text_ ""

tagReducation :: Tag -> [HtmlElem] -> HtmlElem
tagReducation tag = foldr (append_ . el tag) unitHtml

-- Internal utilities
el :: Tag -> HtmlElem -> HtmlElem
el tag content =
  HtmlElem ("<" <> tag <> ">")
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
   in concatMap predicate
