module Html.Type where

import Numeric.Natural

data HtmlInlineElem a
  = Text a
  | ListElem a
  | Bold a
  | Italic a

type ListElem = HtmlInlineElem

data HtmlHeadingElem a
  = Head [HtmlHeadingElem a]
  | Title (HtmlInlineElem a)

data HtmlBodyElem a
  = Paragraph [HtmlInlineElem a]
  | Body (HtmlInlineElem a)
  | CodeBlock [HtmlInlineElem a]
  | UnorderedList [ListElem a]
  | OrderedList [ListElem a]
  | Heading Natural (HtmlInlineElem a)

data Html a
  = Document (HtmlHeadingElem a) (HtmlBodyElem a)
