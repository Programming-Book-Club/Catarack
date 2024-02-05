{-# LANGUAGE LambdaCase #-}

-- Deviating from the book here to parser combinators instead of recursive decent like I think the book wants
-- See https://www.youtube.com/watch?v=N9RUqGYuGfw for more details on how this is done
module Parser where

import Catarack_lib
import Control.Applicative
import Numeric.Natural

data ParseFailure
  = BadParse
  | EndOfInput
  | NoMatch
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Either ParseFailure (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Right (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', x) <- p2 input'
      Right (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Left BadParse
  (Parser p1) <|> (Parser p2) =
    Parser $ \input ->
      case (p1 input, p2 input) of
        (Left e1, Left _) -> Left e1
        (Right x, _) -> Right x
        (Left _, Right x) -> Right x

instance Semigroup (Parser a) where
  p1 <> p2 = p1 <|> p2

instance Monoid (Parser a) where
  mempty = Parser $ const (Left NoMatch)

-- Helper parsers
charParser :: Char -> Parser Char
charParser c =
  Parser $ \case
    y:t
      | y == c -> Right (t, c)
    [] -> Left EndOfInput
    _y:_t -> Left NoMatch

discard :: a -> Parser a -> Parser a
discard defaultCase p = p <|> pure defaultCase

wsParser :: Parser String
wsParser = some $ foldr ((<|>) . charParser) empty [' ', '\t', '\n']

stringParser :: String -> Parser String
stringParser = traverse charParser

lineParser :: String -> (String -> Catarack String) -> Parser (Catarack String)
lineParser prefix constr =
  Parser $ \input -> do
    (input', _) <- runParser (stringParser prefix) input
    (input'', _) <- runParser wsParser input'
    Right ("", constr input'')

hnParser :: Natural -> Parser (Catarack String)
hnParser n =
  lineParser (replicate ((fromIntegral . toInteger) n) '*') (Heading n)

-- Markup Parsers
blockLevelParsers :: [Parser (Catarack String)]
blockLevelParsers =
  [ headingParser
  , codeblockParser
  , unorderedParser
  , orderedParser
  , paragraphParser
  ]

headingParser :: Parser (Catarack String)
headingParser = foldr ((<|>) . hnParser) empty [1 .. 6]

codeblockParser :: Parser (Catarack String)
codeblockParser = lineParser ">" CodeBlock

unorderedParser :: Parser (Catarack String)
unorderedParser = lineParser "-" (\item -> UnorderedList [item])

orderedParser :: Parser (Catarack String)
orderedParser = lineParser "#" (\item -> OrderedList [item])

paragraphParser :: Parser (Catarack String)
paragraphParser = Parser $ \line -> Right ("", Paragraph (line ++ " "))

catarackLineParser :: Parser (Catarack String)
catarackLineParser = mconcat blockLevelParsers

catarackOfLines :: [String] -> Either ParseFailure (Catarack String)
catarackOfLines =
  fmap mconcat . fmap (map snd) . sequenceA . map (runParser catarackLineParser)
