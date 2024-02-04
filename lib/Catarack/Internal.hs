module Catarack.Internal where
-- import Html
import Numeric.Natural

-- Yes, this being abstract can lead to wonky behavior, but having the ergonomics of
-- fmap and <> are worth the tradeoff of being able to have your paragraphs be ints
-- Though it does have the benefit of later being able to encode things like language for
-- code blocks
data Catarack a
  = Heading !Natural a
  | Paragraph !a
  | OrderedList ![a]
  | UnorderedList ![a]
  | CodeBlock !a
  | Document ![Catarack a]
  deriving (Show, Eq)

instance Functor Catarack where
  fmap f (Heading l h) = Heading l (f h)
  fmap f (Paragraph str) = Paragraph (f str)
  fmap f (OrderedList l) = OrderedList (map f l)
  fmap f (UnorderedList l) = UnorderedList (map f l)
  fmap f (CodeBlock str) = CodeBlock (f str)
  fmap f (Document d) = Document (map (fmap f) d)

instance Semigroup a => Semigroup (Catarack a) where
  (Heading n1 h1) <> (Heading n2 h2) = Document [Heading n1 h1, Heading n2 h2]
  (Paragraph p1) <> (Paragraph p2) = Paragraph (p1 <> p2)
  -- (Document ((Paragraph p1) : t)) <> (Paragraph p2) = Document ((Paragraph p1 <> Paragraph p2) : t)
  -- (Paragraph p1) <> (Document ((Paragraph p2) : t)) = Document ((Paragraph p1 <> Paragraph p2) : t)
  (OrderedList l1) <> (OrderedList l2) = OrderedList (l1 ++ l2)
  (UnorderedList l1) <> (UnorderedList l2) = UnorderedList (l1 ++ l2)
  (CodeBlock b1) <> (CodeBlock b2) = CodeBlock (b1 <> b2)
  (Document d1) <> (Document d2) = Document (d1 ++ d2)
  (Document d) <> el = Document (d ++ [el])
  el1 <> (Document (el2 : t)) = flatten $ Document ((el1 <> el2) : t)
  el1 <> el2 = Document [el1, el2]

instance Semigroup a => Monoid (Catarack a) where
  mempty = Document []

flatten :: Semigroup a => Catarack a -> Catarack a
flatten (Document d) = mconcat d
flatten x = x
