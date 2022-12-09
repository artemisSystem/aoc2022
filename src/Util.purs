module Util where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Effect.Aff (Aff, error, throwError)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (char, parseErrorHuman)
import QualifiedDo.Semigroup as S

newline ∷ Parser String Unit
newline = void (char '\n')

manyMonoid ∷ ∀ s a. Monoid a ⇒ Parser s a → Parser s a
manyMonoid = many >>> map fold

parseInput ∷ ∀ a. Parser String a → String → Aff a
parseInput parser file = do
  string ← readTextFile UTF8 file
  case runParser string parser of
    Left err → throwError $ error S.do
      "Error parsing file: "
      file
      "\n"
      intercalate "\n" $ parseErrorHuman string 5 err
    Right a → pure a