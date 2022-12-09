module Day9 where

import Prelude

import Control.Apply (lift2)
import Data.Array (foldl, intercalate, length, nub, replicate)
import Data.Either (Either(..))
import Data.FastVect.Common (term)
import Data.FastVect.FastVect (Vect)
import Data.FastVect.FastVect as Vect
import Data.Foldable (fold, or)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (abs, signum)
import Data.Traversable (scanl)
import Data.Vector.Polymorphic (Vector2, (><))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.Combinators.Array (many)
import Parsing.String (char, parseErrorHuman)
import Parsing.String.Basic (intDecimal, whiteSpace)
import QualifiedDo.Alt as Alt
import QualifiedDo.Semigroup as S
import QualifiedDo.Semigroupoid as Compose
import Safe.Coerce (coerce)

type Pos = Vector2 (Additive Int)

-- parsing helpers
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

-- parsing
direction ∷ Parser String Pos
direction = Alt.do
  char 'R' $> coerce (1 >< 0)
  char 'L' $> coerce (-1 >< 0)
  char 'U' $> coerce (0 >< 1)
  char 'D' $> coerce (0 >< -1)

directionLine ∷ Parser String (Array Pos)
directionLine = ado
  dir ← direction
  whiteSpace
  num ← intDecimal
  optional newline
  in replicate num dir

inputParser ∷ Parser String (Array Pos)
inputParser = manyMonoid directionLine

-- calculation
tooFarAway ∷ Pos → Pos → Boolean
tooFarAway head tail = or $ lift2
  (\(Additive h) (Additive t) → abs (h - t) > 1)
  head
  tail

moveCloser ∷ Pos → Pos → Pos
moveCloser head tail = if tooFarAway head tail then move head tail else tail
  where
  move = lift2 \(Additive h) (Additive t) → Additive $ t + signum (h - t)

type State1 = { head ∷ Pos, tail ∷ Pos, visited ∷ Array Pos }

solvePart1 ∷ Array Pos → Int
solvePart1 = Compose.do
  flip foldl (mempty ∷ State1) \state movement → do
    let
      head = state.head <> movement
      tail = moveCloser head state.tail
    { head, tail, visited: state.visited <> [ tail ] }
  _.visited
  nub
  length

type State2 = { rope ∷ Vect 10 Pos, visited ∷ Array Pos }

solvePart2 ∷ Array Pos → Int
solvePart2 = Compose.do
  flip foldl (mempty ∷ State2) \state movement → do
    let
      ropeWithMovedHead = Vect.modify (term ∷ _ 0) (_ <> movement) state.rope
      rope = scanl moveCloser (Vect.head ropeWithMovedHead) ropeWithMovedHead
    { rope, visited: state.visited <> [ Vect.last rope ] }
  _.visited
  nub
  length

main ∷ Effect Unit
main = launchAff_ do
  parseInput inputParser "input/9.txt" <#> solvePart1 >>= logShow
  parseInput inputParser "input/9.txt" <#> solvePart2 >>= logShow
