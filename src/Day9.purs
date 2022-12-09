module Day9 where

import Prelude

import Control.Apply (lift2)
import Data.Array (foldl, length, nub, replicate)
import Data.FastVect.Common (term)
import Data.FastVect.FastVect (Vect)
import Data.FastVect.FastVect as Vect
import Data.Foldable (or)
import Data.Monoid.Additive (Additive(..))
import Data.Ord (abs, signum)
import Data.Reflectable (class Reflectable)
import Data.Traversable (scanl)
import Data.Vector.Polymorphic (Vector2, (><))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Parsing (Parser)
import Parsing.Combinators (optional)
import Parsing.String (char)
import Parsing.String.Basic (intDecimal, whiteSpace)
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import QualifiedDo.Alt as Alt
import QualifiedDo.Semigroupoid as Compose
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Util (manyMonoid, newline, parseInput)

type Pos = Vector2 (Additive Int)

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

day9Parser ∷ Parser String (Array Pos)
day9Parser = manyMonoid directionLine

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

type State length = { rope ∷ Vect length Pos, visited ∷ Array Pos }

solve
  ∷ ∀ length
  . Reflectable length Int
  ⇒ Compare length 0 GT
  ⇒ Proxy length
  → Array Pos
  → Int
solve _ = Compose.do
  flip foldl (mempty ∷ State length) \state movement → do
    let
      ropeWithMovedHead = Vect.modify (term ∷ _ 0) (_ <> movement) state.rope
      rope = scanl moveCloser (Vect.head ropeWithMovedHead) ropeWithMovedHead
    { rope, visited: state.visited <> [ Vect.last rope ] }
  _.visited
  nub
  length

main ∷ Effect Unit
main = launchAff_ do
  parseInput day9Parser file <#> solve (Proxy ∷ _ 2) >>= logResult "Part 1"
  parseInput day9Parser file <#> solve (Proxy ∷ _ 10) >>= logResult "Part 2"
  parseInput day9Parser file <#> solve (Proxy ∷ _ 100)
    >>= logResult "Just for fun (100 knots)"
  where
  logResult str result = log (str <> ": " <> show result)
  file = "input/9.txt"
