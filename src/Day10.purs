module Day10 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (StateT, execStateT, get, modify_)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (elem)
import Data.Foldable (traverse_)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Parsing (Parser)
import Parsing.Combinators.Array (many)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, whiteSpace)
import Record as Record
import Type.Proxy (Proxy(..))
import Util (newline, parseInput)

data Instruction = AddX Int | Noop

instruction ∷ Parser String Instruction
instruction =
  ( AddX <$> (string "addx" *> whiteSpace *> intDecimal) <|> Noop <$ string
      "noop"
  ) <* newline

parse10 ∷ Parser String (Array Instruction)
parse10 = many instruction

type W = { answer ∷ Additive Int, help ∷ Array { register ∷ Int, cycle ∷ Int } }

solve'
  ∷ Array Instruction
  → StateT { inst ∷ Int, x ∷ Int } (Writer W) Unit
solve' = traverse_ fn
  where
  maybeLog = do
    { inst, x } ← get
    when ((inst + 1) `elem` [ 20, 60, 100, 140, 180, 220 ]) do
      tell
        { answer: Additive ((inst + 1) * x)
        , help: [ { register: x, cycle: inst } ]
        }

  fn ∷ Instruction → StateT { inst ∷ Int, x ∷ Int } (Writer W) Unit
  fn = case _ of
    Noop → modify_ (Record.modify (Proxy ∷ _ "inst") (_ + 1)) *> maybeLog
    AddX num → do
      modify_ (Record.modify (Proxy ∷ _ "inst") (_ + 1))
      maybeLog
      modify_ (Record.modify (Proxy ∷ _ "inst") (_ + 1))
      modify_ (Record.modify (Proxy ∷ _ "x") (_ + num))
      maybeLog

solve ∷ Array Instruction → W
solve = solve' >>> flip execStateT { inst: 0, x: 1 } >>> runWriter >>>
  Tuple.snd

main ∷ Effect Unit
main = launchAff_ do
  parseInput parse10 file <#> solve >>= logResult "Part 1"
  where
  logResult str result = log (str <> ": " <> show result)
  file = "input/10.txt"