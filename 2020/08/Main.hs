{-# language TypeApplications #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# options_ghc -Wall -Werror #-}
import Data.Array.MArray
import Control.Monad.State
import Data.Array.IO
import qualified Text.Read as Read
import qualified Text.ParserCombinators.ReadP as Read
import Data.Char
import Data.Foldable

main :: IO ()
main = do
  tp <- getInput
  (_, _, acc) <- runInterpreter tp interp
  print acc
  ascs <- getAssocs tp
  traverse_ (go tp) ascs
    where
    go :: Tape -> (Int, Instruction) -> IO ()
    go tp (idx, instr) = case instr of
      Acc{} -> pure ()
      Jmp n -> do
        writeArray tp idx $ Nop n
        (b, _, acc) <- runInterpreter tp interp
        when b $ print acc
        writeArray tp idx $ Jmp n
      Nop n -> do
        writeArray tp idx $ Jmp n
        (b, _, acc) <- runInterpreter tp interp
        when b $ print acc
        writeArray tp idx $ Nop n


type Tape = IOArray Int Instruction

data Instruction = Acc Int | Jmp Int | Nop Int

instance Read Instruction where
  readPrec = do
    instr <- Read.lift $ do
      instr <- Read.munch isAlpha
      Read.skipSpaces
      Read.optional $ Read.char '+'
      pure instr
    n <- Read.readPrec @Int
    pure $ case instr of
      "jmp" -> Jmp n
      "acc" -> Acc n
      "nop" -> Nop n
      _     -> error "unknown instruction"

instance Show Instruction where
  show = \case
    Acc n -> "acc " <> show n
    Jmp n -> "jmp " <> show n
    Nop n -> "nop " <> show n

getInput :: IO Tape
getInput = getContents >>= go
  where
  go :: String -> IO Tape
  go = mk . fmap (read @Instruction) . lines
  mk :: [] Instruction -> IO Tape
  mk xs = newListArray (0, pred $ length xs) xs

type S = (Tape, IOArray Int Bool, Int, Int)

type Interpreter a = StateT S IO a

runInterpreter :: Tape -> Interpreter Bool -> IO (Bool, Int, Int)
runInterpreter tp m = do
  bounds <- getBounds tp
  vs <- newListArray bounds $ repeat False
  (b, (_, _, pc, acc)) <- runStateT m (tp, vs, 0, 0)
  pure (b, pc, acc)

load :: Interpreter (Either Bool Instruction)
load = do
  (tp, vs, pc, acc) <- get
  (lo, hi) <- liftIO $ getBounds tp
  let outOfBounds = not $ lo <= pc && pc <= hi
  if
    | outOfBounds -> pure $ Left True
    | otherwise   -> do
        visited <- liftIO $ readArray vs pc
        if
          | visited     -> pure $ Left False
          | otherwise   -> do
            liftIO $ writeArray vs pc True
            put (tp, vs, succ pc, acc)
            instr <- liftIO $ readArray tp pc
            pure $ pure instr

interp :: Interpreter Bool
interp = do
  i <- load
  case i of
    Left b -> pure b
    Right (Acc n) -> accumulate n >> interp
    Right (Jmp n) -> jmp n >> interp
    Right Nop{} -> interp

jmp, accumulate :: Int -> Interpreter ()
-- The pred is a bit of a hack. Loading an instruction already
-- increments the program counter, so we correc the jump here.
jmp offset = modify $ \(tp, vs, pc, acc) -> (tp, vs, pred $ pc + offset, acc)

accumulate n = modify $ \(tp, vs, pc, acc) -> (tp, vs, pc, acc + n)
