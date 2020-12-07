{-# language TypeApplications #-}
{-# language LambdaCase #-}
import Data.Array.MArray
import Control.Monad.State
import Data.Array.IO
import System.Environment

main :: IO ()
main = do
  tp <- getInput
  getArguments >>= \case
    Nothing -> pure()
    Just (noun, verb) -> do
      writeArray tp 1 noun
      writeArray tp 2 verb
  a <- runInterpreter tp interp
  print a

getArguments :: IO (Maybe (Int, Int))
getArguments =
  fmap read <$> getArgs >>= \case
    (noun:verb:_) -> pure $ Just (noun, verb)
    _             -> pure Nothing

type Tape = IOArray Int Int

runInterpreter :: Tape -> Interpreter a -> IO Int
runInterpreter tp m = do
  (_, (tp', _)) <- runStateT m (tp, 0)
  readArray tp' 0

getInput :: IO Tape
getInput = getContents >>= go
  where
  go :: String -> IO Tape
  go = mk . fmap read . lines
  mk :: [] Int -> IO Tape
  mk xs = newListArray (0, pred $ length xs) xs

type S = (Tape, Int)

type Interpreter a = StateT S IO a

load :: Interpreter Int
load = do
  (tp, pc) <- get
  put (tp, succ pc)
  liftIO $ readArray tp pc

add, mult, interp, halt :: Interpreter ()
interp = do
  i <- load
  case i of
    1 -> add >> interp
    2 -> mult >> interp
    _ -> halt

bin :: (Int -> Int -> Int) -> Interpreter ()
bin op = do
  a <- fetch
  b <- fetch
  r <- load
  writeTape r (a `op` b)

add = bin (+)

mult = bin (*)

fetch :: Interpreter Int
fetch = do
  r <- load
  tp <- gets fst
  liftIO $ readArray tp r

halt = pure ()

writeTape :: Int -> Int -> Interpreter ()
writeTape i e = do
  tp <- gets fst
  liftIO $ writeArray @IOArray @_ @IO tp i e
