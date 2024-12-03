{-# language GHC2021, DerivingStrategies, StrictData, LambdaCase #-}
module Main (main) where

import Text.Parsec hiding (many, (<|>), State)
import Text.Parsec.Char
import Data.Functor.Identity
import Control.Applicative
import Data.Foldable
import Control.Monad.State.Strict

main :: IO ()
main = do
  contents <- getContents
  let Right instructions = parseInstruction contents
  print $ snd $ (`execState` (True, 0)) $ traverse_ interpret instructions

interpret :: Instr -> State (Bool, Int) ()
interpret = \case
  Mul xs -> do
    b <- gets fst
    when b $ putsVal (+ product xs)
  Dont -> putFlag False
  Do -> putFlag True

putFlag :: MonadState (a, b) m => a -> m ()
putFlag b = modify (\(_, n) -> (b, n))

putsVal :: MonadState (a, t) m => (t -> t) -> m ()
putsVal f = modify (\(b, n) -> (b, f n))

parseInstruction :: String -> Either ParseError [Instr]
parseInstruction = runParser parser () mempty

anythingBut :: Stream s m Char => ParsecT s u m end -> ParsecT s u m [Char]
anythingBut p = try $ manyTill anyChar (lookAhead p)

anythingThen :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
anythingThen p = anythingBut p *> p

parser :: ParsecT String u Identity [Instr]
parser = many $ anythingThen instr

instr :: Parsec String u Instr
instr = try mul <|> try dont <|> try ddo

dont :: ParsecT String u Identity Instr
dont = Dont <$ string "don't()"

ddo :: ParsecT String u Identity Instr
ddo = Do <$ string "do()"

data Instr = Mul [Int] | Dont | Do

deriving stock instance Show Instr

number :: Stream s Identity Char => Parsec s u Int
number = read <$> many1 digit

mul :: Stream s Identity Char => Parsec s u Instr
mul = do
  _ <- string "mul"
  _ <- char '('
  xs <- number `sepBy` char ','
  _ <- char ')'
  pure $ Mul xs
