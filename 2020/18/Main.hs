{-# language TypeApplications #-}
{-# language LambdaCase #-}
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP hiding (optional)
import Text.Read
import Control.Applicative
import Data.Foldable
import Control.Monad

main :: IO ()
main = do
  inp <- lines <$> getContents
  -- traverse_ (runParser @IO expr >=> print) inp
  exprs <- traverse (runParser @IO expr) inp
  traverse_ (print . eval) exprs
  putStrLn "---"
  print $ sum $ eval <$> exprs
  putStrLn "==="

runParser :: MonadFail m => ReadPrec e -> String -> m e
runParser p s =
  case reverse $ readP_to_S (readPrec_to_P (p <* lift skipSpaces) minPrec) s of
    ((e,""):_) -> pure e
    _          -> fail "Parse error"

data Expr = ExprBin Expr Bin Expr | ExprLit Lit deriving (Show)

newtype Lit = Lit Int deriving (Show)

data Bin = Mult | Add deriving (Show)

chainl1 :: ReadPrec a -> ReadPrec (a -> a -> a) -> ReadPrec a
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 <|> return x

expr :: ReadPrec Expr
expr = Main.chainl1 expr1 (parseOp mult)
  where
  par :: ReadPrec a -> ReadPrec a
  par p = do
    _ <- lift $ char '('
    e <- p
    _ <- lift $ char ')'
    pure e
  literal :: ReadPrec Lit
  literal = Lit <$> readPrec @Int
  mult = Mult <$ lift (char '*')
  add = Add <$ lift (char '+')
  parseOp p = do
    lift skipSpaces
    op <- p
    lift skipSpaces
    pure (\e0 e1 -> ExprBin e0 op e1)
  expr1 = Main.chainl1 expr2 (parseOp add)
  expr2 :: ReadPrec Expr
  expr2 = (ExprLit <$> literal) <|> par expr

eval :: Expr -> Int
eval = \case
  ExprBin e0 op e1 -> runOp op (eval e0) (eval e1)
  ExprLit (Lit n) -> n

runOp :: Bin -> Int -> Int -> Int
runOp = \case
  Mult -> (*)
  Add  -> (+)
