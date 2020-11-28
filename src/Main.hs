module Main where

import RIO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.Console.Haskeline
import RIO.Char (toLower)
import qualified RIO.Text as T
import Data.Text.Read (double)

main :: IO ()
main = runInputT defaultSettings loop 

loop :: InputT IO ()
loop = do
  input <- getInputLine "LISP> "
  case map toLower <$> input of
    Nothing -> return ()
    Just "quit" -> return ()
    Just "" -> loop
    Just input -> outputStrLn input >> loop

type Parser = Parsec Void Text

data Expr =
    Symbol Text
  | Number Double
  | Exprs [Expr]
  deriving (Show)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";;") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

run :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
run p input = runParser p "" input

expr :: Parser Expr
expr = do
  symbolOrNum <- lexeme $ takeWhile1P Nothing (/= ' ')
  pure $ case double symbolOrNum of
    Right (n, "") -> Number n
    _ -> Symbol symbolOrNum
  
