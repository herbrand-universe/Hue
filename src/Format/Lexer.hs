module Format.Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Data.Char

-- Tokens

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["Π", "λ", ":", ".", "->", "/\\", "\\/", "¬"]
        names = ["Prop", "Type"]
        style = emptyDef {Tok.reservedOpNames = ops,
                          Tok.reservedNames = names,
                          Tok.commentLine = "#",
                          Tok.opStart = oneOf "Πλ:.-/\\¬",
                          Tok.opLetter = oneOf ">",
                          Tok.identStart = letter,
                          Tok.identLetter = letter <|> digit
                         }

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

comma :: Parser ()
comma = Tok.comma lexer >> return ()

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

allOf :: Parser a -> Parser a
allOf p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
