module Format.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import Text.Parsec.Prim
import Prelude hiding (pi)

import Format.Lexer
import Format.AST

{-
Precedence table
----------------
Var
App L
¬ R
/\ L
\/ L
-> R
Lam Pi R
-}

cocast :: Parser CoCAST
cocast = try abst <|> imps

abst :: Parser CoCAST
abst = try lamP <|> lam <|> try piP <|> pi

lam :: Parser CoCAST
lam = do
  reservedOp "λ"
  (VarAST x) <- var
  reservedOp ":"
  m <- imps
  reservedOp "."
  e <- cocast
  return (LamAST x m e)

lamP :: Parser CoCAST
lamP = do
  reservedOp "λ"
  (VarAST x) <- var
  reservedOp "."
  e <- cocast
  return (LamAST x PropAST e)

pi :: Parser CoCAST
pi = do
  reservedOp "Π"
  (VarAST x) <- var
  reservedOp ":"
  m <- imps
  reservedOp "."
  e <- cocast
  return (PiAST x m e)

piP :: Parser CoCAST
piP = do
  reservedOp "Π"
  (VarAST x) <- var
  reservedOp "."
  e <- cocast
  return (PiAST x PropAST e)



imps :: Parser CoCAST
imps = ors `chainr1` do reservedOp "->"
                        return $ \m n -> PiAST "_" m n
         
ors :: Parser CoCAST
ors = ands `chainl1` do reservedOp "\\/"
                        return $ \m n -> AppAST (AppAST (VarAST "or") m) n
  
ands :: Parser CoCAST
ands = atom `chainl1` do reservedOp "/\\"
                         return $ \m n -> AppAST (AppAST (VarAST "and") m) n

nots :: Parser CoCAST
nots = do
  reservedOp "¬"
  m <- atom
  return $ AppAST (VarAST "not") m

atom :: Parser CoCAST
atom =  nots <|> apps

apps :: Parser CoCAST
apps = elems `chainl1` (return $ \m n -> AppAST m n)

elems :: Parser CoCAST
elems = try prop <|> try typeA <|> var <|> parens cocast

prop :: Parser CoCAST
prop = do
  reserved "Prop"
  return PropAST

typeA :: Parser CoCAST
typeA = do
  reserved "Type"
  return TypeAST

var :: Parser CoCAST
var = do
  x <- identifier
  return (VarAST x)

parseCoCAST :: String -> CoCAST
parseCoCAST t =
  case parse (allOf cocast) "" t of
    Left err -> error (show err)
    Right ast -> ast

