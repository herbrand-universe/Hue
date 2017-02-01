module Interactive.Repl.Parser(parseCommand) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

import Interactive.Repl.Command
import Format.Parser

-- Lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops   = ["="]
        names = [":quit", ":type", ":conv", ":load", ":undo", "proof", "var", "is","def", "import", ";"]
        style = emptyDef {Tok.reservedOpNames = ops,
                          Tok.reservedNames = names,
                          Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

numero :: Parser Integer 
numero  = Tok.integer lexer

filepath :: Parser String
filepath = do s <- many1 (noneOf ";")
              return s


lexeme = Tok.lexeme lexer

-- Parser

quitC = do reserved ":quit"
           eof
           return Quit

typeC = do reserved ":type"
           t <- cocast
           eof
           return $ Type t

convC = do reserved ":conv"
           t1 <- cocast
           reservedOp "="
           t2 <- cocast
           eof
           return $ Conv t1 t2
           
proofC = do reserved "proof"
            id <- identifier
            reserved "is"
            t <- cocast
            reserved ";"
            eof
            return $ Proof id t
     
printC = do reserved ":print"
            t <- cocast
            eof
            return $ Print t

loadC = do reserved ":load"
           fp <- lexeme (many anyChar)
           eof
           return $ Load fp

defC = do reserved "def"
          id <- identifier
          reserved "is"
          t <- cocast
          reservedOp "="
          ty <- cocast
          reserved ";"
          eof
          return $ Def id t ty

varC = do reserved "var"
          id <- identifier
          reserved "is"
          t <- cocast
          reserved ";"
          eof
          return $ Var id t

importC = do reserved "import"
             fp <- filepath
             reserved ";"
             eof
             return $ Import fp


{- Comandos para PG -}
restartC = do reserved "pragma_restart"
              reserved ";"
              eof
              return $ PragmaRestart

undoC = do reserved "pragma_undo"
           n <- numero 
           reserved ";"
           eof
           return $ PragmaUndo n 

backC    = do reserved "pragma_back"
              reserved ";"
              eof
              return $ PragmaBack


pathC    = do reserved "pragma_path"
              fp <- filepath
              reserved ";"
              eof
              return $ PragmaPath fp




nopC = do eof
          return $ Nop
          
command =       quitC 
            <|> restartC 
            <|> undoC 
            <|> backC 
            <|> pathC 
            <|> loadC 
            <|> typeC 
            <|> convC 
            <|> proofC 
            <|> printC 
            <|> varC 
            <|> defC 
            <|> importC 
            <|> nopC

parseCommand l = parse command "<interactive>" l
