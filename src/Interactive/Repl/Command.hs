module Interactive.Repl.Command where

import Format.AST

data Command = Quit 
             | Nop 
             | Type CoCAST 
             | Conv CoCAST CoCAST
             | Proof String CoCAST 
             | Print CoCAST 
             | Var String CoCAST 
             | Def String CoCAST CoCAST
             | Load FilePath
             | Import FilePath
             {- Directivas parar PG -}
             | PragmaRestart
             | PragmaBack
             | PragmaUndo Integer
             | PragmaPath FilePath
             deriving Show

