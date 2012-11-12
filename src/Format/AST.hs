module Format.AST (CoCAST(..)) where

data CoCAST = VarAST String 
            | LamAST String CoCAST CoCAST 
            | AppAST CoCAST CoCAST 
            | PiAST String CoCAST CoCAST 
            | PropAST 
            | TypeAST deriving Show

