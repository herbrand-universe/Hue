module Interactive.Prover.Prover(replProver) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import System.Console.Haskeline
import Control.Monad.State.Strict
import Control.Monad.Trans

import Prelude hiding (read)

import Tactics.Tactics
import Tactics.ProverState
import Core.API
import Format.Parser

data ProverCommand = Tac Tactic | Qed | Kill | Nop

-- Lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where names = ["intro", "admit", "assumption", "apply", "exact",  "split", 
                 "left", "right", "elim","unfold","absurd", "cut" , "with",
                 "qed", "kill"]
        style = emptyDef {Tok.reservedNames = names,
                          Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

-- Parser

intronT = do reserved "intro"
             id <- identifier
             return $ Tac (Intro $ Just id)

introT = do reserved "intro"
            return $ Tac (Intro Nothing)

admitT = do reserved "admit"
            return $ Tac Admit

applynT = do reserved "apply"
             t <- cocast 
             return $ Tac (Apply (toDeBruijn t, Nothing))

applyT = do reserved "apply"
            t <- cocast
            reserved "with"
            t' <- cocast
            return $ Tac (Apply (toDeBruijn t,Just $ toDeBruijn t))
 
assumptionT = do reserved "assumption"
                 return $ Tac Assumption

exactT = do reserved "exact"
            t <- cocast
            return $ Tac (Exact (toDeBruijn t))

splitT = do reserved "split"
            return $ Tac Split

leftT= do reserved "left"
          return $ Tac LeftT 

rightT = do reserved "right"
            return $ Tac RightT

elimT = do reserved "elim"
           id <- identifier
           return $ Tac (Elim id)

unfoldT = do reserved "unfold"
             id <- identifier
             return $ Tac (Unfold id)

absurdT = do reserved "absurd"
             id <- identifier
             return $ Tac (Absurd id)

cutT = do reserved "cut"
          t <- cocast
          return $ Tac (Cut(toDeBruijn t))

killC = do reserved "kill"
           return Kill
           
qedC = do reserved "qed"
          return Qed
           
tactic = try intronT <|> introT <|> admitT <|> try applyT <|> applynT 
         <|> assumptionT <|> exactT <|> splitT  <|> leftT <|> rightT 
         <|> elimT <|> unfoldT <|> absurdT <|> cutT <|> qedC <|> killC

parseTactic = parse tactic "<prover>"

-- REPL

read :: InputT (StateT SContext IO) ProverCommand
read = do ctx <- lift get 
          line <- getInputLine ("Huetop[" ++ (show $ lengthCtx ctx) ++ "]#")
          case line of
               Nothing -> return Qed
               Just l -> proc l

proc :: String -> InputT (StateT SContext IO) ProverCommand
proc l = case parseTactic l of
              Left err -> do outputStrLn "[ERROR] when parsing tactic."
                             return Nop
              Right cmd -> return cmd
                                                                                          
{-
read :: InputT (StateT SContext IO) ProverCommand
read = do line <- getInputLine "# "
          case line of
               Nothing -> return Nop
               Just l  -> case parseTactic l of
                 Left err -> do outputStrLn "[ERROR] Error when parsing tactic."
                                return Nop
                 Right cmd -> return cmd
-}

freshNames = map (('H':) . show) [0..]


eval :: [String] -> Tree -> ProverCommand -> InputT (StateT SContext IO) Tree
eval ss s Nop = loop ss s
eval ss s Kill = return s
eval ss s Qed = do (outputStrLn $ "Proof-Term [" ++ (show $ proof s) ++ "]") 
                   ctx <- lift get
                   case safeAddDef ctx (getName s) (getType s) (proof s) of
                     Just c  -> do outputStrLn ((getName s) ++ " is assumed.")
                                   lift $ put $ c
                     Nothing -> outputStrLn "[ERROR] Proof-term error."
                   return s

eval ss s (Tac t) = let (it, ss') = runTactic ss (appTactic t s) in
                      case it of
                         Left err -> do outputStrLn $ "[ERROR] " ++ err
                                        loop ss s
                         Right s' -> do outputStrLn (show it)
                                        loop ss' s'
{-                                        
serialEvalProver ctx name t [] = return []
serialEvalProver ctx name t (s:ss) = case proc s of
                                       Qed -> return ss
-}
  
loop :: [String] -> Tree -> InputT (StateT SContext IO) Tree
loop ss s = do t <- read
               eval ss s t


replProver ctx name t = loop freshNames $ emptyTree ctx name t
