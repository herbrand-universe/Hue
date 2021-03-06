module Interactive.Repl.Loop(repl) where

import System.Console.Haskeline hiding (handle)
import System.Exit
import Prelude hiding (read)
import System.IO
import System.IO.Error
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Exception (try)

import Interactive.Repl.Command
import Interactive.Repl.Parser
import Core.API
import Format.Pretty
import Interactive.Prover.Prover

read :: InputT (StateT SContext IO) Command
read = do  ctx <- lift get
           line <- getInputLine ("Huetop[" ++ (show $ lengthCtx ctx) ++ "]#")
           case line of
               Nothing -> return Quit
               Just l -> proc l
                              
proc :: String -> InputT (StateT SContext IO) Command
proc l = case parseCommand l of
                              Left err -> do outputStrLn ("[ERROR] " ++ show err)
                                             return Nop
                              Right cmd -> return cmd


eval :: Command -> InputT (StateT SContext IO) Bool
eval Quit        = return False
eval Nop         = return True
eval (Type ast) = do ctx <- lift get
                     outputStrLn (show (typeOf ctx (toDeBruijn ast)))
                     return True
eval (Conv ast ast') = do ctx <- lift get
                          outputStrLn (show (conv ctx (toDeBruijn ast) (toDeBruijn ast')))
                          return True
eval (Proof name ast) = do ctx <- lift get
                           replProver ctx name (toDeBruijn ast)
                           return True
eval (Print ast) = do outputStrLn (show (toDeBruijn ast))
                      return True
eval (Var s ast) = do ctx <- lift get
                      case addContext ctx s (toDeBruijn ast) of
                        Just c -> do outputStrLn (s ++ " is assumed.")
                                     lift $ put $ c
                        Nothing -> outputStrLn "[ERROR] Type error."
                      return True
eval (Def s ast ast') = do ctx <- lift get
                           case safeAddDef ctx s (toDeBruijn ast) (toDeBruijn ast') of
                             Just c  -> do outputStrLn (s ++ " is assumed.")
                                           lift $ put $ c
                             Nothing -> outputStrLn "[ERROR] Type error."
                           return True
eval (Load s) = do ctx <- lift get
                   ((), ctx') <- liftIO $ runStateT (runInputTBehavior (useFile s) defaultSettings loop) ctx
                   lift (put ctx')
                        --try (readFile s) >>= handleRead
                   -- mapM ((>>= eval) . proc) (lines c)
                   return True
eval (Import s) = do c <- liftIO $ readFile s
                     mapM ((>>= eval) . proc) (lines c)
                     return True
eval (PragmaUndo n) = do ctx <- lift get
                         case popCtxUntil ctx (fromInteger n)  of
                           Just c -> lift $ put $ c
                           Nothing -> outputStrLn "[ERROR] Type error."
                         return True

eval _ = return True

{-
serialEval [] = return []
serialEval (s:ss) = case proc s of
                      Proof name ast -> do ctx <- lift get
                                           res <- serialEvalProver ctx name (toDeBruijn ast) ss
                                           serialEval res
                      t -> do eval t
                              serialEval ss
-}
loop :: InputT (StateT SContext IO) ()
loop = do c <- read
          b <- eval c
          case b of
            True -> loop
            False -> return ()
            
-- TODO: Ver como tomar un path por argumento ... huetop -I <PATH>
pre = do c <- liftIO $ try (readFile "Logic.hue") >>= handleRead
         mapM ((>>= eval) . proc) (lines c)
         loop

handleRead :: Either IOError String -> IO String
handleRead (Right v) = return v
handleRead (Left e)  = putStrLn ("[ERROR] IO: " ++ show (ioeGetFileName e)) >> return ""
  

repl = flip runStateT empty $ runInputT defaultSettings pre

