module Core.Context(SContext(..),safeAddDef,existType,addContext,empty,memberT,getVarT,nameOfType) where

import Data.Tuple
import Core.Term
import Core.RawContext 
import Core.Reduction
import Core.TypeChecker
import Format.Pretty

data SContext = SC Context 

base = ["and","or","false"]

instance Show SContext where
  show (SC []) = "---------" ++ "\n"
  show (SC ((s,t):xs))  | elem s base = show (SC xs)
  show (SC ((s,t):xs))  = s ++ ":" ++ show (snd t) ++ "\n" ++ show (SC (xs))

empty :: SContext
empty = SC emptyC



getVarT :: SContext -> String -> Maybe CoCT
getVarT (SC v) n = getTy v n 

existType :: SContext -> CoCT -> Bool
existType (SC xs) t  = or $ map ((conv xs t) . snd .snd) xs

aux :: Context -> CoCT -> [(Bool,String)]
aux c t = map (\(n,(_,ty)) -> (conv c t ty,n)) c

nameOfType :: SContext -> CoCT -> Maybe String
nameOfType (SC xs) t = do n <- lookup True (aux xs t) 
                          return n


memberT :: SContext -> String -> Bool
memberT (SC c) n = member c n

{-----------------------------------------------------------------------------
safeAddDef: C n ty t                                                          
                                                                               
    C |- t : ty                                                               
  ------------------------                                                     
    C, (n,t,ty)  es valido                                  si n \not\in dom(C)
                                                                               
------------------------------------------------------------------------------}
safeAddDef :: SContext -> String -> CoCT -> CoCT -> Maybe SContext 
safeAddDef (SC c) n ty def | (check c def ty)      = do c' <- (addDef c n (def,ty))
                                                        return (SC c')
safeAddDef _ _ _ _                                 = Nothing

addContext :: SContext -> String -> CoCT -> Maybe SContext
addContext (SC c) n t | (validT c t) = addVar c n t >>= (\c' -> Just (SC c'))
addContext  _     _  _               = Nothing
--addContext (SC c) n t = maybe Nothing (\c' -> Just (SC c')) (addContextT c n t)


