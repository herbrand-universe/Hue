module Core.RawContext where

import Core.Term
import Data.Tuple


type Context = [ (String, (Maybe Term, Type)) ]


dom :: Context -> [String]
dom ys = map fst ys

emptyC :: Context
emptyC = []

addVar :: Context -> String -> Type -> Maybe Context
addVar c n _ | (elem n (dom c)) = Nothing
addVar i n t                    = Just $ ((n,(Nothing,t)) : i)

getTy :: Context -> String -> Maybe Type 
getTy v n = do d  <- lookup n v
               return $ snd d

getDef :: Context -> String -> Maybe Term 
getDef v n = do d <- lookup n v
                fst d

addDef :: Context -> String -> (Term, Type) -> Maybe Context
addDef c n _ | (elem n (dom c))  = Nothing
addDef defs n (t,ty) = Just $ (n,(Just t,ty)) : defs

member :: Context -> String -> Bool
member v n =  maybe False (const True)  $ lookup n v
