module Core.Term(CoCT(..), toDeBruijn,impl,freeVars,subs,subsV,subsVI,upId,Term,Type,freeId,app) where

import Data.List

import Format.AST

data CoCT =  IdT Int 
           | VarT String 
           | LamT CoCT CoCT 
           | AppT CoCT CoCT 
           | PiT CoCT CoCT 
           | PropT 
           | TypeT deriving (Eq)

type Type = CoCT
type Term = CoCT

{------------------------------------------------------------------------------
    Introduce -> y luego reacomoda los indices
  impl A B  = A -> B
------------------------------------------------------------------------------}
impl :: CoCT -> CoCT -> CoCT
impl a b = PiT a b' where b' = upId 1 0 b

{------------------------------------------------------------------------------
    Reacomoda los indices mayores a 'n'. Útil cuando se introducen bindings en
  el medio de una expresión
------------------------------------------------------------------------------}
upId :: Int -> Int -> CoCT -> CoCT
upId k n (IdT m) | n <= m  = IdT (m + k)
upId k n (AppT a b)        = AppT (upId k n a) (upId k n b)
upId k n (LamT a b)        = LamT (upId k n a) (upId k (n + 1) b)
upId k n (PiT  a b)        = PiT  (upId k n a) (upId k (n + 1) b)
upId k n t                 = t

downId :: Int -> Int -> CoCT -> CoCT
downId k n (IdT m) | n <= m  = IdT (m - k)
downId k n (AppT a b)        = AppT (downId k n a) (downId k n b)
downId k n (LamT a b)        = LamT (downId k n a) (downId k (n + 1) b)
downId k n (PiT  a b)        = PiT  (downId k n a) (downId k (n + 1) b)
downId k n t                 = t

toDeBruijn' :: [String] -> CoCAST -> CoCT
toDeBruijn' ctx (VarAST n)     = maybe (VarT n) IdT (n `elemIndex` ctx)
toDeBruijn' ctx (LamAST x m e) = LamT (toDeBruijn' ctx m) (toDeBruijn' (x:ctx) e)
toDeBruijn' ctx (AppAST m n)   = AppT (toDeBruijn' ctx m) (toDeBruijn' ctx n)
toDeBruijn' ctx (PiAST x m e)  = PiT (toDeBruijn' ctx m) (toDeBruijn' (x:ctx) e)
toDeBruijn' ctx PropAST        = PropT
toDeBruijn' ctx TypeAST        = TypeT 

toDeBruijn :: CoCAST -> CoCT
toDeBruijn = toDeBruijn' []


freeVars :: CoCT -> [String]
freeVars (VarT n)   = [n]
freeVars (LamT s t) = freeVars s ++ freeVars t
freeVars (AppT s t) = freeVars s ++ freeVars t
freeVars (PiT s t)  = freeVars s ++ freeVars t
freeVars _          = []

freeId :: CoCT -> Int -> Bool
freeId PropT _             = True
freeId TypeT _             = True
freeId (VarT _) _          = True
freeId (IdT n) m | n == m  = False
freeId (IdT _) _           = True
freeId (AppT p q) n        = freeId p n && freeId q n
freeId (LamT p q) n        = freeId p n && freeId q (n + 1)
freeId (PiT p q) n         = freeId p n && freeId q (n + 1)

{------------------------------------------------------------------------------
    Sustitución de una variable por un termino cerrado.
  Se lee: 
    subsV t v t' = t [x <- t'] 
------------------------------------------------------------------------------}
subsV :: CoCT -> String -> CoCT -> CoCT
subsV (PropT) _ _    = PropT
subsV (IdT n) _ _    = IdT n
subsV (VarT n') n t  | n' == n   =  t
                     | otherwise = (VarT n')
subsV (AppT s t) n m = AppT (subsV s n m) (subsV t n m)
subsV (PiT s t) n m  = PiT  (subsV s n m) (subsV t n m)
subsV (LamT s t) n m = LamT (subsV s n m) (subsV t n m)


{------------------------------------------------------------------------------
  Sustitución de una variable por un indice.
  Se lee: 
    subsVI t v n = t [x <- 0]
------------------------------------------------------------------------------}
subsVI :: CoCT -> String -> Int -> CoCT
subsVI (TypeT) _ _    = TypeT
subsVI (PropT) _ _    = PropT
subsVI (IdT n) _ _    = IdT n
subsVI (VarT n') n t  | n' == n   =  IdT t
                      | otherwise = (VarT n')
subsVI (AppT s t) n m = AppT (subsVI s n m) (subsVI t n m)
subsVI (PiT s t) n m  = PiT  (subsVI s n m) (subsVI t n (m + 1))
subsVI (LamT s t) n m = LamT (subsVI s n m) (subsVI t n (m + 1))


{------------------------------------------------------------------------------
  Sustitución de indice por un termino.
  Se lee: 
    subs t n t' = t [0 <- t']
------------------------------------------------------------------------------}


app :: CoCT -> CoCT -> CoCT
app a t = downId 1 0  (asubs 1 a 0 t)

subs :: CoCT -> Int -> CoCT -> CoCT
subs = asubs 0 

asubs :: Int -> CoCT -> Int -> CoCT -> CoCT
asubs k (TypeT) _ _     = TypeT
asubs k (PropT) _ _     = PropT
asubs k (VarT n) _ _    = VarT n
asubs k (IdT n') n t    | n' == n   =  upId k 0 t
                        | otherwise = (IdT n')
asubs k (AppT s t) n m  = AppT (asubs k s n m) (asubs k t n m)
asubs k (PiT s t) n m   = PiT (asubs k s n m) (asubs (k + 1) t (n + 1) m)
asubs k (LamT  s t) n m = LamT (asubs k s n m) (asubs (k + 1) t (n + 1) m)

