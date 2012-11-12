{-# OPTIONS_GHC -fglasgow-exts #-}
module Tactics.Tactics(appTactic, runTactic) where
{-----------------------------------------------------------------------------
  Para crear una nueva tactica 'foo [args]' deberiamos:
   a) Agregar en Tactic el constructor Foo args

   b) Definir
       fooTac :: args -> Goal -> [Goal] 

         args   --> Son los argumentos, si tiene.
         Goal   --> Es el termino a demostrar y en que contexto
         [Goal] --> Son los subgoals que genera aplicar la tactica

   c) Agregar un caso en update para el constructor Foo.
------------------------------------------------------------------------------}

import Data.Maybe
import Control.Monad.Error
import Data.List

import Core.API
import Tactics.ProverState

addVar :: SContext -> String -> CoCT -> TacticState SContext
addVar c n s = case addContext c n s of 
                  Nothing -> fail "No se puede agregar la variable"
                  Just c  -> return c

isPi :: SContext -> Maybe CoCT -> TacticState (CoCT,CoCT)
isPi c Nothing  = fail "Variable no definida"
isPi c (Just t) = case getPiT c t of
                        Nothing     -> fail "No es una funcion"
                        Just (a,b)  -> return (a,b)


isImpl :: SContext -> Maybe CoCT -> TacticState (CoCT,CoCT)
isImpl c t = do (a,b) <- isPi c t 
                if freeId b 0 then
                   return (a,b)
                else
                   fail "No es una implicacion"


isConv :: SContext -> CoCT -> CoCT -> TacticState ()
isConv c s t = if conv c s t then return () else fail "Tipos no unifican"


{------------------------------------------------------------------------------
 Táctica: Intro
 Descripición:
 Regla:
   C |- ΠA.B    ==>  C,x:A |- B [0 <- x]    donde         x \not\in dom(C,A,B)

 Proof-Term
   [y:B]        ==>  λA.B[x <-0]
------------------------------------------------------------------------------}
introPrf :: String -> CoCT -> Proof
introPrf n s = (\p -> LamT s (subsVI (head p) n 0))

introTac :: String -> Goal -> TacticState ([Goal],Proof)
introTac n (c, p) = do  (s,t) <- isPi c (Just p)
                        c' <- addVar c n s
                        return ([(c', subs t 0 (VarT n))],introPrf n s)



{------------------------------------------------------------------------------
 Táctica: Assumption 
 Descripición:
 Regla:
   C |- A      si existe (x:A') \in C      donde                    conv A A'

 Proof-Term
   []        ==>  x:A' 
------------------------------------------------------------------------------}
assumptionPrf :: Goal -> Proof
assumptionPrf (c,t) _ =  VarT $fromJust $ nameOfType c t

assumptionTac :: Goal -> TacticState ([Goal],Proof)
assumptionTac (c,t) | (existType c t) = return ([],assumptionPrf (c,t))
assumptionTac _                       = fail "No se puede aplicar assumption"

{------------------------------------------------------------------------------
 Táctica: Apply
 Descripición:
 Regla:
   C, H:M |- A    M whnf Pi B A'   conv A A'      ==>  C,H:M |- B

 Proof-Term
   [x:B]        ==> H x
------------------------------------------------------------------------------}
applyPrf :: CoCT -> Proof
applyPrf t  p = AppT t (p!!0)

applyTac :: CoCT -> Goal -> TacticState ([Goal],Proof)
applyTac n  (c,t)  = do (a,b) <- isImpl c (typeOf c n)
                        isConv c b t
                        return ([(c,a)],applyPrf n)
                                                      


--applyWithTac :: CoCT -> CoCT -> Goal -> TacticState [Goal]
--applyWithTac n t (c,m)  = do (a,b) <- isPi c (typeOf c n)
--                             isConv c (subs b 0 t) m
--                             return [(c,a)]
--applyWithTac _ _ _                     = fail "No se puede aplicar apply"


exactPrf :: CoCT -> Proof
exactPrf t _ = t

exactTac :: CoCT -> Goal -> TacticState ([Goal],Proof)
exactTac a (g, t) | (check g a t) = return ([],exactPrf a)
exactTac _ _                      = fail "No se puede aplicar exact"

--admitTac :: Goal -> TacticState [Goal]
--admitTac _ = return []

{------------------------------------------------------------------------------
 Táctica: Split 
 Descripición:
 Regla:
   C |- A /\ B      ==>  C |- A     y    C |-B 

 Proof-Term
   [x:A,y:B]        ==> λC:Prop.λf:(A->B->C).f x y
------------------------------------------------------------------------------}
splitPrf :: CoCT -> CoCT -> Proof
splitPrf a b p = LamT PropT $ LamT (impl a (impl b (IdT 0))) $ AppT (AppT (IdT 0) (p!!0)) (p!!1)

splitTac :: Goal -> TacticState ([Goal],Proof)
splitTac (g, AppT (AppT (VarT "and") a) b) = return ([(g,a),(g,b)],splitPrf a b)
splitTac _                                 = fail "No se puede aplicar split"

{------------------------------------------------------------------------------
 Táctica: Left 
 Descripición:
 Regla:
   C |- A \/ B      ==>  C |- A 

 Proof-Term
   [x:A]            ==> λC:Prop.λf:(A -> C).λg:(B -> C). f x
------------------------------------------------------------------------------}
leftPrf :: CoCT -> CoCT -> Proof
leftPrf a b p = LamT PropT $ LamT (impl a (IdT 0)) $ LamT (impl b (IdT 1)) $ AppT (IdT 1) (p!!0)

leftTac :: Goal -> TacticState ([Goal], Proof)
leftTac (g, AppT (AppT (VarT "or") a)  b) = return ([(g,a)],leftPrf a b)
leftTac _                                 = fail "No se puede aplicar left"

{------------------------------------------------------------------------------
 Táctica: Right 
 Descripición:
 Regla:
   C |- A \/ B      ==>  C |- B 

 Proof-Term
   [x:B]            ==>  λC:Prop.λf:(A -> C).λg:(B -> C). g x 
------------------------------------------------------------------------------}
rightPrf :: CoCT -> CoCT -> Proof
rightPrf a b p = LamT PropT $ LamT (impl a (IdT 0)) $ LamT (impl b (IdT 1)) $ AppT (IdT 0) (p!!0)

rightTac :: Goal -> TacticState ([Goal],Proof)
rightTac (g, AppT (AppT (VarT "or") a)  b) = return ([(g,b)],rightPrf a b)
rightTac _                                 = fail "No se puede aplicar right"

--elimTac :: String -> Goal -> TacticState [Goal]
--elimTac n (g,t) | getVarT g n == Nothing  = fail "No existe el identificador"
--elimTac n (g,t)                           = elimTacAux (fromJust (getVarT g n)) (g,t)
 
--elimTacAux :: CoCT -> Goal -> TacticState [Goal] 
--elimTacAux (AppT (AppT (VarT "and") a) b) (g, t) = return [(g, impl a (impl b t))] 
--elimTacAux (AppT (AppT (VarT "or") a) b) (g, t)  = return [(g, impl a t),(g, impl b t)]


--unfoldTac :: String -> Goal -> TacticState [Goal]
--unfoldTac "not" (g,AppT (VarT "not") a) = return [(g,  PiT a (VarT "false"))]
--unfoldTac _  _                          = fail "No se puede aplicar unfold"

--absurdTac :: String -> Goal -> TacticState [Goal]
--absurdTac a (g,_) = return [(g,VarT a), (g, AppT (VarT "not") (VarT a))]

{------------------------------------------------------------------------------
 Táctica: Cut B
 Descripición: Chequear el tema bindings
 Regla:
   C |- A           ==>  C |- B -> A   y  C |- B 

 Proof-Term
   [x:B -> A,y:B]   ==>  (x y)
------------------------------------------------------------------------------}
cutPrf :: Proof 
cutPrf = (\p -> AppT (p!!0) (p!!1))

cutTac :: CoCT -> Goal -> TacticState ([Goal],Proof)
cutTac c (g,t) = return ([(g, impl c t), (g,c)],cutPrf)


update :: Tactic -> Goal -> TacticState ([Goal],Proof)
update (Intro (Just n))    = introTac n
update (Intro Nothing)     =  \g -> (fresh >>= (\n -> introTac n g))
update (Apply (n,Nothing)) = applyTac n
--update (Apply (n,Just t))  = applyWithTac n t
update (Exact n)           = exactTac n 
update Assumption          = assumptionTac
--update Admit               = admitTac
update Split               = splitTac
update LeftT               = leftTac 
update RightT              = rightTac
--update (Elim n)            = elimTac n
--update (Unfold n)          = unfoldTac n
--update (Absurd n)          = absurdTac n
update (Cut n)             = cutTac n

appTactic :: Tactic -> Tree -> TacticState Tree
appTactic tac t  = do g <- currentGoal t
                      as <- update tac g
                      return $ updateGoal t tac as

runTactic :: [String] -> TacticState a -> (Either String a, [String])
runTactic ss s = (runUnique (runErrorT (unT s)) ss)
