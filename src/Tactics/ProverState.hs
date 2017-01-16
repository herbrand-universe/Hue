{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tactics.ProverState(Tactic(..),Goal,Tree,TacticState,currentGoal,updateGoal,
                   emptyTree,runUnique,unT,fresh,Proof(..),proof,getType,getName) where

import Core.API
import Format.Pretty
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Except



data Tactic = Intro (Maybe String) 
            | Apply (CoCT, Maybe CoCT)
            | Exact CoCT
            | Assumption 
            | Split
            | Admit
            | LeftT
            | RightT
            | Elim String
            | Absurd String
            | Unfold String
            | Cut CoCT 
            deriving Show

type Goal = (SContext,CoCT)


data ZTree = Gen (SContext,CoCT,Tactic) Proof [ZTree] | Hole Int 
type Ctx = [(Int, (SContext,CoCT))]

data Tree = TA (Ctx,ZTree,[Int],String)

instance Show Tree where
  show (TA ([],_,_,_)) = "QED" 
  show (TA ((_,(g,a)):xs,_,_,_)) = show ((length xs)+1) ++ " subgoals\nContext:\n" ++ "----------\n" ++ show g ++ show a


 
newtype UniqueT m a = UniqueT (StateT [String] m a) deriving ( Monad, MonadTrans, MonadIO)

instance (Monad m) => Applicative (UniqueT m) where
    pure = return
    (<*>) = ap 

instance (Monad m) => Functor (UniqueT m) where
    fmap = liftM

newtype Unique a = Unique (UniqueT Identity a) deriving (Monad, MonadUnique)

instance Applicative (Unique) where
    pure = return
    (<*>) = ap

instance Functor (Unique) where
    fmap = liftM


class Monad m => MonadUnique m where
    fresh :: m String
     
instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do (x:xs) <- get
                         put xs
                         return x
 
runUniqueT (UniqueT s) ss = runStateT s ss
runUnique (Unique s) ss = runIdentity (runUniqueT s ss)

type ProverState = Unique Tree

newtype TacticState a = T { unT :: ExceptT String Unique a } deriving (Monad)

instance Applicative (TacticState) where
    pure = return
    (<*>) = ap

instance Functor (TacticState) where
    fmap = liftM


instance MonadUnique TacticState where
  fresh = T $ lift fresh


emptyTree :: SContext -> String -> CoCT -> Tree 
emptyTree g name a = TA ([(0,(g,a))],Hole 0,[1..],name)


currentGoal :: Tree -> TacticState Goal 
currentGoal (TA ([],_, _,_))   = fail "No hay goals abiertos"
currentGoal (TA (x:_, _ ,_,_)) = return (snd x)

updateNode :: ZTree -> Int -> [ZTree] -> Goal -> Tactic -> Proof -> ZTree
updateNode (Hole n) m ts (g,a) tac p | n == m  = Gen (g,a,tac) p ts
updateNode (Gen b p xs) m t a tac p' = Gen b p (map (\x -> updateNode x m t a tac p') xs)
updateNode t _ _ _  _ _ = t


updateGoal :: Tree -> Tactic -> ([Goal],Proof) -> Tree
updateGoal (TA (a:xs,t,id,n)) tac as = updateGoal' (fst a) (TA (xs,t,id,n)) tac as (snd a)



updateGoal' :: Int -> Tree -> Tactic -> ([Goal],Proof)-> Goal -> Tree 
updateGoal' n (TA (c,zt,id,s)) tac (as,p) a = let c'  = (zip id as) ++ c 
                                                  id' = drop (length as) id
                                                  ts  = map (\n-> Hole (fst n)) c'
                                                  zt' = updateNode zt n ts a tac p
                                              in  TA (c',zt',id',s)


-- Dos opciones:
--  a) Guardamos en el arbol la prueba en cada nodo
--  b) En funcion a que táctica se aplico en el nodo, generamos una prueba
--
--  To be continued
type Proof = ([CoCT] -> CoCT)


proof :: Tree  -> CoCT
proof (TA (_,t,_,_)) = getProof t

getProof :: ZTree -> CoCT
getProof (Gen _ p xs) = p (map getProof xs)

getType' :: ZTree -> CoCT
getType'  (Gen (_,p,_) _ _) = p

getType :: Tree -> CoCT
getType (TA (_,t,_,s)) = getType' t

getName :: Tree -> String
getName (TA (_,_,_,s)) = s
