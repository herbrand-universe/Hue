module Core.TypeChecker where
{-----------------------------------------------------------------------------
   PTS of Calulus of Contruction:

   Sorts    : {Prop, Type}
   Axioms   : {Prop : Type}
   Pi-Rules : {(s1,s2,s2) | s1,s2 \in Sorts}

  Propiedades:
   * Full
   * Functional


  Bibliografia:
  [] 1993 - Checking Algorithms for Pure Type Systems
------------------------------------------------------------------------------}
import Data.List
import Data.Maybe
import Core.Term
import Core.RawContext
import Core.Reduction 

axioms :: [(CoCT,CoCT)]
axioms = [(PropT, TypeT)]

piRules = [(PropT, PropT, PropT), (TypeT, PropT, PropT), 
            (PropT, TypeT, TypeT), (TypeT, TypeT, TypeT)]


funAxioms :: CoCT -> Maybe CoCT
funAxioms PropT = Just TypeT
funAxioms _     = Nothing

funRules :: CoCT -> CoCT -> Maybe CoCT
funRules PropT PropT = Just PropT
funRules TypeT PropT = Just PropT
funRules PropT TypeT = Just TypeT
funRules TypeT TypeT = Just TypeT
funRules _     _     = Nothing






freshV :: Context -> [String] -> [String] -> Maybe String
freshV c (x:xs) v | (elem x v || elem x (dom c))   = freshV c xs v
freshV c (x:_)    _                            = Just x
freshV c _     _                               = Nothing

venum :: [String]
venum = map (('Y':) . show) [0..]




{-----------------------------------------------------------------------------
 validT C t:

  C |- t : s                                                 s \in {Prop,Type} 
-----------------------------------------------------------------------------}
validT c t = maybe False isSort (typeof c t) 


isSort :: CoCT -> Bool
isSort PropT  = True
isSort TypeT  = True
isSort _      = False


{-----------------------------------------------------------------------------
Judgement                                                          C |- T : ? 
-----------------------------------------------------------------------------}
typeof :: Context -> CoCT -> Maybe CoCT
typeof _ s   | isSort s = funAxioms s
typeof c (IdT n)        = fail "cannot be typed"
typeof c (VarT n)       = getTy c n

typeof c (AppT a b)     = do t1 <- typeof c a
                             (p,q) <- getPi c t1
                             p' <- typeof c b
                             if conv c p p' 
                               then return (app q b)
                               else fail "cannot be typed"


typeof c (LamT a b)     = do x <- freshV c venum (freeVars b)
                             t1 <- typeof c a
                             s1 <- getSort c t1 
                             c' <- addVar c x a
                             t2 <- typeof c' (app b (VarT x))
                             if not (isSort t2) || isJust (funAxioms t2) 
                               then return (PiT a (subsVI t2 x 0))
                               else fail "cannot be typed"

typeof c (PiT a b)      = do x  <- freshV c venum (freeVars b)
                             t1 <- typeof c a
                             s1 <- getSort c t1
                             c' <- addVar c x a
                             t2 <- typeof c' (app b (VarT x))
                             s2 <- getSort c' t2
                             funRules s1 s2

{-----------------------------------------------------------------------------
Judgement                                                          C |- N : M 

  C |- N : ?    C |- M : s       conv (? M)
 ------------------------------------------
                C |- N : M

-----------------------------------------------------------------------------}
check :: Context -> CoCT -> CoCT -> Bool
check g n m | (validT g m)  = maybe False (conv g m) (typeof g n)
check _ _ _                 = False

