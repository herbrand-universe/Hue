module Core.Reduction (Atom(..),NTerm(..),conv,whnf,getPi,getSort) where

import Core.Term
import Core.RawContext

data Atom = AId Int | AVar String | AApp Atom CoCT | APi CoCT CoCT | AProp | AType
data NTerm = NLam CoCT CoCT | NAtom Atom 

{------------------------------------------------------------------------------
    Reduce a Weak Head Normal Form.
------------------------------------------------------------------------------}
whnf :: Context -> CoCT -> NTerm
whnf c (IdT n)    = NAtom (AId n)
whnf c (VarT s)   = maybe (NAtom (AVar s)) (whnf c) (getDef c s)
whnf c (PiT s t)  = NAtom (APi s t)
whnf c PropT      = NAtom (AProp)
whnf c TypeT      = NAtom (AType)
whnf c (LamT s t) = NLam s t
whnf c (AppT m n) = case whnf c m of 
			NLam _ t -> whnf c (app t n)  
			NAtom t  -> NAtom (AApp t n)


convA :: Context -> Atom -> Atom -> Bool
convA c AProp  AProp          = True
convA c AType  AType          = True
convA c (AId n) (AId m)       = n == m
convA c (AVar s) (AVar t)     = s == t
convA c (APi s t) (APi n m)   = (conv c s n) && (conv c t m)
convA c (AApp s t) (AApp n m) = (convA c s n) && (conv c t m)
convA c _ _                   = False

convT :: Context -> NTerm -> NTerm -> Bool
convT c (NLam s t) (NLam n m) = (conv c s n) && (conv c t m)
convT c (NAtom s) (NAtom t)   = convA c s t
convT c _ _                   = False


{------------------------------------------------------------------------------
    Equivalencia de tipos, por el momento es una equivalencia beta, delta

    Importante: Ambos terminos deben ser bien tipados, de lo contrario la
  reduccion puede no terminar.

    Deberíamos mostrar que vale
  *  M \equiv_{\beta,\delta} N  <=> conv M N
  teniendo en cuenta que conv, usa Weak Head Normal From y no Normal Form
------------------------------------------------------------------------------}
conv :: Context -> CoCT -> CoCT -> Bool
conv c s t = convT c (whnf c s) (whnf c t)

getPi :: Context -> CoCT -> Maybe (CoCT,CoCT)
getPi c t = case whnf c t of
              NAtom (APi m n) -> Just (m, n)
              _       -> Nothing

getSort :: Context -> CoCT -> Maybe CoCT
getSort c t = case whnf c t of
                NAtom AProp -> Just PropT
                NAtom AType -> Just TypeT
                _           -> Nothing
