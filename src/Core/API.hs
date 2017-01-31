module Core.API (
  CoCT(..),
  SContext,
  subsVI,
  subs,
  existType,
  check,
  impl,
  toDeBruijn,
  typeOf,
  empty,
  addContext,
  safeAddDef,
  freeId,
  getVarT,
  nameOfType,
  memberT,
  getPiT,
  lengthCtx,
  popCtxUntil,
  conv
) where

import Core.Term 
import qualified Core.Reduction as R
import qualified Core.TypeChecker as Ty
import Core.Context

getPiT :: SContext -> CoCT -> Maybe (CoCT,CoCT)
getPiT (SC c) t | (Ty.validT c t) = R.getPi c t
getPiT _      t                   = Nothing

typeOf :: SContext -> CoCT -> Maybe CoCT
typeOf (SC c) = Ty.typeof c

check :: SContext -> CoCT -> CoCT -> Bool
check (SC c) =  Ty.check c


conv :: SContext -> CoCT -> CoCT -> Bool
conv (SC c) = R.conv c
