module Format.Pretty where

import Text.PrettyPrint.HughesPJ

import Core.Term

{-
Precedence table
----------------
Var
App L
¬ R
/\ L
\/ L
-> R
Lam Pi R
-}
used' i TypeT = False
used' i PropT = False
used' i (IdT j) | i == j = True
used' i (IdT j) | i /= j = False
used' i (VarT _) = False
used' i (AppT s t) = used' i s || used' i t
used' i (LamT s t) = used' i s || used' (i+1) t
used' i (PiT s t) = used' i s || used' (i+1) t

used = used' 0

pparens True = parens
pparens False = id

pp :: Int -> [String] -> [String] -> CoCT -> Doc
pp i bs fs (IdT n) = text $ bs !! n
pp _ _ _ (VarT s) = text s
pp _ _ _ TypeT= text "Type"
pp _ _ _ PropT = text "Prop"
pp i bs (f:fs) (LamT s t) = pparens (i > 0) $
                            text "λ"
                            <> text f
                            <> text ":"
                            <> pp 1 bs (f:fs) s
                            <> text "."
                            <> pp 0 (f:bs) fs t
pp i bs (f:fs) (PiT s t) | used t = pparens (i > 0) $
                                    text "Π"
                                    <> text f
                                    <> text ":"
                                    <> pp 1 bs (f:fs) s
                                    <> text "."
                                    <> pp 0 (f:bs) fs t
pp i bs fs (PiT s t) = pparens (i > 1) $
                       pp 2 bs fs s
                       <+> text "->"
                       <+> pp 1 (undefined:bs) fs t
pp i bs fs (AppT (AppT (VarT "or") s) t) = pparens (i > 2) $ pp 2 bs fs s
                                            <+> text "\\/"
                                            <+> pp 2 bs fs t
pp i bs fs (AppT (AppT (VarT "and") s) t) = pparens (i > 3) $ pp 3 bs fs s
                                             <+> text "/\\"
                                             <+> pp 3 bs fs t
pp i bs fs (AppT (VarT "not") s) = pparens (i > 4) $ text "¬"
                                    <> pp 4 bs fs s
pp i bs fs (AppT s t) = pparens (i > 5) $
                        pp 5 bs fs s
                        <+> pp 6 bs fs t

{-
placeof :: CoCT -> Int
placeOf PropT = 0
placeOf (IdT _) = 0
placeOf (VarT _) = 0
placeOf (AppT _ _) = 1
placeOf (LamT _ _) = 2
placeOf (PiT _ _) = 2


dups [] = ["_"]
dups (x:xs) = x:x:xs

pp :: [String] -> [String] -> CoCT -> Doc
pp bs fs (IdT n) = text (bs !! n)
pp _ _ (VarT s) = text s
pp _ _ PropT = text "Prop"
pp bs (f:fs) (LamT s t) = text "λ"
                               <> text f
                               <> text ":"
                               <> ppNest 0 (f:bs) fs s
                               <> text "."
                               <> ppNest 2 (f:bs) fs t
pp bs (f:fs) (PiT s t) | used t = text "Π"
                                       <> text f
                                       <> text ":"
                                       <> ppNest 0 (f:bs) fs s
                                       <> text "."
                                       <> ppNest 2 (f:bs) fs t
pp bs (f:fs) (PiT s t) = ppNest 0 (dups bs) (f:fs) s
                              <+> text "->"
                              <+> ppNest 2 (dups bs) (f:fs) t
pp bs (f:fs) (AppT (AppT (VarT "$and") s) t) = parens $ ppNest 1 bs (f:fs) s
                                                   <+> text "/\\"
                                                   <+> ppNest 0 bs (f:fs) t
pp bs (f:fs) (AppT (AppT (VarT "$or") s) t) = parens $ ppNest 1 bs (f:fs) s
                                              <+> text "\\/"
                                              <+> ppNest 0 bs (f:fs) t
pp bs (f:fs) (AppT s t) = ppNest 1 bs (f:fs) s
                          <+> ppNest 0 bs (f:fs) t


ppNest :: Int -> [String] -> [String] -> CoCT -> Doc
ppNest p bsi fs t | placeOf t > p = parens (pp bsi fs t)
                  | otherwise = pp bsi fs t
-}                         
instance Show CoCT where
  show = render . pp 0 [] (map (("x" ++) . show) [0..])
