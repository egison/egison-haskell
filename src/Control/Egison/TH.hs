{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Control.Egison.TH (
  makeExprQ,
  extractPatVars,
  changePat,
               ) where

import           Control.Egison.Core
import           Data.List
import           Data.Map            (Map)
import           Data.Maybe
import           Language.Haskell.TH
import           Unsafe.Coerce
import           Useful.Dictionary

makeExprQ :: [String] -> Exp -> ExpQ
makeExprQ vars expr = do
  vars' <- mapM newName vars
  vars'' <- mapM (\s -> newName $ s ++ "'") vars
  lamE [listP $ map (\x -> conP 'Result [varP x]) vars']
    $ foldr (\(x, x') acc -> letE [valD (varP x') (normalB (appE (varE 'unsafeCoerce) (varE x))) []] acc) (return $ changeExp (dict $ zip vars vars'') expr) $ zip vars' vars''

changeExp :: Map String Name -> Exp -> Exp
changeExp dct (VarE name) = VarE $ changeName dct name
changeExp dct (AppE e1 e2) = AppE (changeExp dct e1) (changeExp dct e2)
-- changeExp dct (AppTypeE e t) = AppTypeE (changeExp dct e) t
changeExp dct (InfixE me1 e me2) = InfixE (fmap (changeExp dct) me1) e (fmap (changeExp dct) me2)
changeExp dct (UInfixE e1 e2 e3) = UInfixE (changeExp dct e1) (changeExp dct e2) (changeExp dct e3)
changeExp dct (ParensE e) = ParensE (changeExp dct e)
changeExp dct (LamE pats e) = LamE pats (changeExp dct e)
changeExp dct (TupE es) = TupE $ map (changeExp dct) es
changeExp dct (UnboxedTupE es) = UnboxedTupE $ map (changeExp dct) es
-- changeExp dct (UnboxedSumE e s1 s2) = UnboxedSumE (changeExp dct e) s1 s2
changeExp dct (CondE e1 e2 e3) = CondE (changeExp dct e1) (changeExp dct e2) (changeExp dct e3)
changeExp dct (MultiIfE es) = MultiIfE $ map (\(g, e) -> (g, changeExp dct e)) es
changeExp dct (LetE ds e) = LetE ds $ changeExp dct e
changeExp dct (CaseE e ms) = CaseE (changeExp dct e) ms
changeExp dct (ListE es) = ListE $ map (changeExp dct) es
changeExp dct (SigE e t) = SigE (changeExp dct e) t
changeExp dct (RecConE name fs) = RecConE (changeName dct name) fs
changeExp dct (RecUpdE e fs) = RecUpdE (changeExp dct e) fs
changeExp dct (StaticE e) = StaticE $ changeExp dct e
changeExp dct (UnboundVarE name) = VarE $ changeName dct name
changeExp _ e = e

changeName :: Map String Name -> Name -> Name
changeName dct name = fromMaybe name $ nameBase name #! dct

extractPatVars :: [Exp] -> [String] -> ([String], [Int])
extractPatVars [] vars = (vars, [])
extractPatVars (ParensE x:xs) vars = extractPatVars (x:xs) vars
extractPatVars (AppE (ConE name) p:xs) vars
  | nameBase name == "PatVar" = case p of (LitE (StringL s)) -> extractPatVars xs (vars ++ [s])
  | nameBase name == "ValuePat" = let (vs, ns) = extractPatVars xs vars in (vs, length vars:ns)
  | nameBase name == "NotPat" = extractPatVars (p:xs) vars
  | nameBase name == "LaterPat" =
      let (vs1, ns1) = extractPatVars xs vars in
      let (vs2, ns2) = extractPatVars [p] vs1 in (vs2, ns2 ++ ns1)
extractPatVars (AppE (AppE (ConE name) p1) p2:xs) vars
  | (nameBase name) `elem` ["AndPat", "OrPat", "ConsPat", "JoinPat"] = extractPatVars (p1:p2:xs) vars
extractPatVars (InfixE (Just (ConE name)) (VarE op) (Just p):xs) vars = extractPatVars (AppE (ConE name) p:xs) vars
extractPatVars (_:xs) vars = extractPatVars xs vars

changePat :: Exp -> [[String]] -> Q (Exp, [[String]])
changePat e@(AppE (ConE name) p) vs
  | name == 'ValuePat = do
      let (vars:varss) = vs
      vars' <- mapM newName vars
      vars'' <- mapM (\s -> newName $ s ++ "'") vars
      (, varss) <$> appE (conE 'ValuePat) (lamE [listP $ map (\x -> conP 'Result [varP x]) vars']
        $ foldr (\(x, x') acc -> letE [valD (varP x') (normalB (appE (varE 'unsafeCoerce) (varE x))) []] acc) (return $ changeExp (dict $ zip vars vars'') p) $ zip vars' vars'')
  | name == 'NotPat = do
      (e', vs') <- changePat p vs
      (, vs') <$> appE (conE name) (return e')
  | name == 'LaterPat = do
      (e', vs') <- changePat p vs
      (, vs') <$> appE (conE name) (return e')
  | otherwise = return (e, vs)
changePat e@(AppE (AppE (ConE name) p1) p2) vs
  | name `elem` ['AndPat, 'OrPat, 'ConsPat, 'JoinPat] = do
      (e1, vs1) <- changePat p1 vs
      (e2, vs2) <- changePat p2 vs1
      (, vs2) <$> appE (appE (conE name) (return e1)) (return e2)
  | otherwise = return (e, vs)
changePat (InfixE (Just (ConE name)) (VarE op) (Just p)) vs = changePat (AppE (ConE name) p) vs
changePat e vs = return (e, vs)
