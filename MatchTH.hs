{-# LANGUAGE TemplateHaskell #-}

module MatchTH ( makeExprQ, extractPatVars ) where

import           Data.List
import           Data.Map            (Map)
import           Data.Maybe
import           Language.Haskell.TH
import           Match
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
changeExp dct (AppE e1 e2) = AppE (changeExp dct e1) (changeExp dct e1)
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
changeExp dct (StaticE e) = StaticE (changeExp dct e)
changeExp dct (UnboundVarE name) = VarE (changeName dct name)
changeExp _ e = e

changeName :: Map String Name -> Name -> Name
changeName dct name = fromMaybe name $ nameBase name #! dct

extractPatVars :: Exp -> [String]
extractPatVars (AppE (ConE name) p)
  | name == 'PatVar = case p of (LitE (StringL s)) -> [s]
  | name == 'NotPat = extractPatVars p
  | name == 'LaterPat = extractPatVars p
extractPatVars (AppE (AppE (ConE name) p1) p2)
  | name == 'AndPat = extractPatVars p1 ++ extractPatVars p2
  | name == 'OrPat = extractPatVars p1 ++ extractPatVars p2
  | name == 'ConsPat = extractPatVars p1 ++ extractPatVars p2
  | name == 'JoinPat = extractPatVars p1 ++ extractPatVars p2
extractPatVars _               = []
