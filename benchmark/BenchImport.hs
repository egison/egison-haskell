{-# OPTIONS_GHC -Wno-orphans #-}

module BenchImport
  ( parseEgison
  , evalEgison
  , EgisonExpr
  )
where

import           Language.Egison                ( initialEnv
                                                , evalEgisonExpr
                                                , EgisonValue(..)
                                                )
import           Language.Egison.Data           ( fromEgisonM )
import           Language.Egison.AST
import           Language.Egison.ParserNonS     ( readExpr )
import           Language.Egison.CmdOptions     ( defaultOption )

import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )

deriving instance Generic BinOpAssoc
instance NFData BinOpAssoc

deriving instance Generic Infix
instance NFData Infix

deriving instance Generic LoopRange
instance NFData LoopRange

deriving instance Generic Arg
instance NFData Arg

deriving instance Generic PMMode
instance NFData PMMode

deriving instance Generic PrimitivePatPattern
instance NFData PrimitivePatPattern

deriving instance Generic InnerExpr
instance NFData InnerExpr

deriving instance Generic PrimitiveDataPattern
instance NFData PrimitiveDataPattern

deriving instance Generic EgisonPattern
instance NFData EgisonPattern

instance NFData Var
instance NFData (Index ())
instance NFData (Index EgisonExpr)

deriving instance Generic EgisonExpr
instance NFData EgisonExpr

evalEgison :: EgisonExpr -> IO EgisonValue
evalEgison expr = do
  egisonEnv   <- initialEnv defaultOption
  Right value <- evalEgisonExpr egisonEnv expr
  pure value

parseEgison :: String -> IO EgisonExpr
parseEgison exprString = leftToFail =<< fromEgisonM (readExpr exprString)
 where
  leftToFail (Left  e) = fail $ show e
  leftToFail (Right x) = pure x
