{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Concise interface of miniEgison using view patterns.

module Control.Egison.QQ.View
  ( view
  )
where

import           Control.Egison.Match           ( matchAll )
import           Control.Egison.QQ.Internal     ( parseMode
                                                , parseExp
                                                , parsePatternExpr
                                                , compilePattern
                                                )

import           Control.Monad                  ( unless )
import qualified Data.Char                     as Char
                                                ( isSpace )

import           Language.Haskell.TH            ( Q
                                                , Name
                                                , Exp(..)
                                                , Pat(..)
                                                )
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )

view :: QuasiQuoter
view = QuasiQuoter { quoteExp  = makeMatchExp
                   , quotePat  = makeMatchPat
                   , quoteType = const $ unSupported "a type"
                   , quoteDec  = const $ unSupported "a declaration"
                   }
 where
  unSupported s =
    fail
      $ "'match' quasi-quoter must be placed as an expression or a pattern, not as "
      ++ s

makeMatchExp :: String -> Q Exp
makeMatchExp = fmap snd . makeView

makeMatchPat :: String -> Q Pat
makeMatchPat content = do
  (vars, view) <- makeView content
  [p| ($(pure view) -> $(pure . TupP $ map VarP vars):_) |]

makeView :: String -> Q ([Name], Exp)
makeView content = do
  (patStr, matcherStr) <- expectAs content ""
  mode                 <- parseMode
  matcher              <- parseExp mode matcherStr
  (pat, rest)          <- parsePatternExpr mode patStr
  unless (null $ skipSpace rest)
    .  fail
    $  "unexpected "
    ++ show rest
    ++ " found after the pattern"
  (vars, mc) <- compilePattern pat (TupE . map VarE)
  view       <-
    [| \x -> Control.Egison.Match.matchAll x $(pure matcher) [$(pure mc)] |]
  pure (vars, view)
 where
  skipSpace [] = []
  skipSpace (c : cs) | Char.isSpace c = skipSpace cs
                     | otherwise      = c : cs
  expectAs ('a' : 's' : rest) acc = pure (acc, rest)
  expectAs (c         : cs  ) acc = expectAs cs $ acc ++ [c]
  expectAs []                 _   = fail "'as' is expected but not found"
