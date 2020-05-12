{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Concise interface of miniEgison using view patterns and pattern synonyms.

module Control.Egison.View
  ( pattern Matches
  , pattern Match
  , pattern Exactly
  , pattern Is
  , pattern Are
  , view
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

pattern Matches :: [a] -> [a]
pattern Matches xs <- xs

{-# COMPLETE Matches #-}

pattern Match :: [a]
pattern Match <- _:_

pattern Null :: [a]
pattern Null <- []

{-# COMPLETE Null, Match #-}

pattern Exactly :: a -> [a]
pattern Exactly x <- [x]

pattern Is :: a -> [a]
pattern Is x <- x:_

{-# COMPLETE Null, Is #-}

pattern Are :: [a] -> [a]
pattern Are xs <- xs@(_:_)

{-# COMPLETE Null, Are #-}

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
  [p| ($(pure view) -> Control.Egison.View.Is $(pure . TupP $ map VarP vars)) |]

makeView :: String -> Q ([Name], Exp)
makeView content = do
  afterAs              <- takeAs (skipSpace content)
  (matcherStr, patStr) <- expectOf afterAs ""
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
  takeAs ('a' : 's' : xs) = pure xs
  takeAs (c         : _ ) = fail $ "'as' is expected, but got " ++ show c
  takeAs []               = fail "'as' is expected, but not found"
  expectOf ('o' : 'f' : rest) acc = pure (acc, rest)
  expectOf (c         : cs  ) acc = expectOf cs $ acc ++ [c]
  expectOf []                 _   = fail "'of' is expected but not found"
