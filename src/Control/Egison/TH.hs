{-# LANGUAGE ViewPatterns #-}

module Control.Egison.TH
  ( makeMatcher
  , makeMatcherWith
  , MatcherNamer
  , PatternNamer
  , MatcherRules(..)
  , defaultRules
  )
where

import qualified Data.Char                     as Char
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( catMaybes )
import           Control.Monad                  ( replicateM
                                                , zipWithM
                                                , (<=<)
                                                )
import           Control.Monad.State            ( runState
                                                , modify
                                                , get
                                                )
import           Language.Haskell.TH.Syntax     ( Q
                                                , Name
                                                , Dec
                                                , Type(VarT)
                                                , newName
                                                , nameBase
                                                , mkName
                                                )
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Datatype   ( DatatypeInfo(..)
                                                , ConstructorInfo(..)
                                                , reifyDatatype
                                                , datatypeType
                                                , tvName
                                                )

import           Control.Egison.Core            ( Pattern(..)
                                                , Matcher
                                                , (:++:)
                                                , MList(..)
                                                , MAtom(..)
                                                )

type MatcherNamer
  =  Name  -- ^ Name of the data type that matchers are being generated for.
  -> Name  -- ^ Name of the generated matcher type.

type PatternNamer
  =  Name  -- ^ Name of the data type that matchers are being generated for.
  -> Name  -- ^ Name of the constructor corresponding to the pattern being named.
  -> Maybe Name  -- ^ Name of the generated pattern.

data MatcherRules
  = MatcherRules { nameMatcher :: MatcherNamer
                 , namePattern :: PatternNamer
                 }

-- | Default 'MatcherRules':
--   append M to data type name to obtain matcher name,
--   and uncapitalize the initial letter of constructor name to obtain pattern name.
defaultRules :: MatcherRules
defaultRules =
  MatcherRules { nameMatcher = toMName, namePattern = uncapCtorName }

toMName :: MatcherNamer
toMName = mkName . (++ "M") . nameBase

uncapCtorName :: PatternNamer
uncapCtorName _ = Just . mkName . mapInit Char.toLower . nameBase
 where
  mapInit f (x : xs) = f x : xs
  mapInit f []       = []

-- | @'makeMatcher' = 'makeMatcherWith' 'defaultRules'@
makeMatcher :: Name -> Q [Dec]
makeMatcher = makeMatcherWith defaultRules

makeMatcherWith :: MatcherRules -> Name -> Q [Dec]
makeMatcherWith rules@MatcherRules { namePattern } name = do
  datatype <- reifyDatatype name
  config   <- makeMatcherConfig rules datatype
  dataDec  <- makeMatcherData config
  instDec  <- makeMatcherInstance config
  patDecs  <- makeMatcherPatterns config
  pure $ [dataDec, instDec] ++ patDecs


data FieldVariant
  = RecField
  | VarField Int

data Field
  = Field { variant      :: FieldVariant
          , boundVarsVar :: Name
          }

data PatternConfig
  = PatternConfig { patternName :: Name
                  , conName     :: Name
                  , fields      :: [Field]
                  , ctxVar      :: Name
                  , fieldVars   :: [Name]
                  , paramVars   :: [Name]
                  }

data MatcherConfig
  = MatcherConfig { matcherName :: Name
                  , dataName    :: Name
                  , matcherVars :: [Name]
                  , dataVars    :: [Name]
                  , patterns    :: [PatternConfig]
                  }

makeMatcherConfig :: MatcherRules -> DatatypeInfo -> Q MatcherConfig
makeMatcherConfig rules@MatcherRules { nameMatcher } datatype@DatatypeInfo { datatypeName, datatypeVars, datatypeCons }
  = do
    matcherVars <- replicateM numVars $ newName "m"
    dataVars    <- replicateM numVars $ newName "a"
    patterns    <- traverse (makePatternConfig rules datatype) datatypeCons
    pure MatcherConfig { matcherName
                       , dataName    = datatypeName
                       , matcherVars
                       , dataVars
                       , patterns    = catMaybes patterns
                       }
 where
  numVars     = length datatypeVars
  matcherName = nameMatcher datatypeName

makePatternConfig
  :: MatcherRules -> DatatypeInfo -> ConstructorInfo -> Q (Maybe PatternConfig)
makePatternConfig MatcherRules { namePattern } datatype@DatatypeInfo { datatypeName, datatypeVars } ConstructorInfo { constructorName, constructorFields }
  = do
    fields    <- traverse (makeField <=< identifyField) constructorFields
    ctxVar    <- newName "ctx"
    fieldVars <- replicateM numFields $ newName "x"
    paramVars <- replicateM numFields $ newName "p"
    case namePattern datatypeName constructorName of
      Just patternName -> pure $ Just PatternConfig { patternName
                                                    , conName = constructorName
                                                    , fields
                                                    , ctxVar
                                                    , fieldVars
                                                    , paramVars
                                                    }
      Nothing -> pure Nothing
 where
  makeField variant = do
    boundVarsVar <- newName "vs"
    pure Field { variant, boundVarsVar }
  identifyField (VarT (varIndex -> Just idx)) = pure $ VarField idx
  identifyField t | t == datatypeType datatype = pure RecField
  identifyField t = fail $ "unsupported field type " ++ show t
  varIndex v = elemIndex v $ map tvName datatypeVars
  numFields = length constructorFields

makeMatcherData :: MatcherConfig -> DecQ
makeMatcherData MatcherConfig { matcherName, matcherVars } = dataD
  (pure [])
  matcherName
  (map plainTV matcherVars)
  Nothing
  [ctor]
  []
 where
  ctor      = normalC matcherName $ map makeField matcherVars
  makeField = bangType (bang noSourceUnpackedness noSourceStrictness) . varT

makeMatcherInstance :: MatcherConfig -> DecQ
makeMatcherInstance cfg@MatcherConfig { matcherName, dataName, matcherVars, dataVars }
  = instanceD cxt head []
 where
  cxt  = makeMatcherCxt cfg
  head = [t| Control.Egison.Core.Matcher $(matcherType cfg) $(dataType cfg) |]

makeMatcherPatterns :: MatcherConfig -> DecsQ
makeMatcherPatterns cfg@MatcherConfig { patterns } =
  concat <$> traverse makePatternDecs patterns
 where
  makePatternDecs pcfg = do
    sigDec <- makeMatcherPatternSig cfg pcfg
    valDec <- makeMatcherPattern cfg pcfg
    pure [sigDec, valDec]

makeMatcherPatternSig :: MatcherConfig -> PatternConfig -> DecQ
makeMatcherPatternSig cfg@MatcherConfig { matcherName, dataName, matcherVars, dataVars } PatternConfig { patternName, fields, ctxVar }
  = sigD patternName patQualType
 where
  patQualType = forallT bndrs cxt patType
  cxt         = makeMatcherCxt cfg
  bndrs       = map plainTV $ ctxVar : matcherVars ++ dataVars ++ fieldVars
  fieldVars   = map (\Field { boundVarsVar } -> boundVarsVar) fields
  patType     = foldr (appT . appT arrowT) ret params
  ret
    = [t| Control.Egison.Core.Pattern $(dataType cfg) $(matcherType cfg) $(varT ctxVar) $(concatTypeListsR collectedVs) |]
  (params, collectedVs) = runState (traverse makeFieldParamType fields) []
  makeFieldParamType Field { variant, boundVarsVar } = do
    collectedVs <- get
    let ctx = concatTypeListsL $ ctxVar : collectedVs
    modify (++ [boundVarsVar])
    let (a, m) = case variant of
          RecField     -> (dataType cfg, matcherType cfg)
          VarField idx -> (varT $ dataVars !! idx, varT $ matcherVars !! idx)
    pure [t| Control.Egison.Core.Pattern $a $m $ctx $(varT boundVarsVar) |]
  concatTypeListsL (h : t) = foldl appendTypeLists (varT h) $ map varT t
  concatTypeListsL []      = [t| '[] |]
  concatTypeListsR (reverse -> h:t) =
    foldl (flip appendTypeLists) (varT h) $ map varT t
  concatTypeListsR [] = [t| '[] |]
  appendTypeLists x y = [t| $x Control.Egison.Core.:++: $y |]

makeMatcherPattern :: MatcherConfig -> PatternConfig -> DecQ
makeMatcherPattern MatcherConfig { matcherName, matcherVars } pcfg@PatternConfig { conName, patternName, fields, paramVars, fieldVars }
  = funD patternName [clause (map varP paramVars) (normalB body) []]
 where
  body =
    [| Control.Egison.Core.Pattern
         (\_ $(conP matcherName (map varP matcherVars)) t ->
            case t of
              $(conP conName $ map varP fieldVars) -> [$(mlist)]
              _ -> []
         )
    |]
  mlist = foldr makeMCons [| Control.Egison.Core.MNil |]
    $ zipWith3 toMAtom fields fieldVars paramVars
  makeMCons x acc = [| Control.Egison.Core.MCons $x $acc |]
  toMAtom Field { variant = RecField } fieldVar paramVar
    = [| Control.Egison.Core.MAtom $(varE paramVar) $matcherExpr $(varE fieldVar) |]
  toMAtom Field { variant = VarField idx } fieldVar paramVar
    = [| Control.Egison.Core.MAtom $(varE paramVar) $(varE $ matcherVars !! idx) $(varE fieldVar) |]
  matcherExpr = foldl appE (conE matcherName) $ map varE matcherVars

makeMatcherCxt :: MatcherConfig -> CxtQ
makeMatcherCxt MatcherConfig { matcherVars, dataVars } = zipWithM
  makeMatcherPred
  matcherVars
  dataVars
 where
  makeMatcherPred m a = [t| Control.Egison.Core.Matcher $(varT m) $(varT a) |]

applyNames :: TypeQ -> [Name] -> TypeQ
applyNames = foldl (\acc -> appT acc . varT)

matcherType :: MatcherConfig -> TypeQ
matcherType MatcherConfig { matcherName, matcherVars } =
  applyNames (conT matcherName) matcherVars

dataType :: MatcherConfig -> TypeQ
dataType MatcherConfig { dataName, dataVars } =
  applyNames (conT dataName) dataVars
