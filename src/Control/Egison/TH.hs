{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Control.Egison.TH
  ( makeMatcher
  , makeMatcherWith
  , MatcherNamer
  , PatternNamer
  , MatcherRules(..)
  , defaultRules
  , (:%:)
  , M
  , Rec
  )
where

import qualified Data.Char                     as Char
import           Data.List                      ( elemIndex )
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Void                      ( Void
                                                , absurd
                                                )
import qualified Data.Map                      as Map
                                                ( fromList )
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
                                                , Type(VarT, ConT, AppT)
                                                , newName
                                                , nameBase
                                                , namePackage
                                                , nameModule
                                                , mkName
                                                , mkNameG_d
                                                )
import           Language.Haskell.TH.Ppr        ( pprint )
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Datatype   ( DatatypeInfo(..)
                                                , ConstructorInfo(..)
                                                , applySubstitution
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

-- | Type-level operator to specify matchers on each field of the constructor.
type family t :%: matcher where
  t :%: _ = t

-- | Type-level name to specify a generated matcher of a type parameter.
type family M v :: *
-- | Type-level name to specify a generated matcher of the data type itself.
type family Rec :: *

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


data MatcherCon
  = MatcherCon { typeName :: Name
               , dataName :: Name
               }

data MatcherSpec
  = RecM
  | VarM Int
  | ConM MatcherCon
  | AppM MatcherSpec MatcherSpec

data Field
  = RecField
  | VarField Int
  | SpecField TypeQ MatcherSpec

data FieldConfig
  = FieldConfig { variant      :: Field
                , boundVarsVar :: Name
                }

data PatternConfig
  = PatternConfig { patternName :: Name
                  , conName     :: Name
                  , fields      :: [FieldConfig]
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
    patterns    <- traverse (makePatternConfig rules dataVars datatype)
                            datatypeCons
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
  :: MatcherRules
  -> [Name]
  -> DatatypeInfo
  -> ConstructorInfo
  -> Q (Maybe PatternConfig)
makePatternConfig MatcherRules { namePattern } generatedDataVars datatype@DatatypeInfo { datatypeName, datatypeVars } ConstructorInfo { constructorName, constructorFields }
  = do
    fields    <- traverse (makeFieldConfig <=< identifyField) constructorFields
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
  makeFieldConfig variant = do
    boundVarsVar <- newName "vs"
    pure FieldConfig { variant, boundVarsVar }
  identifyField (VarT (varIndex -> Just idx))  = pure $ VarField idx
  identifyField t | t == datatypeType datatype = pure RecField
  identifyField (AppT (AppT (ConT op) t) m) | op == ''(:%:) =
    SpecField (substVars t) <$> toSpec m
  identifyField t =
    fail
      $  "unsupported field type "
      ++ pprint t
      ++ ". you may want to specify a matcher with :%:"
  toSpec (ConT c) | c == ''Rec = pure RecM
  toSpec (ConT t@(dataify -> Just d)) =
    pure $ ConM MatcherCon { typeName = t, dataName = d }
  toSpec (AppT (ConT c) (VarT (varIndex -> Just idx))) | c == ''M =
    pure $ VarM idx
  toSpec (AppT a b) = AppM <$> toSpec a <*> toSpec b
  toSpec m          = fail $ "unsupported form of matcher " ++ pprint m
  dataify name = case (namePackage name, nameModule name, nameBase name) of
    (Just pkg, Just modu, occ) -> Just $ mkNameG_d pkg modu occ
    _                          -> Nothing
  substVars = pure . applySubstitution varMap
  varMap =
    Map.fromList $ zip (map tvName datatypeVars) (map VarT generatedDataVars)
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
  fieldVars   = map (\FieldConfig { boundVarsVar } -> boundVarsVar) fields
  patType     = foldr (appT . appT arrowT) ret params
  ret
    = [t| Control.Egison.Core.Pattern $(dataType cfg) $(matcherType cfg) $(varT ctxVar) $(concatTypeListsR collectedVs) |]
  (params, collectedVs) = runState (traverse makeFieldParamType fields) []
  makeFieldParamType FieldConfig { variant, boundVarsVar } = do
    collectedVs <- get
    let ctx = concatTypeListsL $ ctxVar : collectedVs
    modify (++ [boundVarsVar])
    let (a, m) = case variant of
          RecField      -> (dataType cfg, matcherType cfg)
          VarField idx  -> (varT $ dataVars !! idx, varT $ matcherVars !! idx)
          SpecField t m -> (t, specToType cfg m)
    pure [t| Control.Egison.Core.Pattern $a $m $ctx $(varT boundVarsVar) |]
  concatTypeListsL (h : t) = foldl appendTypeLists (varT h) $ map varT t
  concatTypeListsL []      = [t| '[] |]
  concatTypeListsR (reverse -> h:t) =
    foldl (flip appendTypeLists) (varT h) $ map varT t
  concatTypeListsR [] = [t| '[] |]
  appendTypeLists x y = [t| $x Control.Egison.Core.:++: $y |]

makeMatcherPattern :: MatcherConfig -> PatternConfig -> DecQ
makeMatcherPattern cfg@MatcherConfig { matcherName, matcherVars } pcfg@PatternConfig { conName, patternName, fields, paramVars, fieldVars }
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
  toMAtom FieldConfig { variant = RecField } fieldVar paramVar
    = [| Control.Egison.Core.MAtom $(varE paramVar) $(matcherExpr cfg) $(varE fieldVar) |]
  toMAtom FieldConfig { variant = VarField idx } fieldVar paramVar
    = [| Control.Egison.Core.MAtom $(varE paramVar) $(varE $ matcherVars !! idx) $(varE fieldVar) |]
  toMAtom FieldConfig { variant = SpecField _ m } fieldVar paramVar
    = [| Control.Egison.Core.MAtom $(varE paramVar) $(specToExpr cfg m) $(varE fieldVar) |]

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

matcherExpr :: MatcherConfig -> ExpQ
matcherExpr MatcherConfig { matcherName, matcherVars } =
  foldl appE (conE matcherName) $ map varE matcherVars

specToType :: MatcherConfig -> MatcherSpec -> TypeQ
specToType cfg@MatcherConfig { matcherVars } = go
 where
  go RecM                           = matcherType cfg
  go (VarM idx                    ) = varT $ matcherVars !! idx
  go (ConM MatcherCon { typeName }) = conT typeName
  go (AppM a b                    ) = appT (specToType cfg a) (specToType cfg b)

specToExpr :: MatcherConfig -> MatcherSpec -> ExpQ
specToExpr cfg@MatcherConfig { matcherVars } = go
 where
  go RecM                           = matcherExpr cfg
  go (VarM idx                    ) = varE $ matcherVars !! idx
  go (ConM MatcherCon { dataName }) = conE dataName
  go (AppM a b                    ) = appE (specToExpr cfg a) (specToExpr cfg b)
