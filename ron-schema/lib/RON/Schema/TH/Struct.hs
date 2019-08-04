{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- HLINT ignore "Reduce duplication" -}

module RON.Schema.TH.Struct (
    mkReplicatedStructLww,
    mkReplicatedStructSet,
) where

import           RON.Prelude

import           Data.Char (toTitle)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Language.Haskell.TH (bindS, conT, doE, listE, newName, noBindS,
                                      recC, recConE, recP, sigD, varE, varP,
                                      varT)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (liftData)

import           RON.Data (MonadObjectState, ObjectStateT, Rep,
                           Replicated (encoding),
                           ReplicatedAsObject (getObject, newObject, rempty),
                           getObjectStateChunk, objectEncoding)
import           RON.Data.LWW (LwwRep)
import qualified RON.Data.LWW as LWW
import           RON.Data.ORSet (ORSetRep)
import qualified RON.Data.ORSet as ORSet
import           RON.Error (MonadE, errorContext)
import           RON.Event (ReplicaClock)
import           RON.Schema as X
import           RON.Schema.TH.Common (fieldExp', fieldPat', liftText,
                                       mkGuideType, mkNameT, newNameT, valDP)
import           RON.Types (Object (Object), UUID)
import           RON.Util (Instance (Instance))
import qualified RON.UUID as UUID

type instance XField Equipped = XFieldEquipped

data XFieldEquipped = XFieldEquipped
    { haskellName :: Text
    , ronName     :: UUID
    }

equipStruct :: Struct e Resolved -> Struct e Equipped
equipStruct Struct{name, fields, annotations} = Struct
    {name, fields = Map.mapWithKey (equipField annotations) fields, annotations}

mkReplicatedStructLww :: StructLww Resolved -> TH.DecsQ
mkReplicatedStructLww structResolved = do
    dataType               <- mkDataType                struct
    [instanceReplicated]   <- mkInstanceReplicated      type'
    [instanceReplicatedAO] <- mkInstanceReplicatedAOLww struct
    accessors <- fold <$> traverse (mkAccessorsLww name') fields
    pure
        $ dataType
        : instanceReplicated : instanceReplicatedAO
        : accessors
  where
    struct@Struct{name, fields} = equipStruct structResolved
    name' = mkNameT name
    type' = conT    name'

mkReplicatedStructSet :: StructSet Resolved -> TH.DecsQ
mkReplicatedStructSet structResolved = do
    dataType               <- mkDataType                struct
    [instanceReplicated]   <- mkInstanceReplicated      type'
    [instanceReplicatedAO] <- mkInstanceReplicatedAOSet struct
    accessors <- fold <$> traverse (mkAccessorsSet name') fields
    pure
        $ dataType
        : instanceReplicated : instanceReplicatedAO
        : accessors
  where
    struct@Struct{name, fields} = equipStruct structResolved
    name' = mkNameT name
    type' = conT    name'

equipField :: StructAnnotations -> Text -> Field Resolved -> Field Equipped
equipField structAnnotations schemaName Field{..} =
    case UUID.mkName $ Text.encodeUtf8 schemaName of
        Just ronName -> Field{ext = XFieldEquipped{haskellName, ronName}, ..}
        Nothing -> error $
            "Field name is not representable in RON: " ++ show schemaName
  where
    haskellName = mkHaskellFieldName structAnnotations schemaName

varBangType' :: Text -> TH.TypeQ -> TH.VarBangTypeQ
varBangType' name
    = TH.varBangType (mkNameT name)
    . TH.bangType (TH.bang TH.noSourceUnpackedness TH.sourceStrict)

mkDataType :: Struct encoding Equipped -> TH.DecQ
mkDataType Struct{name, fields} =
    TH.dataD
        (TH.cxt [])
        name'
        []
        Nothing
        [recC name'
            [ varBangType' haskellName $ mkFieldType ronType
            | Field{ronType, ext = XFieldEquipped{haskellName}} <- toList fields
            ]]
        []
  where
    name' = mkNameT name

mkInstanceReplicated :: TH.TypeQ -> TH.DecsQ
mkInstanceReplicated type' = [d|
    instance Replicated $type' where
        encoding = objectEncoding
    |]

mkInstanceReplicatedAOLww :: StructLww Equipped -> TH.DecsQ
mkInstanceReplicatedAOLww Struct{name, fields} = do
    ops  <- newName "ops"
    vars <- traverse (newNameT . haskellName . ext) fields
    let packFields = listE
            [ let
                mvar
                    | isObjectType ronType = [| Just $(varE var) |]
                    | otherwise            = varE var  -- already in Maybe
                in
                [| ($ronName', Instance <$> $mvar) |]
            | Field{ronType, ext = XFieldEquipped{ronName}} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
        unpackFields =
            [ bindS (varP var) $ let
                view = [| LWW.viewField $ronName' $(varE ops) |]
                in
                if isObjectType ronType then
                    [| fromMaybe rempty <$> $view |]
                else
                    view
            | Field{ronType, ext = XFieldEquipped{ronName}} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
    let consE = recConE name'
            [ fieldExp' haskellName $ varE var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            | var <- toList vars
            ]
        consP = recP name'
            [ fieldPat' haskellName $ varP var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            | var <- toList vars
            ]
    let getObjectImpl = doE
            $   bindS (varP ops) [| getObjectStateChunk |]
            :   unpackFields
            ++  [noBindS [| pure $consE |]]
        remptyImpl = recConE name'
            [ fieldExp' haskellName remptyField
            | Field{ronType, ext = XFieldEquipped{haskellName}} <- toList fields
            , let
                remptyField
                    | isObjectType ronType = [| rempty  |]
                    | otherwise            = [| Nothing |]
            ]
    [d| instance ReplicatedAsObject $type' where
            type Rep $type' = LwwRep
            newObject $consP = Object <$> LWW.newStruct $packFields
            getObject = errorContext $(liftText errCtx) $getObjectImpl
            rempty = $remptyImpl
        |]
  where
    name' = mkNameT name
    type' = conT    name'
    errCtx = "getObject @" <> name

mkInstanceReplicatedAOSet :: StructSet Equipped -> TH.DecsQ
mkInstanceReplicatedAOSet Struct{name, fields} = do
    ops  <- newName "ops"
    vars <- traverse (newNameT . haskellName . ext) fields
    let packFields = listE
            [ let
                mvar
                    | isObjectType ronType = [| Just $(varE var) |]
                    | otherwise            = varE var  -- already in Maybe
                in
                [| ($ronName', Instance <$> $mvar) |]
            | Field{ronType, ext = XFieldEquipped{ronName}} <- toList fields
            , let ronName' = liftData ronName
            | var <- toList vars
            ]
        unpackFields =
            [ bindS
                (varP var)
                [| errorContext $(liftText haskellName) $
                    $(orSetViewField mergeStrategy) $ronName' $(varE ops) |]
            | Field{annotations = FieldAnnotations{mergeStrategy}, ext} <-
                toList fields
            , let
                XFieldEquipped{haskellName, ronName} = ext
                ronName' = liftData ronName
            | var <- toList vars
            ]
    let consE = recConE name'
            [ fieldExp' haskellName $ varE var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            | var <- toList vars
            ]
        consP = recP name'
            [ fieldPat' haskellName $ varP var
            | Field{ext = XFieldEquipped{haskellName}} <- toList fields
            | var <- toList vars
            ]
    let getObjectImpl = doE
            $   bindS (varP ops) [| getObjectStateChunk |]
            :   unpackFields
            ++  [noBindS [| pure $consE |]]
        remptyImpl = recConE name'
            [ fieldExp' haskellName remptyField
            | Field{ronType, ext = XFieldEquipped{haskellName}} <- toList fields
            , let
                remptyField
                    | isObjectType ronType = [| rempty  |]
                    | otherwise            = [| Nothing |]
            ]
    [d| instance ReplicatedAsObject $type' where
            type Rep $type' = ORSetRep
            newObject $consP = Object <$> ORSet.newStruct $packFields
            getObject = errorContext $(liftText errCtx) $getObjectImpl
            rempty = $remptyImpl
        |]
  where
    name' = mkNameT name
    type' = conT name'
    errCtx = "getObject @" <> name

mkHaskellFieldName :: StructAnnotations -> Text -> Text
mkHaskellFieldName annotations base = prefix <> base' where
    StructAnnotations
            { haskellFieldPrefix        = prefix
            , haskellFieldCaseTransform = caseTransform
            }
        = annotations
    base' = case caseTransform of
        Nothing        -> base
        Just TitleCase -> case Text.uncons base of
            Nothing            -> base
            Just (b, baseTail) -> Text.cons (toTitle b) baseTail

mkAccessorsLww :: TH.Name -> Field Equipped -> TH.DecsQ
mkAccessorsLww name' field = do
    a <- varT <$> newName "a"
    m <- varT <$> newName "m"
    let assignF =
            [ sigD assign
                [t| (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m)
                    => $fieldType -> $m () |]
            , valDP assign $
                if isObjectType ronType then
                    [| LWW.assignField $ronName' . Just |]
                else
                    [| LWW.assignField $ronName' |]
            ]
        readF =
            [ sigD read
                [t| (MonadE $m, MonadObjectState $type' $m) => $m $fieldType |]
            , valDP read $
                if isObjectType ronType then
                    [| fromMaybe rempty <$> LWW.readField $ronName' |]
                else
                    [| LWW.readField $ronName' |]
            ]
        zoomF =
            [ sigD zoom
                [t| MonadE $m
                    => ObjectStateT $guideType $m $a
                    -> ObjectStateT $type'     $m $a |]
            , valDP zoom [| LWW.zoomField $ronName' |]
            ]
    sequenceA $ assignF ++ readF ++ zoomF
  where
    Field{ronType, ext = XFieldEquipped{haskellName, ronName}} = field
    ronName' = liftData ronName
    type'    = conT name'
    fieldType = mkFieldType ronType
    guideType = mkGuideType ronType
    assign = mkNameT $ haskellName <> "_assign"
    read   = mkNameT $ haskellName <> "_read"
    zoom   = mkNameT $ haskellName <> "_zoom"

mkAccessorsSet :: TH.Name -> Field Equipped -> TH.DecsQ
mkAccessorsSet name' field = do
    a <- varT <$> newName "a"
    m <- varT <$> newName "m"
    let assignF =
            [ sigD assign
                [t| (ReplicaClock $m, MonadE $m, MonadObjectState $type' $m)
                    => $fieldType -> $m () |]
            , valDP assign $
                if isObjectType ronType then
                    [| LWW.assignField $ronName' . Just |]
                else
                    [| LWW.assignField $ronName' |]
            ]
        readF =
            [ sigD read
                [t| (MonadE $m, MonadObjectState $type' $m) => $m $fieldType |]
            , valDP read
                [| do
                    chunk <- getObjectStateChunk
                    $(orSetViewField mergeStrategy) $ronName' chunk |]
            ]
        zoomF
            | isObjectType ronType =
                [ sigD zoom
                    [t| (MonadE $m, ReplicaClock $m)
                        => ObjectStateT $guideType $m $a
                        -> ObjectStateT $type'     $m $a |]
                , valDP zoom [| ORSet.zoomFieldObject $ronName' |]
                ]
            | otherwise = []
    sequenceA $ assignF ++ readF ++ zoomF
  where
    Field{ronType, annotations = FieldAnnotations{mergeStrategy}, ext} = field
    XFieldEquipped{haskellName, ronName} = ext
    ronName' = liftData ronName
    type'    = conT name'
    fieldType = mkFieldType ronType
    guideType = mkGuideType ronType
    assign = mkNameT $ haskellName <> "_assign"
    read   = mkNameT $ haskellName <> "_read"
    zoom   = mkNameT $ haskellName <> "_zoom"

orSetViewField :: Maybe MergeStrategy -> TH.ExpQ
orSetViewField = varE . \case
    Nothing  -> 'ORSet.viewField
    Just LWW -> 'ORSet.viewFieldLWW
    Just Max -> 'ORSet.viewFieldMax
    Just Min -> 'ORSet.viewFieldMin
    Just Set -> 'ORSet.viewFieldSet

mkFieldType :: RonType -> TH.TypeQ
mkFieldType t
    | isObjectType t = mkGuideType t
    | otherwise      = [t| Maybe $(mkGuideType t) |]
