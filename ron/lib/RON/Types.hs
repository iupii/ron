{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | RON model types
module RON.Types (
    Atom (..),
    ClosedOp (..),
    Object (..),
    ObjectFrame (..),
    Op (..),
    OpTerm (..),
    Payload,
    StateChunk (..),
    StateFrame,
    UUID (..),
    WireChunk (..),
    WireFrame,
    WireReducedChunk (..),
    WireStateChunk (..),
    -- * Op patterns
    OpPattern (..),
    opPattern,
    pattern AckP,
    pattern AnnotationDerivedP,
    pattern AnnotationP,
    pattern CreateP,
    pattern DeleteP,
    pattern RegularP,
    pattern UndeleteP,
) where

import           RON.Prelude

import           Data.Typeable (typeRep)
import           Text.Show (showParen, showString, showsPrec)
import qualified Text.Show

import           RON.Util.Word (pattern B00, pattern B10, pattern B11, Word2)
import           RON.UUID (UUID (UUID), uuidVersion)
import qualified RON.UUID as UUID

-- | Atom — a payload element
data Atom = AFloat Double | AInteger Int64 | AString Text | AUuid UUID
    deriving (Data, Eq, Generic, Hashable, Show)

-- | Closed op
data ClosedOp = ClosedOp
    { reducerId :: UUID
        -- ^ type
    , objectId  :: UUID
        -- ^ object id
    , op        :: Op
        -- ^ other keys and payload, that are common with reduced op
    }
    deriving (Data, Eq, Generic)

type Payload = [Atom]

-- | Open op (operation)
data Op = Op
    { opId :: UUID
        -- ^ event id (usually timestamp)
    , refId :: UUID
        -- ^ reference to other op; actual semantics depends on the type
    , payload :: Payload
        -- ^ payload
    }
    deriving (Data, Eq, Generic, Hashable, Show)

instance Show ClosedOp where
    show ClosedOp{reducerId, objectId, op = Op{opId, refId, payload}} =
        unwords
            [ "ClosedOp"
            , insert '*' $ show reducerId
            , insert '#' $ show objectId
            , insert '@' $ show opId
            , insert ':' $ show refId
            , show payload
            ]
      where
        insert k = \case
            []   -> [k]
            c:cs -> c:k:cs

-- | Common reduced chunk
data WireReducedChunk = WireReducedChunk
    { wrcHeader :: ClosedOp
    , wrcBody   :: [Op]
    }
    deriving (Data, Eq, Generic, Show)

-- | Common chunk
data WireChunk =
    Closed ClosedOp | Value WireReducedChunk | Query WireReducedChunk
    deriving (Data, Eq, Generic, Show)

-- | Common frame
type WireFrame = [WireChunk]

-- | Op terminator
data OpTerm = TClosed | TReduced | THeader | TQuery
    deriving (Eq, Show)

-- | Reduced chunk representing an object state (i. e. high-level value)
data WireStateChunk = WireStateChunk
    { stateType :: UUID
    , stateBody :: [Op]
    }
    deriving (Eq, Show)

-- | Type-tagged version of 'WireStateChunk'
newtype StateChunk a = StateChunk [Op]

-- | Frame containing only state chunks.
-- Must contain one main object and any number of other objects that are part of
-- the main one.
type StateFrame = Map UUID WireStateChunk

-- | Reference to an object
newtype Object a = Object UUID
    deriving (Eq)

instance Typeable a => Show (Object a) where
    showsPrec a (Object b) =
        showParen (a >= 11)
            $ showString "Object @"
            . showsPrec 11 (typeRep $ Proxy @a)
            . showString " "
            . showsPrec 11 b

-- | Object accompanied with a frame
data ObjectFrame a = ObjectFrame{uuid :: UUID, frame :: StateFrame}
    deriving (Eq, Show)

data OpPattern =
    Regular | Delete | Undelete | Create | Ack | Annotation | AnnotationDerived

pattern AnnotationP         :: (Word2, Word2)
pattern AnnotationP         =  (B00,   B10)
pattern AnnotationDerivedP  :: (Word2, Word2)
pattern AnnotationDerivedP  =  (B00,   B11)
pattern CreateP             :: (Word2, Word2)
pattern CreateP             =  (B10,   B00)
pattern RegularP            :: (Word2, Word2)
pattern RegularP            =  (B10,   B10)
pattern AckP                :: (Word2, Word2)
pattern AckP                =  (B10,   B11)
pattern DeleteP             :: (Word2, Word2)
pattern DeleteP             =  (B11,   B10)
pattern UndeleteP           :: (Word2, Word2)
pattern UndeleteP           =  (B11,   B11)

opPattern :: Op -> Maybe OpPattern
opPattern Op{opId, refId} =
    case mapBoth (uuidVersion . UUID.split) (opId, refId) of
        AnnotationP         -> Just Annotation
        AnnotationDerivedP  -> Just AnnotationDerived
        CreateP             -> Just Create
        RegularP            -> Just Regular
        AckP                -> Just Ack
        DeleteP             -> Just Delete
        UndeleteP           -> Just Undelete
        _                   -> Nothing

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)
