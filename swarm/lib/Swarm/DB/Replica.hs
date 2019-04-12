{-# OPTIONS -Wno-unused-imports #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Swarm.DB.Replica (
    TextReplica,
    createBranch,
    createObject,
    createReplica,
    getObject,
    newTextReplica,
    open,
) where

import           RON.Prelude

import           Control.Exception (mask_)
import           Control.Monad.Trans.Resource (MonadResource, allocate)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy)
import           Foreign (FinalizerPtr, ForeignPtr, Ptr, newForeignPtr)
import           Language.C.Inline.Context (ctxTypesTable)
import qualified Language.C.Inline.Cpp as Cpp
import           Language.C.Types (TypeSpecifier (TypeName))
import           Named ((:!), pattern Arg)

import           RON.Text (serializeWireFrame)
import           RON.Types (ClosedOp (ClosedOp), Op (Op),
                            WireChunk (Closed, Query),
                            WireReducedChunk (WireReducedChunk), objectId, op,
                            opId, payload, reducerId, refId, wrcBody, wrcHeader)
import           RON.UUID (UUID (UUID), zero)

import           Cxx.Std (stdCtx)
import qualified Cxx.Std.String as String
import           Swarm.RON.Status (Status (Status))
import qualified Swarm.RON.Status as Status
import           Swarm.RON.Text (TextFrame)

-- | Class @ron::Replica<TextFrame>@
newtype TextReplica = TextReplica (Ptr (Proxy TextReplica))

Cpp.context
    $   Cpp.cppCtx
    <>  Cpp.bsCtx
    <>  Cpp.fptrCtx
    <>  stdCtx
    <>  mempty
        { ctxTypesTable = Map.fromList
            [ (TypeName "Status"     , [t| Proxy Status      |])
            , (TypeName "TextFrame"  , [t| Proxy TextFrame   |])
            , (TypeName "TextReplica", [t| Proxy TextReplica |])
            ]
        }
Cpp.include "<swarm/db/replica.hpp>"
Cpp.include "<swarm/ron/status.hpp>"
Cpp.include "<swarm/ron/text.hpp>"
Cpp.include "<swarm/ron/uuid.hpp>"
Cpp.verbatim
    "typedef ron::Replica<ron::RocksDBStore<ron::TextFrame>> TextReplica;"
Cpp.verbatim "typedef ron::Status Status;"
Cpp.verbatim "typedef ron::TextFrame TextFrame;"
Cpp.verbatim "typedef ron::Uuid Uuid;"
Cpp.verbatim "typedef std::string std_string;"

-- | Method @Status CreateReplica()@
createReplica :: TextReplica -> IO Status
createReplica (TextReplica replica) =
    Status.decoding_ $ \statusPtr -> [Cpp.block| void {
        * $(Status * statusPtr) = $(TextReplica * replica)->CreateReplica();
    } |]

-- | Method @Status Open()@
open :: TextReplica -> IO Status
open (TextReplica replica) =
    Status.decoding_ $ \statusPtr -> [Cpp.block| void {
        * $(Status * statusPtr) = $(TextReplica * replica)->Open();
    } |]

-- | Method @Status GetObject(Frame& frame, Uuid id, Uuid rdt)@
getObject
    :: "object" :! UUID
    -> "type"   :! UUID
    -> "yarn"   :! Word64
    -> TextReplica
    -> IO (Either Status ByteString)
getObject (Arg object) (Arg typ) = receiveFrame query where
    query =
        toStrict $
        serializeWireFrame
            [Query $ WireReducedChunk
                { wrcHeader = ClosedOp
                    { reducerId = zero
                    , objectId = zero
                    , op = Op{opId = object, refId = typ, payload = []}
                    }
                , wrcBody = []
                }]

-- | Method @Status ReceiveFrame(Builder& response, Frame frame)@
receiveFrame
    :: ByteString
    -> "yarn" :! Word64
    -> TextReplica
    -> IO (Either Status ByteString)
receiveFrame frame (Arg yarn) (TextReplica replicaPtr) =
    Status.with $ \statusPtr ->
    String.with $ \resultDataPtr -> do
        [Cpp.block| void {
            Status & status = * $(Status * statusPtr);
            TextFrame::Builder result;
            status =
                $(TextReplica * replicaPtr)
                ->ReceiveFrame(
                    result,
                    TextFrame{std::string($bs-ptr:frame, $bs-len:frame)},
                    $(uint64_t yarn)
                );
            if (status)
                * $(std_string * resultDataPtr) = result.data();
        } |]
        status <- Status.decode statusPtr
        case status of
            Status code _ | code == Status.ok ->
                Right <$> String.decode resultDataPtr
            _ -> pure $ Left status

newTextReplica :: MonadResource m => m TextReplica
newTextReplica =
    snd <$>
    allocate
        (TextReplica <$> [Cpp.exp| TextReplica * { new TextReplica } |])
        (\(TextReplica p) -> [Cpp.block| void { delete $(TextReplica * p); } |])

createObject
    :: "type" :! UUID
    -> "yarn" :! Word64
    -> TextReplica
    -> IO (Either Status ())
createObject _typ yarn replica =
    receiveFrame frameIn yarn replica >>= \case
        Left errorStatus -> pure $ Left errorStatus
        Right frameOut   -> error $ "frameOut = " <> show frameOut
  where
    frameIn = "@1+0 :rga ;"
        -- toStrict $
        -- serializeWireFrame
        --     [Closed ClosedOp
        --         { reducerId = zero
        --         , objectId = zero
        --         , op = Op{opId = zero, refId = typ, payload = []}
        --         }]

-- | Method @Status CreateBranch(Word yarn_id)@
createBranch :: "yarn" :! Word64 -> TextReplica -> IO Status
createBranch (Arg yarn) (TextReplica replicaPtr) =
    Status.decoding_ $ \statusPtr -> [Cpp.block| void {
        * $(Status * statusPtr) =
            $(TextReplica * replicaPtr)->CreateBranch($(uint64_t yarn));
    } |]
