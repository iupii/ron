{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | RON Storage interface. For usage, see "RON.Storage.FS".
module RON.Storage
  ( Collection (..),
    CollectionDocId (..),
    CollectionName,
    DocId,
    createDocument,
    decodeDocId,
    docIdFromUuid,
    loadDocument,
    modify
    )
where

import qualified Data.Text as Text
import RON.Data (ObjectStateT, reduceObject, runObjectState)
import RON.Error (Error (Error), errorContext, throwErrorString)
import RON.Prelude
import RON.Storage.Backend
  ( Collection (..),
    CollectionName,
    DocId (DocId),
    Document (Document, isTouched, objectFrame, versions),
    IsTouched (IsTouched),
    MonadStorage,
    createVersion,
    decodeDocId,
    getDocumentVersions,
    readVersion
    )
import RON.Types (ObjectFrame, UUID)
import qualified RON.UUID as UUID

data CollectionDocId = forall a. Collection a => CollectionDocId (DocId a)

-- | Load all versions of a document
loadDocument :: (Collection a, MonadStorage m) => DocId a -> m (Document a)
loadDocument docid = loadRetry (3 :: Int)
  where
    loadRetry n
      | n > 0 =
        do
          versions0 <- getDocumentVersions docid
          case versions0 of
            [] ->
              throwErrorString
                $ "Document with id "
                ++ show docid
                ++ " has not found."
            v : vs -> do
              let versions = v :| vs
              let wrapDoc (objectFrame, isTouched) =
                    Document {objectFrame, versions, isTouched}
              readResults <-
                errorContext ("document " <> show docid)
                  $ for versions $ \ver ->
                  try
                    $ errorContext ("version " <> Text.pack ver)
                    $ readVersion docid ver
              liftEither $ wrapDoc <$> vsconcat readResults
      | otherwise = throwError "Maximum retries exceeded"

-- | Validation-like version of 'sconcat'.
vsconcat
  :: NonEmpty (Either Error (ObjectFrame a, IsTouched))
  -> Either Error (ObjectFrame a, IsTouched)
vsconcat = foldr1 vappend
  where
    vappend (Left e1) (Left e2) = Left $ Error "vappend" [e1, e2]
    vappend e1@(Left _) (Right _) = e1
    vappend (Right _) e2@(Left _) = e2
    vappend (Right r1) (Right r2) =
      (,IsTouched (t1 || t2)) <$> reduceObject a1 a2
      where
        (a1, IsTouched t1) = r1
        (a2, IsTouched t2) = r2

try :: MonadError e m => m a -> m (Either e a)
try ma = (Right <$> ma) `catchError` (pure . Left)

-- | Load document, apply changes and put it back to storage
modify :: (Collection a, MonadStorage m) => DocId a -> ObjectStateT a m b -> m b
modify docid f = do
  oldDoc <- loadDocument docid
  (b, objectFrame') <- runObjectState (objectFrame oldDoc) f
  createVersion (Just (docid, oldDoc)) objectFrame'
  pure b

-- | Create document assuming it doesn't exist yet.
createDocument :: (Collection a, MonadStorage m) => ObjectFrame a -> m ()
createDocument = createVersion Nothing

docIdFromUuid :: UUID -> DocId a
docIdFromUuid = DocId . UUID.encodeBase32
