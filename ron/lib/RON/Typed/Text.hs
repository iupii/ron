module RON.Typed.Text (parse, serialize) where

import           RON.Internal.Prelude

import qualified RON.Text as Text
import           RON.Typed (Object, Replicated, objectFromStateChunk,
                            objectToStateChunk)
import           RON.Types (Chunk (Value))

serialize :: Replicated a => Object a -> Either String ByteStringL
serialize obj = do
    chunk <- objectToStateChunk obj
    pure $ Text.serializeFrame [Value chunk]

parse :: Replicated a => ByteStringL -> Either String (Object a)
parse bs = do
    frame <- Text.parseFrame bs
    case frame of
        []            -> Left "empty frame"
        [Value chunk] -> objectFromStateChunk chunk
        _             -> Left "frame reduction is not supported yet"
