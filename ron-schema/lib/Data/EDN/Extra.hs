{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.Extra (
    decodeMultiDoc,
    isTagged,
    parseList,
    parseSymbol',
    withNoPrefix,
    withSymbol',
) where

import           RON.Prelude

import qualified Data.EDN.AST.Lexer as EdnAst
import qualified Data.EDN.AST.Parser as EdnAst
import qualified Data.EDN.AST.Types as EdnAst
import           Data.EDN.Class.Parser (Parser)
import qualified Text.Megaparsec as Mpc

import           Data.EDN (EDNList, FromEDN, Tagged (NoTag, Tagged),
                           TaggedValue, Value (List), parseEDNv, withNoTag,
                           withSymbol)

withNoPrefix :: MonadFail m => (Text -> m a) -> Text -> Text -> m a
withNoPrefix f prefix name = case prefix of
    "" -> f name
    _  -> fail "Expected no prefix"

withSymbol' :: (Text -> Parser a) -> TaggedValue -> Parser a
withSymbol' = withNoTag . withSymbol . withNoPrefix

parseSymbol' :: TaggedValue -> Parser Text
parseSymbol' = withSymbol' pure

isTagged :: Tagged t a -> Bool
isTagged = \case
    NoTag {} -> False
    Tagged{} -> True

parseMultiDoc :: EdnAst.Parser [TaggedValue]
parseMultiDoc = EdnAst.dropWS *> many EdnAst.parseTagged <* Mpc.eof

decodeMultiDoc :: MonadFail m => String -> Text -> m [TaggedValue]
decodeMultiDoc sourceName
    = either (fail . Mpc.errorBundlePretty) pure
    . Mpc.parse parseMultiDoc sourceName

parseList :: FromEDN a => EDNList -> Parser a
parseList = parseEDNv . List
