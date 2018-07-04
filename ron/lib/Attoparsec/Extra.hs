{-# LANGUAGE OverloadedStrings #-}

module Attoparsec.Extra
    ( module Attoparsec
    , failWith
    , getPos
    , label
    , label'
    , manyTillEnd
    , parseWhole
    , someTillEnd
    , takeAtMost
    , takeL
    , whole
    , withInputSize
    ) where

import           Internal.Prelude

import qualified Data.Attoparsec.Internal.Types as Internal
import           Data.Attoparsec.Lazy as Attoparsec
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)

parseWhole :: Parser a -> ByteStringL -> Either String a
parseWhole p = parseOnly (whole p) . toStrict

-- | 'Attoparsec.take' adapter to 'ByteStringL'
takeL :: Int -> Parser ByteStringL
takeL = fmap fromStrict . Attoparsec.take

getPos :: Parser Int
getPos =
    Internal.Parser $ \t pos more _ suc -> suc t pos more $ Internal.fromPos pos

withInputSize :: Parser a -> Parser (Int, a)
withInputSize p = do
    posBefore <- getPos
    r <- p
    posAfter <- getPos
    pure (posAfter - posBefore, r)

label :: String -> Parser a -> Parser a
label = flip (<?>)

label' :: String -> Parser a -> Parser a
label' name p = do
    pos <- getPos
    label (name ++ ':' : show pos) p

manyTillEnd :: Parser a -> Parser [a]
manyTillEnd p = liftA2 (:) p (someTillEnd p)

someTillEnd :: Parser a -> Parser [a]
someTillEnd p = do
    r <- p
    weAreAtEnd <- atEnd
    if weAreAtEnd then
        pure [r]
    else
        someTillEnd p

whole :: Parser a -> Parser a
whole p = do
    r <- p
    weAreAtEnd <- atEnd
    if weAreAtEnd then
        pure r
    else do
        pos <- getPos
        rest <- takeAtMost 11
        let cite
                | BS.length rest < 11 = rest
                | otherwise           = BS.take 10 rest <> "..."
        fail $ show pos <> ": extra input: " <> show cite

takeAtMost :: Int -> Parser ByteString
takeAtMost limit = do
    pos0 <- getPos
    BS.pack <$> manyTill anyWord8 (checkLimit $ pos0 + limit)
  where
    checkLimit maxPos = do
        pos <- getPos
        guard (pos >= maxPos) <|> endOfInput

failWith :: String -> Maybe a -> Parser a
failWith msg = maybe (fail msg) pure