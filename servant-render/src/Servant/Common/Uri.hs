{-# LANGUAGE OverloadedStrings #-}
module Servant.Common.Uri where

import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT (toStrict)
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Builder.Int as LT
import Data.Monoid ((<>))
import qualified Data.List as L
import Data.Bool (bool)

import Control.Applicative ((<|>),optional)
import Data.Attoparsec.Text (Parser,(<?>),takeTill,char,takeText,decimal,sepBy)

import Reflex.Class (Reflex(..),MonadHold(..),leftmost,ffor)
import Reflex.Dynamic (attachPromptlyDyn)
import Reflex.PerformEvent.Class (PerformEvent(..))

data Scheme = Http | Https deriving (Show,Eq,Ord)

encodeScheme :: Scheme -> LT.Builder
encodeScheme Http  = "http"
encodeScheme Https = "https"

parseScheme :: Parser Scheme
parseScheme = Https <$ "https" <|> Http <$ "http" <?> "Unsupported uri scheme"

data Authority = Authority
  { authorityScheme :: Scheme
  , authorityHost   :: T.Text
  , authorityPort   :: Maybe Word16
  , authorityPath   :: T.Text
  } deriving (Show,Eq,Ord)

encodeAuthority :: Authority -> LT.Builder
encodeAuthority (Authority s h mp path) =
  encodeScheme s <> "://" <> LT.fromText h <> maybe "" (\p -> ":" <> LT.fromString (show p)) mp <> "/" <> LT.fromText path

encodeAuthority' :: Authority -> T.Text
encodeAuthority' = LT.toStrict . LT.toLazyText . encodeAuthority

parseAuthority :: Parser Authority
parseAuthority = Authority
  <$> parseScheme <* "://"
  <*> takeTill (\c -> c == '/' || c == ':')
  <*> optional (char ':' *> decimal)
  <*> pure ""

data QueryPiece
  = QueryPieceParam T.Text T.Text
  | QueryPieceFlag  T.Text deriving (Show,Eq,Ord)

data Uri = Uri
  { uriPathPieces :: [T.Text]
  , uriQuerys     :: [QueryPiece]
  } deriving (Show,Eq,Ord)

unconsPathPiece :: Uri -> Maybe (T.Text, Uri)
unconsPathPiece (Uri pathPieces querys) = case pathPieces of
  [] -> Nothing
  (piece:rest) -> Just (piece, Uri rest querys)

unconsQuery :: Uri -> Maybe (QueryPiece, Uri)
unconsQuery (Uri pathPieces querys) = case querys of
  [] -> Nothing
  (query:rest) -> Just (query, Uri pathPieces rest)

encodeUri :: Uri -> LT.Builder
encodeUri (Uri pathPieces querys) =
  mconcat (L.intersperse "/" (fmap LT.fromText pathPieces)) <>
  bool (LT.singleton '?' <> mconcat (L.intersperse "&" (fmap encodeQuery querys))) "" (L.null querys)
  where encodeQuery (QueryPieceParam x y) = LT.fromText x <> LT.singleton '=' <> LT.fromText y
        encodeQuery (QueryPieceFlag  x)   = LT.fromText x

parseUri :: Parser Uri
parseUri = Uri
  <$> filter (not . T.null) . T.splitOn "/" <$> takeTill (== '?')
  <*> (char '?' *> queryParam `sepBy` char '&' <|> pure [])
  where queryParam =
          QueryPieceParam <$> takeTill (=='=') <*> (char '=' *> takeTill (=='&')) <|>
          QueryPieceFlag  <$> takeTill (=='&')

encodeUrl :: Authority -> Uri -> T.Text
encodeUrl authority uri = LT.toStrict . LT.toLazyText $ encodeAuthority authority <> encodeUri uri

parseUrl :: Parser (Authority, Uri)
parseUrl = (,) <$> parseAuthority <*> parseUri
