module Servant.Common.Uri where

import Data.Word (Word16)
import qualified Data.Text as T

data Scheme = Http | Https deriving (Show,Eq,Ord)

data Authority = Authority
  { authorityScheme :: Scheme
  , authorityHost   :: T.Text
  , authorityPort   :: Word16
  , authorityPath   :: T.Text
  } deriving (Show,Eq,Ord)

data Uri = Uri
  { uriPathPieces :: [T.Text]
  , uriQuerys     :: [(T.Text,T.Text)]
  } deriving (Show,Eq,Ord)

