{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Matcha.Path where

import Data.Text (Text)
import Web.HttpApiData (
    FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
 )

pattern Param :: (ToHttpApiData a, FromHttpApiData a) => a -> Text
pattern Param x <- (parseUrlPiece -> Right x)
    where
        Param x = toUrlPiece x