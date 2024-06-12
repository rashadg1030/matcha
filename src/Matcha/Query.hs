{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Matcha.Query where

import Data.Text (Text)
import Network.HTTP.Types (Query)
import Network.Wai (Request)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

data Parser a where
    FMap :: (a -> b) -> Parser a -> Parser b
    Pure :: a -> Parser a
    Apply :: Parser (a -> b) -> Parser a -> Parser b
    Optional :: Parser a -> Parser (Maybe a)
    Option :: a -> Parser a -> Parser a
    Param :: (FromHttpApiData a, ToHttpApiData a) => Text -> Parser a
    Flag :: Text -> Parser ()

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = FMap

instance Applicative Parser where
    pure :: a -> Parser a
    pure = Pure

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = Apply

data Error = Error

parse :: Request -> Parser a -> Either Error a
parse = undefined

match :: Parser a -> Query -> Either Error a
match = undefined

on :: Parser a -> Query -> (Error -> b) -> (a -> b) -> b
on = undefined