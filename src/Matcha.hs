{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Matcha (
    someFunc,
) where

import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types (Method, Query, RequestHeaders)
import Network.Wai (Application, Request)
import Network.Wai.Internal (Request (..))
import Web.HttpApiData (
    FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
 )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Path = [Text]

pattern GET :: Method
pattern GET = "GET"

pattern POST :: Method
pattern POST = "POST"

pattern Param :: (ToHttpApiData a, FromHttpApiData a) => a -> Text
pattern Param x <- (parseUrlPiece -> Right x)
    where
        Param x = toUrlPiece x

pattern Blob :: (ToHttpApiData a, FromHttpApiData a) => [a] -> Path
pattern Blob l <- (partitionEithers . map parseUrlPiece -> ([], l))
    where
        Blob l = map toUrlPiece l

pattern End :: Path
pattern End = []

-- parse :: b -> Parser a -> Result Error a
-- with :: Parser a -> b -> Result Error a

class RequestData i where
    grab :: Request -> i

instance RequestData Path where
    grab = pathInfo

instance RequestData Request where
    grab = id

match :: forall a. (RequestData a) => (a -> Application) -> Application
match f req = f (grab @a req) req

-- RequestData constraint not required
class (RequestData i) => Parseable i where
    type Parser i :: Type -> Type
    parse :: (Parser i) o -> i -> Either e o

-- with :: forall i o e. (Parseable i) => i -> (Parser i) o -> Either e o
-- with = flip parse

with :: forall i o e. (Parseable i) => (Parser i) o -> (Either e o -> Application) -> Application
with parser f req = f (parse parser $ grab @i req) req

on :: forall i o e. (Parseable i) => Request -> (Parser i) o -> Either e o
on request parser = parse @i parser (grab request)

pattern Req :: Method -> Path -> Query -> RequestHeaders -> Bool -> Request
pattern Req{method, path, query, headers, ssl} <-
    Request
        { requestMethod = method
        , pathInfo = path
        , queryString = query
        , requestHeaders = headers
        , isSecure = ssl
        }

pattern MyUrl :: Int -> [Text]
pattern MyUrl a = "hello" : Param a : "blu" : End

ex1 :: Application
ex1 = match @Path \case
    MyUrl num -> ex2
    _ -> ex3

ex2 :: Application
ex2 req send = case path req of
    "hello" : Param @Int name : End -> case method req of
        GET -> undefined
        _ -> undefined
    _ -> undefined

ex3 :: Application
ex3 req send = case req of
    Req{method = GET, path = "hello" : "world" : Param @Int age : End} -> do
        undefined
    Req{ssl = True} ->
        undefined
    _ -> undefined

ex4 :: Application
ex4 = undefined
