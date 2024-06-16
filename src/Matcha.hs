{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Matcha (
    -- Types
    Path,
    -- Patterns
    pattern GET,
    pattern POST,
    pattern PUT,
    pattern (:/),
    pattern Param,
    pattern Blob,
    pattern End,
    pattern Req,
    -- Request Accessors
    method,
    path,
    query,
    headers,
    ssl,
    Matcha.vault,
    -- Functions

    -- ** Returns `Application`
    on,
    match,
    with,
    handle,
    ifThenElse,

    -- ** Returns `Application` (simple)
    node,
    leaf,

    -- ** Returns `Either e o`
    view,
    using,
) where

import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Vault.Lazy (Vault)
import Network.HTTP.Types (Method, Query, RequestHeaders)
import Network.Wai (Application, Request, Response, ResponseReceived)
import Network.Wai.Internal (Request (..))
import Web.HttpApiData (
    FromHttpApiData (parseUrlPiece),
    ToHttpApiData (toUrlPiece),
 )

type Path = [Text]

pattern GET :: Method
pattern GET = "GET"

pattern POST :: Method
pattern POST = "POST"

pattern PUT :: Method
pattern PUT = "PUT"

pattern (:/) :: (ToHttpApiData a, FromHttpApiData a) => a -> Path -> Path
pattern h :/ t <- (parseUrlPiece -> Right h) : t
    where
        h :/ t = toUrlPiece h : t

infixr 8 :/

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

pattern Req :: Method -> Path -> Query -> RequestHeaders -> Bool -> Vault -> Request
pattern Req{method, path, query, headers, ssl, vault} <-
    Request
        { requestMethod = method
        , pathInfo = path
        , queryString = query
        , requestHeaders = headers
        , isSecure = ssl
        , vault = vault
        }

class RequestData i where
    grab :: Request -> i

instance RequestData Path where
    grab :: Request -> Path
    grab = pathInfo

instance RequestData Request where
    grab :: Request -> Request
    grab = id

instance RequestData Method where
    grab :: Request -> Method
    grab = requestMethod

class Parseable a where
    type Parser a :: Type -> Type
    parse :: (Parser a) o -> a -> Either e o

on :: (Request -> a) -> (a -> Application) -> Application
on f g req = (g $ f req) req

match :: forall a. (RequestData a) => (a -> Application) -> Application
match f req = f (grab @a req) req

ifThenElse :: forall a. (RequestData a) => (a -> Bool) -> Application -> Application -> Application
ifThenElse p app1 app2 req send =
    if p $ grab @a req
        then app1 req send
        else app2 req send

with :: forall a o e. (RequestData a, Parseable a) => (Parser a) o -> (Either e o -> Application) -> Application
with parser f req = f (parse parser $ grab @a req) req

handle :: forall m. (forall a. m a -> IO a) -> m Response -> Application
handle run responseM _ send = run responseM >>= send

node :: (Request -> a) -> (a -> Application) -> Application
node = on

leaf :: forall m. (forall a. m a -> IO a) -> m Response -> Application
leaf = handle

view :: (Parseable a) => (Parser a) o -> a -> Either e o
view = parse

using :: forall a o e. (RequestData a, Parseable a) => Request -> (Parser a) o -> Either e o
using request parser = parse @a parser (grab request)
