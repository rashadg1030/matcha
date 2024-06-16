{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Matcha
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
    print @String "Running app..."
    Warp.run 3000 $ match \case
        "hello" : "world" : (age :: Int) :/ End -> match \case
            GET -> handle id do
                return $ Wai.responseLBS HTTP.ok200 [] "Hello world!"
            _ -> \_ send -> send $ Wai.responseLBS HTTP.methodNotAllowed405 [] "Method Not Allowed"
        _ -> handle id do
            return $ Wai.responseLBS HTTP.notFound404 [] "Not Found"

ex1 :: Wai.Application
ex1 = match \case
    "hello" : Param @Int age : "blu" : End -> ex2
    _ -> match \case
        GET -> handle id do
            return $ Wai.responseLBS HTTP.ok200 [] "Hello"
        _ -> ex3

ex2 :: Wai.Application
ex2 req send = case path req of
    "hello" : Param @Int name : End -> case method req of
        GET -> ex3 req send
        _ -> undefined
    _ -> undefined

ex3 :: Wai.Application
ex3 = match \case
    Req{method = GET, path = "hello" : "world" : Param @Int age : End} -> do
        undefined
    Req{ssl = True} ->
        undefined
    _ -> undefined

pattern HelloWorldRoute :: Int -> Path -> Path
pattern HelloWorldRoute age subPath = "hello" : "world" : Param @Int age : subPath

ex8 :: Wai.Application
ex8 = match \case
    Req{method = GET, path = HelloWorldRoute age subPath} -> case subPath of
        End -> undefined
        _ -> undefined
    Req{ssl = True} ->
        undefined
    _ -> undefined
