{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Matcha
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
    putStrLn "Running app..."
    Warp.run 3000 $ match \case
        "hello" : "world" : End -> match \case
            GET -> respond $ Wai.responseLBS HTTP.ok200 [] "Hello world!"
            _ -> respond $ Wai.responseLBS HTTP.methodNotAllowed405 [] "Method Not Allowed"
        End -> respond $ Wai.responseLBS HTTP.ok200 [("Content-Type", "text/html")] "&#127861"
        _ -> respond $ Wai.responseLBS HTTP.notFound404 [] "Not Found"

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
