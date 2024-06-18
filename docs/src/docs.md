## Introduction

*matcha* is a minamalist library for implementing web servers in Haskell.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Matcha

main = serve 3000 myMatcha

myMatcha = match \case
    "matcha" : "please" : End -> match \case
        GET -> respond $ responseLBS ok200 [("Content-Type", "text/html")] "&#127861"
        _ -> respond $ responseLBS methodNotAllowed405 [] "Method Not Allowed"
    _ -> respond $ responseLBS notFound404 [] "Not Found"
```

*matcha* is simple. It provides 3 core abstractions for interacting with *wai* that are ubiquitous throughout Haskell:

- **Patterns** for control flow (routing) and constructing/deconstructing request data in a type-safe manner
- **Parsers** for constructing your domain specific data types from request data in a composable manner
- **Point-free functions** for defining your server in terms of pure `Application` values and focusing on immutability

This means *matcha* is easy to learn. Knowledge gained from using it is easily transferred to other domains, and vice-versa.

## Getting Started

## Web Application Interface

## Going Point-free

## Custom Patterns

## Using Parsers

## Tips & Tricks