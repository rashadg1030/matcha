## Introduction

*matcha* is a minamalist library for implementing web servers in Haskell.

```haskell
main = serve 3000 myMatcha

myMatcha = match \case
    "hello" : "world" : End -> match \case
        GET -> respond $ Wai.responseLBS HTTP.ok200 [] "Hello world!"
        _ -> respond $ Wai.responseLBS HTTP.methodNotAllowed405 [] "Method Not Allowed"
    _ -> respond $ Wai.responseLBS HTTP.notFound404 [] "Not Found"
```

## Structure

The structure is amazing. Do:

1. Nothing
2. Something
3. IDK

## FAQ