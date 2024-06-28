## Introduction

*matcha* is a minamalist library for implementing web servers in Haskell.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Matcha

myMatcha = match \case
    "matcha" : "please" : End -> match \case
        GET -> respond $ responseLBS ok200 [("Content-Type", "text/html")] "&#127861"
        _ -> respond $ responseLBS methodNotAllowed405 [] "Method Not Allowed"
    _ -> respond $ responseLBS notFound404 [] "Not Found"

main = serve 3000 myMatcha
```

*matcha* is simple. It provides 3 core abstractions that are ubiquitous throughout Haskell for building *wai* apps:

- **Patterns** for control flow (routing) and constructing/deconstructing request data safely
- **Parsers** for constructing your domain specific data types from request data in a composable manner
- **Combinators** for defining your server in terms of pure `Application` values and encouraging an easy-to-read, tacit (point-free) programming style

<!-- This means *matcha* is easy to learn. Knowledge gained from using it is easily transferred to other domains, and vice-versa. -->
<!-- *matcha* takes advantadge of Haskell's strenghts. -->

## Getting Started

*Skip this section if you already have a Haskell environment set up on your computer*

To start building with `matcha`, you need to install `GHC`, the de facto Haskell compiler. The easiest way to do this is through [GHCup](https://www.haskell.org/ghcup/). `GHCup` will manage your `GHC` installation, as well as your `HLS` (Haskell Language Server) and `cabal` (Haskell's standard build tool and package manager) installations.

Make sure you have

- GHC, a Haskell compiler
- cabal, Haskell package manager and build tool
- HLS, the official Haskell language server (optional)

installed. Then

1. Create a new directory on your computer
2. `cd` into that directory using your terminal
3. Run the command `cabal init`

This will create a new Haskell project in your directory.

## Web Application Interface

*Skip this section if you're already familiar with the `wai` package*

Haskell's Web Application Interface, or `wai` for short, is a package you can use to implement a web server. It is the foundation of pretty much all Haskell web frameworks, including `yesod`, `servant`, and `ihp`. While most people will use one of these frameworks to implement their web server, it is totally feasible to use raw `wai` instead. In fact, one of Haskell's most widely used open-source projects, `PostgREST`, is implemented using just `wai`. Although `wai` is considered the low-level package for implementing web servers in Haskell, compared to packages in other programming languages, it is relatively high-level. The core type in `wai` is the `Application` type.

```haskell
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
```

The `Application` type is a type synonym for the type of a function that:

- Accepts a `Request` as its first argument
- Accepts an `IO` function that sends a `Response` to the client as its second argument
- Returns an `IO` action that responds to the client

Very simple, and straighforward. You can define an `Application` like so:

```haskell
myApp :: Application
myApp req send = case pathInfo req of
    ["hello", "world"] -> send $ responseLBS ok200 [] "Hello world!"
    _ -> send $ responseLBS notFound404 [] "NotFound"
```

In the above example, we have a `myApp` function that takes a `Request` (we call it `req`) and a `Response -> IO ResponseReceived` (called `send` because it sends the `Response`). Notice that in order to satisfy the `Application` type, we must return `IO ResponseReceived`. We can only do this with the `send` argument of type `Response -> IO ResponseReceived`. It is not possible to construct a `ResponseReceived` value, so we are forced to use the `send` argument to return the correct type (this is a good thing!).

You'll also notice from the above code example that we are getting a value from the `req` argument. `wai` provides various functions for accessing different parts of the `Request`. In the above example we are using `pathInfo` to inspect the path parts of the `Request`, which is just `[Text]` (a list of `Text`). We pattern match on this value to decide which `Response` we want to return to the client. It's really no different than anything else we do in Haskell.

**To implement functions in Haskell we use patterns to realize the shape of its arguments and based on their shape, we return the corresponding data.**

Here's another example:

```haskell
myOtherApp :: Application
myOtherApp req send = case pathInfo req of
    ["hello", "world"] -> case requestMethod req of
        "GET" -> send $ responseLBS ok200 [] "You got ME!"
        "DELETE" -> send $ responseLBS ok200 [] "There's nothing to DELETE!"
        _ -> send $ responseLBS methodNotAllowed405 "Method Not Allowed M8!"
    _ -> send $ responseLBS notFound404 [] "NotFound"
```

This time, we also inspect the `requestMethod` of `req`, but the idea is the same. This time we're looking at more parts of the `Request`, so we can return a more specific `Repsonse`. It comes down to pattern matching, which is the primary control flow mechanism in the Haskell programming language.

**Routing in `wai` is just a matter of pattern matching on some aspect of the `Request`, and returning the appropriate `Response`.**

If you don't believe me, [check out how PostgREST does it](https://github.com/PostgREST/postgrest/blob/main/src/PostgREST/App.hs#L105)!

*matcha* is just small layer on top of *wai* that provides some structure to your `Application`, while not impeding on the simplicity of *wai* we discussed above. One abstraction (or tool) that *matcha* exports to do this is *patterns*.

## Patterns

*Patterns* are the primary method of [*control flow*]() in Haskell.

Patterns are used for **deconstructing** and **constructing** data in Haskell.

<!-- *matcha* provides custom patterns to make it easier to do this for the `Request` path and method parts. -->

### Deconstructing

As we saw in some examples earlier with *wai*, the main thing we are doing when defining an `Application` value is pattern matching on the `Request`, and calculating a `Response` based on what is there. One part of the `Request` that is commonly looked at for routing requests is the path, which we can access using the `pathInfo` function.

The `Request` path is just a value of type `[Text]`, so we can use the various well-known list patterns to match against it.

```haskell
myApp :: Application
myApp req send = case pathInfo req of
    []             -> send $ responseLBS ok200 [] "The root path"           -- www.example.com
    [""]           -> send $ responseLBS ok200 [] "The trailing slash"      -- www.example.com/
    ["foo"]        -> send $ responseLBS ok200 [] "Foo"                     -- www.example.com/foo
    ["foo", ""]    -> send $ responseLBS ok200 [] "Foo with trailing slash" -- www.example.com/foo/
    ["foo", "bar"] -> send $ responseLBS ok200 [] "FooBar"                  -- www.example.com/foo/bar
```

We can use the `(:)` (a.k.a. `cons`) operator as well.

```haskell
myApp :: Application
myApp req send = case pathInfo req of
    []                 -> send $ responseLBS ok200 [] "The root path"           -- www.example.com
    "" : []            -> send $ responseLBS ok200 [] "The trailing slash"      -- www.example.com/
    "foo" : []         -> send $ responseLBS ok200 [] "Foo"                     -- www.example.com/foo
    "foo" : "" : []    -> send $ responseLBS ok200 [] "Foo with trailing slash" -- www.example.com/foo/
    "foo" : "bar" : [] -> send $ responseLBS ok200 [] "FooBar"                  -- www.example.com/foo/bar
    _                  -> send $ responseLBS notFound404 [] "404 Not Found"     -- every other case 
```

*matcha* provides a pattern named `End` to represent the empty list. Let's use that from now on.

```haskell
myApp :: Application
myApp req send = case pathInfo req of
    End                 -> send $ responseLBS ok200 [] "The root path"           -- www.example.com
    "" : End            -> send $ responseLBS ok200 [] "The trailing slash"      -- www.example.com/
    "foo" : End         -> send $ responseLBS ok200 [] "Foo"                     -- www.example.com/foo
    "foo" : "" : End    -> send $ responseLBS ok200 [] "Foo with trailing slash" -- www.example.com/foo/
    "foo" : "bar" : End -> send $ responseLBS ok200 [] "FooBar"                  -- www.example.com/foo/bar
    _                   -> send $ responseLBS notFound404 [] "404 Not Found"     -- every other case 
```

*Sub Routing* is also very natural.

```haskell
v1API :: Path -> Application
v1API path req send = undefined

v2API :: Path -> Application
v2API path req send = undefined

myAPI :: Application
myAPI req send = case pathInfo req of
    End              -> undefined
    "v1" : v1SubPath -> v1API v1SubPath req send
    "v2" : v2SubPath -> v2API v2SubPath req send
    _                -> undefined
```

What if we want a path parameter? Without `matcha` we need to import the `http-api-data` package to decode parts of the request path into structured Haskell values. We do this with the `parseUrlPiece` method under the `FromHttpApiData` typeclass.

To use, `parseUrPiece`, the type to be parsed needs to be an instance of the `FromHttpApiData` typeclass. Instances for common data types like `Int` are already provided.

```haskell
import Web.HttpApiData

checkAge :: Int -> ByteString
checkAge = undefined

ageCheckerApp :: Application
ageCheckerApp req send = case pathInfo req of
    "check" : ageText : End -> case parseUrlPiece ageText of
        Left _ -> undefined
        Right age -> send $ responseLBS ok200 [] $ checkAge age
    _ -> undefined
```

To get the path part after `"check"` as an `Int`, we have to pattern match against the result of `parseUrlPiece`.

*matcha* provides the `Param` pattern to do this more succinctly.

```haskell
ageCheckerApp :: Application
ageCheckerApp req send = case pathInfo req of
    "check" : Param age : End -> send $ responseLBS ok200 [] $ checkAge age
    _ -> undefined
```

Using `parseUrlPiece` is still necessary if you want to use the parse error in the calculation of a `Response`. `Param` only matches the succesful case of parsing the `Text` into the inferred data type.

Some times the target type of `parseUrlPiece` cannot be inferred by the compiler. To satisfy the compiler, use a `ScopedTypeVariable`

```haskell
ageCheckerApp :: Application
ageCheckerApp req send = case pathInfo req of
    "check" : Param (age :: Int) : End -> send $ responseLBS ok200 [] $ checkAge age
    _ -> undefined
```

, or

```haskell
ageCheckerApp :: Application
ageCheckerApp req send = case pathInfo req of
    "check" : Param age : End -> send $ responseLBS ok200 [] $ checkAge (age :: Int)
    _ -> undefined
```

, or a

```haskell
ageCheckerApp :: Application
ageCheckerApp req send = case pathInfo req of
    "check" : Param @Int age : End -> send $ responseLBS ok200 [] $ checkAge age
    _ -> undefined
```

`TypeApplication`.

Each pattern corresponds to a specific shape of the `Request` path. If a pattern matches, we specify what is sent back to the client in that case. Notice that *we must handle* every possibilty of the `Request` path. Otherwise, our code won't compile.

`matcha` also provides custom patterns for every standard method mentioned in [HTTP-RFC0007]().

With just plain `wai` you match against the `Method` type, which is a type synonym for `ByteString`, using `String` syntax (you need the `OverloadedStrings` language extension).

```haskell
petAPI :: Application
petAPI req send = case pathInfo req of
    "pets" : End -> case requestMethod req of
        "GER" -> getAllPets req send
        _ -> undefined
    "pets" : "new" : End -> case requestMethod req of
        "POST" -> newPet req send
        _ -> undefined
    _ -> undefined
```

With `matcha` we can use the `GET` and `POST` patterns respectively.

```haskell
petAPI :: Application
petAPI req send = case pathInfo req of
    "pets" : End -> case requestMethod req of
        GET -> getAllPets req send
        _ -> undefined
    "pets" : "new" : End -> case requestMethod req of
        POST -> newPet req send
        _ -> undefined
    _ -> undefined
```

This prevents the devloper from making small typos. If for some reason you need to handle a custom `Method`, just use `String` syntax to spell it out like you normally would without `matcha`.

### Constructing

Remember, not only can we use patterns for deconstructing data, we can also use them to construct data. This is especially useful for the request path because we can use the same pattern to match against and construct the same URL, given the same parameters of course.

```haskell
toUrl :: Path -> ByteString
toUrl path = undefined

myApp :: Application
myApp req send = case pathInfo req of
    "home" : End -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<h1>Welcome Home</h1>|]
    _ -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<a href={toUrl ("home" : End)}>Go Home!</a>|]
```

As you can see, it's useful for embedding links in our pages or redirecting the client. Can we guarantee that we're sending the client the correct location? No, but we can use *pattern synonyms* to reduce that chance of an invalid link or redirect. To do this, you need to have the `Pattern Synonyms` language extension turned on.

```haskell
pattern HomePath :: Path
pattern HomePath = "home" : End

myApp :: Application
myApp req send = case pathInfo req of
    HomePath -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<h1>Welcome Home</h1>|]
    _ -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<a href={toUrl HomePath}>Go Home!</a>|]
```

Notice how we're using the `HomePath` pattern to match against and construct the correct request path. *Pattern synonyms* give us a way to name a specifc pattern and use it in different contexts, while ensuring we're referring to the same pattern definition every time it's used.

It also works if you have path parameters, just use the `Param` pattern synonym provided by `matcha`. It's bi-directional.

```haskell
pattern HomePath :: Path
pattern HomePath = "home" : End

myApp :: Application
myApp req send = case pathInfo req of
    HomePath -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<h1>Welcome Home</h1>|]
    _ -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<a href={toUrl HomePath}>Go Home!</a>|]
```

It even works naturally with sub routes. Just nest your pattern synonyms!

```haskell
pattern HomePath :: Path
pattern HomePath = "home" : End

myApp :: Application
myApp req send = case pathInfo req of
    HomePath -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<h1>Welcome Home</h1>|]
    _ -> send $ responseLBS ok200 [("Content-Type", "text/html")] [qc|<a href={toUrl HomePath}>Go Home!</a>|]
```

While the level of guarantee that the link you constructed points to a valid location isn't as high as in a web framework like `servant` or `yesod`, it is easy to understand and a lot more natural to use than other mechanisms. As long as your routing logic is ordered with good sense, you can construct valid URLs with ease and without `TemplateHaskell` or *type-level programming*.

### The Req Pattern (EXPERIMENTAL)

**This feature is not a core feature and may be removed. Skip for long-term stability.**

<!--
When defining functions we need a way to 
```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
```
in case statements,
```haskell
```
-->

## Parsers

```haskell

```

## Combinators

## Tips & Tricks