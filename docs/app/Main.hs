{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commonmark
import Data.Text.IO as Text
import Data.Text.Lazy.IO as TextL
import Data.Foldable (fold)

main :: IO ()
main = do
    docsText <- Text.readFile "src/docs.md"
    let docsHtmlResult = commonmark "docs" docsText
    case docsHtmlResult of
        Left error -> Prelude.putStrLn (show error)
        Right (docsHtml :: Html ()) -> TextL.writeFile "dist/docs.html" $ renderHtml $ wrapper docsHtml

{-| Wrapper Template
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <title>HTML 5 Boilerplate</title>
    <link rel="stylesheet" href="style.css">
  </head>
  <body>
	<script src="index.js"></script>
    {{ innerHtml }}
  </body>
</html>
-}
wrapper :: Html a -> Html a
wrapper innerHtml = fold
    [ htmlRaw
        "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"><meta http-equiv=\"X-UA-Compatible\" content=\"ie=edge\"><title>matcha Docs</title><link rel=\"stylesheet\" href=\"docs.css\"></head><body><script src=\"docs.js\"></script>"
    , innerHtml
    , htmlRaw
        "</body></html>"
    ]
