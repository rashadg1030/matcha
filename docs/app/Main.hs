{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Commonmark
import Data.Foldable (fold)
import Data.String.QQ
import Data.Text.IO as Text
import Data.Text.Lazy.IO as TextL

main :: IO ()
main = do
  docsText <- Text.readFile "src/docs.md"
  let docsHtmlResult = commonmark "docs" docsText
  case docsHtmlResult of
    Left error -> Prelude.putStrLn (show error)
    Right (docsHtml :: Html ()) -> TextL.writeFile "dist/docs.html" $ renderHtml $ wrapper docsHtml

wrapper :: Html a -> Html a
wrapper innerHtml =
  fold
    [ htmlRaw
        [s|
          <!DOCTYPE html>
          <html lang="en">
            <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <meta http-equiv="X-UA-Compatible" content="ie=edge">
              <title>matcha Docs</title>
              <link rel="icon" href="matcha-blob.svg">
              <link rel="stylesheet" href="./docs.css">
              <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/sunburst.min.css">
              <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>

              <!-- and it's easy to individually load additional languages -->
              <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/haskell.min.js"></script>

              <script>hljs.highlightAll();</script>
              <script defer src="./docs.js"></script>
            </head>
            <body>
              <main>
                <article>
                  <header>
                    <h1>
                      matcha
                    </h1>
                    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 200" height="80px">
                        <defs>
                          <linearGradient id="gradient" x1="0" x2="0" y1="1" y2="0">
                            <stop stop-color="rgba(105,191,100,1)" offset="0%" />
                            <stop stop-color="rgba(185,220,169,1)" offset="100%" />
                          </linearGradient>
                        </defs>
                      <path fill="url(#gradient)" d="M71.4,-23.2C80.1,3.4,65.9,37.3,40.8,55.5C15.6,73.6,-20.5,75.8,-44.5,58.9C-68.4,42.1,-80.1,6.1,-70.7,-21.5C-61.2,-49.2,-30.6,-68.6,0.4,-68.7C31.4,-68.8,62.8,-49.7,71.4,-23.2Z" transform="translate(100 100)"/>
                    </svg>
                  </header>
        |]
    , innerHtml
    , htmlRaw
        [s|
                  <footer>
                    <p>Powered by </p>
                    <img src="./Haskell-Logo.svg" height="30px"/>
                  </footer>
                </article>
              </main>
            </body>
          </html>
        |]
    ]
