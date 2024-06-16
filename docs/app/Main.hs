{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Commonmark
import Data.Text.IO as Text
import Data.Text.Lazy.IO as TextL

main :: IO ()
main = do
    res <- commonmark "docs" <$> Text.readFile "src/docs.md"
    case res of
        Left e -> error (show e)
        Right (html :: Html ()) -> TextL.writeFile "dist/docs.html" $ renderHtml html
