{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Matcha (
    someFunc,
    Request (..),
) where

import Network.Wai.Internal (Request (..))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
