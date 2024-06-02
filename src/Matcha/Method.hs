{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Matcha.Method where

import Network.HTTP.Types (Method)

pattern GET :: Method
pattern GET = "GET"

pattern POST :: Method
pattern POST = "POST"