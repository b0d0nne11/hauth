#!/usr/bin/env stack
{- stack
     --resolver lts-6.23
     --install-ghc
     runghc
     --package :hauth-test
-}
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
