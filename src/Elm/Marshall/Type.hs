{-# LANGUAGE PackageImports       #-}

module Elm.Marshall.Type where

import "ghcjs-base" GHCJS.Types ( JSVal, IsJSVal )

-- | Represents a value that can pass through an Elm port. It is parameterised
-- by its corresponding Haskell type to prevent different Elm values from
-- being mixed.
newtype ElmValue a = ElmValue { unElmValue :: JSVal }

instance IsJSVal (ElmValue a) where
