{-# LANGUAGE PackageImports       #-}

module Elm.Marshall.Aeson where

import           "this" Elm.Marshall.Class
import qualified "aeson" Data.Aeson as AE
import           "ghcjs-base" GHCJS.Types ( JSVal )
import           "ghcjs-base" GHCJS.Marshal ( fromJSValUnchecked, toJSVal )

toElm_json :: AE.ToJSON a => a -> IO JSVal
toElm_json x = toJSVal $ AE.toJSON x

fromElm_json :: AE.FromJSON a => JSVal -> IO a
fromElm_json jsv = do
    x <- (fromJSValUnchecked jsv) :: IO AE.Value
    let AE.Success res = AE.fromJSON x

    pure $ res

