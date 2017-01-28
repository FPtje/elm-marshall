{-# LANGUAGE PackageImports       #-}

module Elm.Marshall.Aeson where

import           "this" Elm.Marshall.Class
import           "this" Elm.Marshall.Type
import qualified "aeson" Data.Aeson as AE
import           "ghcjs-base" GHCJS.Types ( JSVal )
import           "ghcjs-base" GHCJS.Marshal ( fromJSValUnchecked, toJSVal )

toElm_json :: AE.ToJSON a => a -> IO (ElmValue AE.Value)
toElm_json x = ElmValue <$> toJSVal (AE.toJSON x)

fromElm_json :: AE.FromJSON a => ElmValue AE.Value -> IO a
fromElm_json jsv = do
    x <- (fromJSValUnchecked $ unElmValue jsv) :: IO AE.Value
    let AE.Success res = AE.fromJSON x

    pure $ res

