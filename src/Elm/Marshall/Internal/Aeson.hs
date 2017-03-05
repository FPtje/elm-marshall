{-# LANGUAGE PackageImports       #-}

module Elm.Marshall.Internal.Aeson where

import           "this" Elm.Marshall.Internal.Class
import           "this" Elm.Marshall.Internal.Type

import qualified "aeson" Data.Aeson as AE
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BSL
import           "ghcjs-base" GHCJS.Types ( JSVal )
import qualified "ghcjs-base" Data.JSString as JSS
import           "ghcjs-base" Data.JSString ( JSString )
import           "ghcjs-base" GHCJS.Marshal ( fromJSValUnchecked, toJSVal )

toElm_json :: AE.ToJSON a => a -> IO (ElmValue AE.Value)
toElm_json x = ElmValue <$> toJSVal (AE.toJSON x)

fromElm_json :: AE.FromJSON a => ElmValue AE.Value -> IO a
fromElm_json jsv = do
    x <- (fromJSValUnchecked $ unElmValue jsv) :: IO AE.Value
    let AE.Success res = AE.fromJSON x

    pure $ res

fromElm_jsonstr :: AE.FromJSON a => ElmValue JSString -> IO a
fromElm_jsonstr jsv = do
    x <- (fromJSValUnchecked $ unElmValue jsv) :: IO JSString
    let mRes = AE.eitherDecode' $ BSL.pack $ JSS.unpack x

    case mRes of
        Left err -> error err
        Right x -> pure x

