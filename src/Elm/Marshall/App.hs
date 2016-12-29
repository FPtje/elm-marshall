{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm.Marshall.App
    ( ElmApp
    , fromGlobal
    ) where

import           "ghcjs-base" GHCJS.Types ( JSVal, jsval, isNull )
import           "ghcjs-ffiqq" GHCJS.Foreign.QQ
import           "this" Elm.Marshall.Class
import qualified "ghcjs-base" GHCJS.Foreign.Callback as F


newtype ElmApp = ElmApp { unElmApp :: JSVal }

-- | Get an Elm app reference from a global variable
-- Use this if the elm app is defined elsewhere in the html file that also includes ghcjs.
-- Make sure this function is run after the app is defined of course.
-- See https://guide.elm-lang.org/interop/javascript.html#step-1-embed-in-html
fromGlobal :: String -> IO ElmApp
fromGlobal appName = [js| window[ `appName ] |] >>= pure . ElmApp

-- | Create an embedded Elm app.
-- Requires the `id` of some `div` element.
-- Assumes the Elm module is called Main.
createEmbeddedMainApp :: String -> IO ElmApp
createEmbeddedMainApp divId = [js| Elm.Main.embed(`divId) |] >>= pure . ElmApp

-- | Create a fullscreen Elm app.
-- Assumes the Elm module is called Main.
createFullScreenMainApp :: IO ElmApp
createFullScreenMainApp = [js| Elm.Main.fullscreen() |] >>= pure . ElmApp

-- TODO: Non-Main Elm module support

-- | Listen to an Elm port (Cmd) for data. With this, elm can communicate to ghcjs
-- See https://guide.elm-lang.org/interop/javascript.html#ports
assignPortListener :: ElmMarshall a => ElmApp -> String -> (a -> IO ()) -> IO ()
assignPortListener app portName callback = do
    portCallback <- F.asyncCallback1 $ callback'
    let jsvPortCallback = jsval portCallback
    [jsi_| `app1.ports[ `portName ].subscribe( `jsvPortCallback ); |]

  where
    app1 :: JSVal
    app1 = unElmApp app

    callback' :: JSVal -> IO ()
    callback' jsv = fromElm jsv >>= callback


-- | Send data back to elm through a Sub(scription) port.
-- See https://guide.elm-lang.org/interop/javascript.html#ports
sendSubscriptionObject :: ElmMarshall a => ElmApp -> String -> a -> IO ()
sendSubscriptionObject app subscriptionName value = do
    value1 <- toElm value
    [js_| `app1.ports[ `subscriptionName ].send( `value1 ); |]
  where
    app1 :: JSVal
    app1 = unElmApp app
