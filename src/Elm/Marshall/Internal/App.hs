{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm.Marshall.Internal.App
    ( ElmApp
    , fromGlobalApp
    , createEmbeddedMainApp
    , createFullScreenMainApp
    , assignPortListener
    , assignJSONPortListener
    , assignJSONStringPortListener
    , sendSubscriptionObject
    , sendJSONSubscriptionObject
    ) where

import           "this" Elm.Marshall.Internal.Class
import           "this" Elm.Marshall.Internal.Type
import           "this" Elm.Marshall.Internal.Aeson

import qualified "aeson" Data.Aeson as AE
import           "ghcjs-base" GHCJS.Types ( JSVal, jsval, isNull )
import           "ghcjs-ffiqq" GHCJS.Foreign.QQ
import qualified "ghcjs-base" GHCJS.Foreign.Callback as F

-- | Represents an Elm application.
newtype ElmApp = ElmApp { unElmApp :: JSVal }

-- | Get an Elm app reference from a global variable Use this if the elm app is
-- defined elsewhere in the html file that also includes ghcjs. Make sure this
-- function is run after the app is defined of course. See https://guide.elm-
-- lang.org/interop/javascript.html#step-1-embed-in-html
fromGlobalApp :: String -> IO ElmApp
fromGlobalApp appName = [js| window[ `appName ] |] >>= pure . ElmApp

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

-- | Listen to an Elm port (Cmd) for data. With this, elm can communicate to
-- ghcjs See https://guide.elm-lang.org/interop/javascript.html#ports
assignPortListener :: ElmMarshall a => ElmApp -> String -> (a -> IO ()) -> IO ()
assignPortListener app portName callback =
    assignPortListener' app portName callback'
  where
    callback' :: JSVal -> IO ()
    callback' jsv = fromElm (ElmValue jsv) >>= callback

-- | Same as 'assignPortListener', but listens to JSON data from the port
-- instead. At the Elm side, the port will be sending Json.Values. Use this
-- for types that aren't allowed to go through ports directly (e.g. union
-- types).
assignJSONPortListener
    :: AE.FromJSON a
    => ElmApp
    -> String
    -> (a -> IO ())
    -> IO ()
assignJSONPortListener app portName callback =
    assignPortListener' app portName callback'
  where
    callback' :: JSVal -> IO ()
    callback' jsv = fromElm_json (ElmValue jsv) >>= callback


-- | Same as 'assignJSONPortListener', but listens to JSON _strings_ from the
-- port instead. At the Elm side, the port will be sending Strings. The reason
-- for this is because json seems to be giving weird errors with JSVal
-- objects. See https://github.com/ghcjs/ghcjs-base/issues/88
assignJSONStringPortListener
    :: AE.FromJSON a
    => ElmApp
    -> String
    -> (a -> IO ())
    -> IO ()
assignJSONStringPortListener app portName callback =
    assignPortListener' app portName callback'
  where
    callback' :: JSVal -> IO ()
    callback' jsv = fromElm_jsonstr (ElmValue jsv) >>= callback


assignPortListener' :: ElmApp -> String -> (JSVal -> IO ()) -> IO ()
assignPortListener' app portName callback = do
    portCallback <- F.asyncCallback1 $ callback
    let jsvPortCallback = jsval portCallback
    [jsu_| `app1.ports[ `portName ].subscribe( `jsvPortCallback ); |]

  where
    app1 :: JSVal
    app1 = unElmApp app


-- | Send data back to elm through a Sub(scription) port.
-- See https://guide.elm-lang.org/interop/javascript.html#ports
sendSubscriptionObject :: ElmMarshall a => ElmApp -> String -> a -> IO ()
sendSubscriptionObject app subscriptionName value = do
    value1 <- toElm value
    sendSubscriptionObject' app subscriptionName value1


-- | Same as 'sendSubscriptionObject', but sends JSON values through the port
-- instead. At the Elm side, the port will be receiving Json.Values.Use this
-- for types that aren't allowed to go through ports directly (e.g. union
-- types).
sendJSONSubscriptionObject :: AE.ToJSON a => ElmApp -> String -> a -> IO ()
sendJSONSubscriptionObject app subscriptionName value = do
    value1 <- toElm_json value
    sendSubscriptionObject' app subscriptionName value1


sendSubscriptionObject' :: ElmApp -> String -> ElmValue a -> IO ()
sendSubscriptionObject' app subscriptionName value =
    [js_| `app1.ports[ `subscriptionName ].send( `value1 ); |]
  where
    value1 :: JSVal
    value1 = unElmValue value

    app1 :: JSVal
    app1 = unElmApp app
