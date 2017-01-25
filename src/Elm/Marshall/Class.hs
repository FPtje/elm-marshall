{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds            #-}

module Elm.Marshall.Class where

import           "base" GHC.Generics
import           "base" Data.String ( fromString )
import           "ghcjs-base" GHCJS.Types ( JSVal, jsval, isNull )
import           "ghcjs-base" GHCJS.Foreign ( toJSBool, fromJSBool, jsNull )
import           "ghcjs-base" GHCJS.Marshal ( fromJSValUnchecked, toJSVal )
import qualified "ghcjs-base" JavaScript.Array as A ( fromList, toList, (!) )
import           "ghcjs-base" JavaScript.Array.Internal ( JSArray, SomeJSArray(..) )
import           "ghcjs-base" Data.JSString ( pack, unpack )
import           "ghcjs-base" Data.JSString.Text ( textFromJSVal, textToJSString )
import qualified "text" Data.Text as T
import qualified "containers" Data.Map as M
import qualified "ghcjs-base" JavaScript.Object as Obj
import           "ghcjs-base" JavaScript.Object.Internal (Object(..))


------------------------------------------------------------


class ElmMarshall a where
  toElm   :: a -> IO JSVal

  default toElm :: (Generic a, GenericElmMarshall (Rep a)) => a -> IO JSVal
  toElm = genericToElm . from

  fromElm :: JSVal -> IO a
  -- fromElm = genericFromElm >>= pure . to

class GenericElmMarshall f where
  genericToElm :: f a -> IO JSVal

instance (Datatype d, GenericElmMarshall f) =>
         GenericElmMarshall (D1 d f) where
  genericToElm datatype = genericToElm $ unM1 datatype

instance (Constructor c, GenericElmMarshallSelector f) =>
         GenericElmMarshall (C1 c f) where
  genericToElm constructor = do
    obj <- Obj.create

    fields <- genericToElmSelector $ unM1 constructor
    mapM_ (\(name, val) -> Obj.setProp (fromString name) val obj) fields

    pure $ jsval obj

class GenericElmMarshallSelector f where
  genericToElmSelector :: f a -> IO [(String, JSVal)]

instance (Selector ('MetaSel ('Just u) v w x), GenericElmMarshall a) =>
         GenericElmMarshallSelector (S1 ('MetaSel ('Just u) v w x) a) where
  genericToElmSelector selector =
      case selName selector of
        name -> do
          val <- genericToElm $ unM1 selector
          pure [(name, val)]


instance (GenericElmMarshallSelector f, GenericElmMarshallSelector g) =>
         GenericElmMarshallSelector (f :*: g) where
  genericToElmSelector (l :*: r) = (++) <$> genericToElmSelector l <*> genericToElmSelector r


instance ElmMarshall a => GenericElmMarshall (Rec0 a) where
  genericToElm rec = toElm $ unK1 rec

--------------


instance ElmMarshall Bool where
  toElm   = pure . toJSBool
  fromElm = pure . fromJSBool

instance ElmMarshall Float where
  toElm   = toJSVal
  fromElm = fromJSValUnchecked

instance ElmMarshall Double where
  toElm   = toJSVal
  fromElm = fromJSValUnchecked

instance ElmMarshall Int where
  toElm   = toJSVal
  fromElm = fromJSValUnchecked

instance ElmMarshall [Char] where
  toElm     = toJSVal . pack
  fromElm x = fromJSValUnchecked x >>= fromJSValUnchecked

instance ElmMarshall T.Text where
  toElm   = toJSVal . textToJSString
  fromElm = pure . textFromJSVal

instance ElmMarshall a => ElmMarshall [a] where
  toElm xs = do
      xs' <- mapM toElm xs
      pure $ jsval $ A.fromList xs'

  fromElm xs =
      mapM fromElm $ A.toList $ SomeJSArray xs

instance ElmMarshall a => ElmMarshall (Maybe a) where
  toElm (Just x) = toElm x
  toElm Nothing  = pure $ jsNull

  fromElm x =
    if isNull x then
      pure Nothing
    else
      fromElm x

instance ElmMarshall JSVal where
  toElm = pure
  fromElm = pure

instance ElmMarshall () where
  toElm () = toElm ([] :: [Int])
  fromElm x = pure ()

instance (ElmMarshall a, ElmMarshall b) => ElmMarshall (a, b) where
  toElm (a, b) = do
      a' <- toElm a
      b' <- toElm b
      toElm [a', b']

  fromElm x = do
      a' <- fromElm $ xArr A.! 0
      b' <- fromElm $ xArr A.! 1

      pure $ (a', b')
    where
      xArr :: JSArray
      xArr = SomeJSArray x

instance (ElmMarshall a, ElmMarshall b, ElmMarshall c) => ElmMarshall (a, b, c) where
  toElm (a, b, c) = do
      a' <- toElm a
      b' <- toElm b
      c' <- toElm c
      toElm [a', b', c']

  fromElm x = do
      a' <- fromElm $ xArr A.! 0
      b' <- fromElm $ xArr A.! 1
      c' <- fromElm $ xArr A.! 2

      pure $ (a', b', c')
    where
      xArr :: JSArray
      xArr = SomeJSArray x

instance (ElmMarshall a, ElmMarshall b, ElmMarshall c, ElmMarshall d) => ElmMarshall (a, b, c, d) where
  toElm (a, b, c, d) = do
      a' <- toElm a
      b' <- toElm b
      c' <- toElm c
      d' <- toElm d
      toElm [a', b', c', d']

  fromElm x = do
      a' <- fromElm $ xArr A.! 0
      b' <- fromElm $ xArr A.! 1
      c' <- fromElm $ xArr A.! 2
      d' <- fromElm $ xArr A.! 3

      pure $ (a', b', c', d')
    where
      xArr :: JSArray
      xArr = SomeJSArray x

-- instance (ElmMarshall k, Ord k, ElmMarshall v) => ElmMarshall (Map k v) where
--   toElm m = do
--       obj <- Obj.create


--   fromElm m = _


