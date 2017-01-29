{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Elm.Marshall.Internal.Class
  ( ElmMarshall(..)
  ) where

import           "this" Elm.Marshall.Internal.Type
import           "base" GHC.Generics
import           "base" Data.String ( fromString )
import           "ghcjs-base" GHCJS.Types ( JSVal, jsval, isNull )
import           "ghcjs-base" GHCJS.Foreign ( toJSBool, fromJSBool, jsNull )
import           "ghcjs-base" GHCJS.Marshal ( fromJSValUnchecked, toJSVal )
import qualified "ghcjs-base" JavaScript.Array as A ( fromList, toList, (!) )
import           "ghcjs-base" JavaScript.Array.Internal ( JSArray, SomeJSArray(..) )
import           "ghcjs-base" Data.JSString ( pack, unpack )
import           "ghcjs-base" Data.JSString.Text ( textFromJSVal, textToJSString )
import qualified "ghcjs-base" JavaScript.Object as Obj
import           "ghcjs-base" JavaScript.Object.Internal ( Object(..) )
import qualified "text" Data.Text as T


------------------------------------------------------------

-- | The main class that provides marshalling to and from Elm
class ElmMarshall a where
  -- | Turns a value into a Javascript object that can e.g. pass through a
  -- port
  toElm   :: a -> IO (ElmValue a)

  -- | Converts a Javascript value coming from Elm into a Haskell type.
  fromElm :: ElmValue a -> IO a

  default toElm :: ElmMarshallInternal a => a -> IO (ElmValue a)
  toElm x = ElmValue <$> toElm_ x

  default fromElm :: ElmMarshallInternal a => ElmValue a -> IO a
  fromElm x = fromElm_ $ unElmValue x


class ElmMarshallInternal a where
  toElm_   :: a -> IO JSVal

  default toElm_ :: (Generic a, GenericElmMarshall (Rep a)) => a -> IO JSVal
  toElm_ = genericToElm . from

  fromElm_ :: JSVal -> IO a

  default fromElm_ :: (Generic a, GenericElmMarshall (Rep a)) => JSVal -> IO a
  fromElm_ v = genericFromElm (from (undefined :: a)) v >>= pure . to


class GenericElmMarshall f where
  genericToElm :: f a -> IO JSVal

  -- the f a here can be undefined
  genericFromElm :: f a -> JSVal -> IO (f a)

instance (Datatype d, GenericElmMarshall f) =>
         GenericElmMarshall (D1 d f) where
  genericToElm datatype = genericToElm $ unM1 datatype
  genericFromElm datatype jsv = M1 <$> genericFromElm (unM1 datatype) jsv

instance (Constructor c, GenericElmMarshallSelector f) =>
         GenericElmMarshall (C1 c f) where
  genericToElm constructor = do
      obj <- Obj.create

      fields <- genericToElmSelector $ unM1 constructor
      mapM_ (\(name, val) -> Obj.setProp (fromString name) val obj) fields

      pure $ jsval obj

  genericFromElm constructor jsv =
      M1 <$> genericFromElmSelector (unM1 constructor) (Object jsv)

class GenericElmMarshallSelector f where
  genericToElmSelector :: f a -> IO [(String, JSVal)]
  genericFromElmSelector :: f a -> Obj.Object -> IO (f a)

instance (Selector ('MetaSel ('Just u) v w x), GenericElmMarshall a) =>
         GenericElmMarshallSelector (S1 ('MetaSel ('Just u) v w x) a) where
  genericToElmSelector selector =
      case selName selector of
        name -> do
          val <- genericToElm $ unM1 selector
          pure [(name, val)]

  genericFromElmSelector selector obj =
      case selName selector of
        name -> do
          prop <- Obj.getProp (fromString name) obj
          val <- genericFromElm (unM1 selector) prop
          pure $ M1 val


instance (GenericElmMarshallSelector f, GenericElmMarshallSelector g) =>
         GenericElmMarshallSelector (f :*: g) where
  genericToElmSelector (l :*: r) =
      (++) <$> genericToElmSelector l <*> genericToElmSelector r

  genericFromElmSelector (l :*: r) obj =
      (:*:) <$> genericFromElmSelector l obj <*> genericFromElmSelector r obj


instance ElmMarshallInternal a => GenericElmMarshall (Rec0 a) where
  genericToElm rec = toElm_ $ unK1 rec

  genericFromElm rec val = K1 <$> fromElm_ val

--------------


instance ElmMarshallInternal Bool where
  toElm_   = pure . toJSBool
  fromElm_ = pure . fromJSBool

instance ElmMarshallInternal Float where
  toElm_   = toJSVal
  fromElm_ = fromJSValUnchecked

instance ElmMarshallInternal Double where
  toElm_   = toJSVal
  fromElm_ = fromJSValUnchecked

instance ElmMarshallInternal Int where
  toElm_   = toJSVal
  fromElm_ = fromJSValUnchecked

instance ElmMarshallInternal [Char] where
  toElm_     = toJSVal . pack
  fromElm_ x = fromJSValUnchecked x >>= fromJSValUnchecked

instance ElmMarshallInternal T.Text where
  toElm_   = toJSVal . textToJSString
  fromElm_ = pure . textFromJSVal

instance ElmMarshallInternal a => ElmMarshallInternal [a] where
  toElm_ xs = do
      xs' <- mapM toElm_ xs
      pure $ jsval $ A.fromList xs'

  fromElm_ xs =
      mapM fromElm_ $ A.toList $ SomeJSArray xs

instance ElmMarshallInternal a => ElmMarshallInternal (Maybe a) where
  toElm_ (Just x) = toElm_ x
  toElm_ Nothing  = pure $ jsNull

  fromElm_ x =
      if isNull x then
        pure Nothing
      else
        fromElm_ x

instance ElmMarshallInternal JSVal where
  toElm_ = pure
  fromElm_ = pure

instance ElmMarshallInternal () where
  toElm_ () = toElm_ ([] :: [Int])
  fromElm_ x = pure ()

instance (ElmMarshallInternal a, ElmMarshallInternal b) => ElmMarshallInternal (a, b) where
  toElm_ (a, b) = do
      a' <- toElm_ a
      b' <- toElm_ b
      toElm_ [a', b']

  fromElm_ x = do
      a' <- fromElm_ $ xArr A.! 0
      b' <- fromElm_ $ xArr A.! 1

      pure $ (a', b')
    where
      xArr :: JSArray
      xArr = SomeJSArray x

instance (ElmMarshallInternal a, ElmMarshallInternal b, ElmMarshallInternal c) =>
         ElmMarshallInternal (a, b, c) where
  toElm_ (a, b, c) = do
      a' <- toElm_ a
      b' <- toElm_ b
      c' <- toElm_ c
      toElm_ [a', b', c']

  fromElm_ x = do
      a' <- fromElm_ $ xArr A.! 0
      b' <- fromElm_ $ xArr A.! 1
      c' <- fromElm_ $ xArr A.! 2

      pure $ (a', b', c')
    where
      xArr :: JSArray
      xArr = SomeJSArray x

instance (ElmMarshallInternal a, ElmMarshallInternal b, ElmMarshallInternal c, ElmMarshallInternal d) =>
         ElmMarshallInternal (a, b, c, d) where
  toElm_ (a, b, c, d) = do
      a' <- toElm_ a
      b' <- toElm_ b
      c' <- toElm_ c
      d' <- toElm_ d
      toElm_ [a', b', c', d']

  fromElm_ x = do
      a' <- fromElm_ $ xArr A.! 0
      b' <- fromElm_ $ xArr A.! 1
      c' <- fromElm_ $ xArr A.! 2
      d' <- fromElm_ $ xArr A.! 3

      pure $ (a', b', c', d')
    where
      xArr :: JSArray
      xArr = SomeJSArray x


