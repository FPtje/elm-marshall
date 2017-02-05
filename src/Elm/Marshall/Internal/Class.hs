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

  default toElm :: (Generic a, GenericElmMarshall (Rep a)) => a -> IO (ElmValue a)
  toElm x = ElmValue <$> (genericToElm $ from x)

  default fromElm :: (Generic a, GenericElmMarshall (Rep a)) => ElmValue a -> IO a
  fromElm x = genericFromElm (unElmValue x) >>= pure . to


class GenericElmMarshall f where
  genericToElm :: f a -> IO JSVal

  genericFromElm :: JSVal -> IO (f a)

instance (Datatype d, GenericElmMarshall f) =>
         GenericElmMarshall (D1 d f) where
  genericToElm datatype = genericToElm $ unM1 datatype
  genericFromElm jsv = M1 <$> genericFromElm jsv

instance (Constructor c, GenericElmMarshallSelector f) =>
         GenericElmMarshall (C1 c f) where
  genericToElm constructor = do
      obj <- Obj.create

      fields <- genericToElmSelector $ unM1 constructor
      mapM_ (\(name, val) -> Obj.setProp (fromString name) val obj) fields

      pure $ jsval obj

  genericFromElm jsv = M1 <$> genericFromElmSelector (Object jsv)

class GenericElmMarshallSelector f where
  genericToElmSelector :: f a -> IO [(String, JSVal)]
  genericFromElmSelector :: Obj.Object -> IO (f a)

instance (Selector ('MetaSel ('Just u) v w x), GenericElmMarshall a) =>
         GenericElmMarshallSelector (S1 ('MetaSel ('Just u) v w x) a) where
  genericToElmSelector selector =
      case selName selector of
        name -> do
          val <- genericToElm $ unM1 selector
          pure [(name, val)]

  genericFromElmSelector obj =
      case selName (undefined :: t ('MetaSel ('Just u) v w x) a p) of
        name -> do
          prop <- Obj.getProp (fromString name) obj
          val <- genericFromElm prop
          pure $ M1 val


instance (GenericElmMarshallSelector f, GenericElmMarshallSelector g) =>
         GenericElmMarshallSelector (f :*: g) where
  genericToElmSelector (l :*: r) =
      (++) <$> genericToElmSelector l <*> genericToElmSelector r

  genericFromElmSelector obj =
      (:*:) <$> genericFromElmSelector obj <*> genericFromElmSelector obj


instance ElmMarshall a => GenericElmMarshall (Rec0 a) where
  genericToElm rec = unElmValue <$> (toElm $ unK1 rec)

  genericFromElm val =
    K1 <$> fromElm (ElmValue val)

--------------


instance ElmMarshall Bool where
  toElm   = pure . ElmValue . toJSBool
  fromElm = pure . fromJSBool . unElmValue

instance ElmMarshall Float where
  toElm   = fmap ElmValue . toJSVal
  fromElm = fromJSValUnchecked . unElmValue

instance ElmMarshall Double where
  toElm   = fmap ElmValue . toJSVal
  fromElm = fromJSValUnchecked . unElmValue

instance ElmMarshall Int where
  toElm   = fmap ElmValue . toJSVal
  fromElm = fromJSValUnchecked . unElmValue

instance {-# OVERLAPPING #-} ElmMarshall String where
  toElm     = fmap ElmValue . toJSVal . pack
  fromElm x =
    fromJSValUnchecked (unElmValue x) >>= fromJSValUnchecked

instance ElmMarshall T.Text where
  toElm   = fmap ElmValue . toJSVal . textToJSString
  fromElm = pure . textFromJSVal . unElmValue

instance ElmMarshall a => ElmMarshall [a] where
  toElm xs = do
      xs' <- mapM (fmap unElmValue . toElm) xs
      pure $ ElmValue $ jsval $ A.fromList xs'

  fromElm xs =
      mapM (fromElm . ElmValue) $ A.toList $ SomeJSArray $ unElmValue xs

instance ElmMarshall a => ElmMarshall (Maybe a) where
  toElm (Just x) = fmap (ElmValue . unElmValue) $ toElm x
  toElm Nothing  = pure $ ElmValue jsNull

  fromElm x =
      if isNull $ unElmValue x then
        pure Nothing
      else
        Just <$> fromElm (ElmValue $ unElmValue x)

instance ElmMarshall JSVal where
  toElm = pure . ElmValue
  fromElm = pure . unElmValue

instance ElmMarshall () where
  toElm () = fmap (ElmValue . unElmValue) $ toElm ([] :: [Int])
  fromElm _ = pure ()

instance (ElmMarshall a, ElmMarshall b) =>
         ElmMarshall (a, b) where
  toElm (a, b) = do
      a' <- toElm a
      b' <- toElm b
      fmap (ElmValue . unElmValue) $ toElm [unElmValue a', unElmValue b']

  fromElm x = do
      a' <- fromElm $ ElmValue $ xArr A.! 0
      b' <- fromElm $ ElmValue $ xArr A.! 1

      pure $ (a', b')
    where
      xArr :: JSArray
      xArr = SomeJSArray $ unElmValue x

instance (ElmMarshall a, ElmMarshall b, ElmMarshall c) =>
         ElmMarshall (a, b, c) where
  toElm (a, b, c) = do
      a' <- toElm a
      b' <- toElm b
      c' <- toElm c
      fmap (ElmValue . unElmValue) $ toElm [unElmValue a', unElmValue b', unElmValue c']

  fromElm x = do
      a' <- fromElm $ ElmValue $ xArr A.! 0
      b' <- fromElm $ ElmValue $ xArr A.! 1
      c' <- fromElm $ ElmValue $ xArr A.! 2

      pure $ (a', b', c')
    where
      xArr :: JSArray
      xArr = SomeJSArray $ unElmValue x

instance (ElmMarshall a, ElmMarshall b, ElmMarshall c, ElmMarshall d) =>
         ElmMarshall (a, b, c, d) where
  toElm (a, b, c, d) = do
      a' <- toElm a
      b' <- toElm b
      c' <- toElm c
      d' <- toElm d
      fmap (ElmValue . unElmValue) $ toElm [unElmValue a', unElmValue b', unElmValue c', unElmValue d']

  fromElm x = do
      a' <- fromElm $ ElmValue $ xArr A.! 0
      b' <- fromElm $ ElmValue $ xArr A.! 1
      c' <- fromElm $ ElmValue $ xArr A.! 2
      d' <- fromElm $ ElmValue $ xArr A.! 2

      pure $ (a', b', c', d')
    where
      xArr :: JSArray
      xArr = SomeJSArray $ unElmValue x


