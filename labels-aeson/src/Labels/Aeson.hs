{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Support for reading from/to JSON via the Aeson package.

module Labels.Aeson where

import           Data.Aeson
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits
import           Labels.Internal

instance FromJSON (Labels ()) where
  parseJSON (Object _) = return (L ())
  parseJSON _ = fail "Expected object for labelled data structure."

instance (FromJSON a, KnownSymbol l) =>
         FromJSON (Labels ((l :: Symbol) := a)) where
  parseJSON (Object o) = do
      let l = Proxy :: Proxy l
      a <- o .: T.pack (symbolVal l)
      return (L (l := a))
  parseJSON _ = fail "Expected object for labelled data structure."

instance (FromJSON a, FromJSON a1,KnownSymbol l, KnownSymbol l1) =>
         FromJSON (Labels ((l :: Symbol) := a, (l1 :: Symbol) := a1)) where
  parseJSON (Object o) = do
      let l = Proxy :: Proxy l
          l1 = Proxy :: Proxy l1
      a <- o .: T.pack (symbolVal l)
      a1 <- o .: T.pack (symbolVal l1)
      return (L (l := a, l1 := a1))
  parseJSON _ = fail ("Expected object for labelled data structure.")
