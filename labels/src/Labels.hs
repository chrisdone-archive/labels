{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
-- | Labels for fields in a tuple.
--
-- Enable these extensions:
--
-- In GHCi:
--
-- @
-- :set -XOverloadedLabels -XTypeOperators -XDataKinds -XFlexibleContexts
-- @
--
-- In a module:
--
-- @
-- {-\# LANGUAGE OverloadedLabels, TypeOperators, DataKinds, FlexibleContexts \#-}
-- @
--
-- Construct a record:
--
-- >>> (#foo := "hi", #bar := 123)
-- (#foo := "hi",#bar := 123)
--
-- Get fields of a record:
--
-- >>> get #bar (#foo := "hi", #bar := 123)
-- 123
--
-- Set fields of a record:
--
-- >>> set #bar 66 (#foo := "hi", #bar := 123)
-- (#foo := "hi",#bar := 66)
--
-- Modify fields of a record:
--
-- >>> modify #mu (*0.1) (#bar := "hi", #mu := 123)
-- (#bar := "hi",#mu := 12.3)
--
-- Add fields to a record:
--
-- >>> cons (#mu := [1,2,3]) (#foo := "hi", #bar := 123)
-- (#mu := [1,2,3],#foo := "hi",#bar := 123)
--
-- Abstraction:
--
-- >>> let double field record = set field (get field record * 2) record
-- >>> double #mu (#bar := "hi", #mu := 123)
-- (#bar := "hi",#mu := 246)
--
-- Lenses:
-- >>> over (lens #sub . lens #foo) (*2) (#bar := "hello", #sub := (#foo := 123))
-- (#bar := "hello",#sub := #foo := 246)

module Labels
-- Field access
  ( get
  , set
  , modify
  , lens
  , cons
  , project
   -- Construction
  , (:=)(..)
  , Has
  , Cons
  , Project)
  where

import Data.Proxy
import GHC.TypeLits
import Labels.Internal

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- | Make a lens out of the label. Example: @over (lens #salary) (* 1.1) employee@
lens
  :: Has label value record
  => Proxy (label :: Symbol) -> Lens record record value value
lens label f record =
  fmap (\value -> set label value record) (f (get label record))

-- | Strictly modify a field by doing: @modify #salary (* 1.1) employee@
modify :: Has label value record => Proxy label -> (value -> value) -> record -> record
modify f g r = set f (g v) r
  where !v = get f r
{-# INLINE modify #-}
