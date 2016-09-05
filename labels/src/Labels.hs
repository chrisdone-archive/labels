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
module Labels
-- Field access
  ( get
  , set
  , modify
  , cons
   -- Construction
  , (:=)(..)
  , Has
  , Cons)
  where

import Labels.Internal