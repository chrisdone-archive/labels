{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provide instances of FromNamedRecord for named tuples up to 24 fields.
--
-- Also provides helpful functions for reading them.
--
-- Import like: @import Labels.Cassava@
--

module Labels.CSV where

import Labels.Cassava.Instances ()
