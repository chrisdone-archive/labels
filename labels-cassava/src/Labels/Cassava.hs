{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provide instances of FromNamedRecord for named tuples up to 24 fields.
--
-- Import like: @import Labels.Cassava ()@
--

module Labels.Cassava where

import qualified Data.ByteString.Char8 as S8
import           Data.Csv
import qualified Data.HashMap.Strict as M
import           Data.Proxy
import           GHC.TypeLits
import           Labels
import           Language.Haskell.TH

$(let makeInstance :: Int -> Q Dec
      makeInstance size =
          instanceD
              context
              (appT (conT ''FromNamedRecord) instHead)
              [ funD
                    'parseNamedRecord
                    [clause [varP hash_var] (normalB (tuplize (map getter [1::Int .. size]))) []]]
        where
          l_tyvar j = mkName ("l" ++ show j)
          v_tyvar j = mkName ("v" ++ show j)
          hash_var = mkName "hash"
          context =
              return
                  (concat
                       (map
                            (\i ->
                                  [AppT (ConT ''KnownSymbol) (VarT (l_tyvar i))
                                  ,AppT (ConT ''FromField) (VarT (v_tyvar i))])
                            [1 .. size]))
          instHead =
              foldl
                  appT
                  (tupleT size)
                  (map
                       (\j ->
                             appT
                                 (appT (conT ''(:=)) (varT (l_tyvar j)))
                                 (varT (v_tyvar j)))
                       [1 .. size])
          tuplize [] = fail "Need at least one field."
          tuplize [j] = j
          tuplize js = foldl (\acc (i,g) -> infixApp acc (varE (if i == 1
                                                                   then '(<$>)
                                                                   else '(<*>))) g)
                             tupSectionE
                             (zip [1::Int ..] js)
          tupSectionE  =
            lamE (map (varP.var) [1..size])
                 (tupE (map (varE.var) [1..size]))
            where var i = mkName ("t" ++ show i)
          getter (j::Int) =
              [|let proxy = Proxy :: Proxy $(varT (l_tyvar j))
                in case M.lookup (S8.pack (symbolVal proxy)) $(varE hash_var) of
                       Nothing -> fail ("Missing field " ++ symbolVal proxy)
                       Just v -> fmap (proxy :=) (parseField v)|]
  in mapM makeInstance [1..24])
