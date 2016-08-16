{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Support for reading from/to JSON via the Aeson package.

module Labels.Aeson where

import           Data.Aeson
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits
import           Labels.Internal
import           Language.Haskell.TH

$(let makeInstance :: Int -> Q Dec
      makeInstance size =
          instanceWithOverlapD
              (Just Overlapping)
              context
              (appT (conT ''FromJSON) instHead)
              [ funD
                    'parseJSON
                    [clause [varP j_var]
                            (normalB [|do hash <- parseJSON $(varE j_var)
                                          $(tuplize (map (getter 'hash) [1::Int .. size])) |]) []]]
        where
          l_tyvar j = mkName ("l" ++ show j)
          v_tyvar j = mkName ("v" ++ show j)
          j_var = mkName "j"
          context =
              return
                  (concat
                       (map
                            (\i ->
                                  [AppT (ConT ''KnownSymbol) (VarT (l_tyvar i))
                                  ,AppT (ConT ''FromJSON) (VarT (v_tyvar i))])
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
          getter o (j::Int) =
              [|let proxy = Proxy :: Proxy $(varT (l_tyvar j))
                in do v <- $(varE o) .: (T.pack (symbolVal proxy))
                      return (proxy := v)|]
  in mapM makeInstance [1..24])
