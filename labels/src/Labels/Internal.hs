{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE OverloadedLabels #-}
#endif
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides a way to name the fields in a regular
-- Haskell tuple and then look them up later, statically.

module Labels.Internal where

import Data.Data
import Data.String
import GHC.TypeLits
import Language.Haskell.TH

#if __GLASGOW_HASKELL__ >= 800
import GHC.OverloadedLabels
#endif

--------------------------------------------------------------------------------
-- A labelled value

-- | Field named @l@ labels value of type @t@.
-- Example: @(#name := \"Chris\") :: (\"name\" := String)@
data label := value = KnownSymbol label => Proxy label := value
deriving instance Typeable (:=)
deriving instance Typeable (label := value)
infix 6 :=

instance (Eq value) => Eq (label := value) where
  _ := x == _ := y = x == y
  {-# INLINE (==) #-}

instance (Ord value) => Ord (label := value) where
  compare (_ := x) (_ := y) = x `compare` y
  {-# INLINE compare #-}

instance (Show t) =>
         Show (l := t) where
  showsPrec p (l := t) =
    showParen (p > 10) (showString ("#" ++ (symbolVal l) ++ " := " ++ show t))

--------------------------------------------------------------------------------
-- Basic accessors

class Has (label :: Symbol) value record | label record -> value where
  -- | Get a field by doing: @get #salary employee@
  get :: Proxy label -> record -> value
  -- | Set a field by doing: @set #salary 54.00 employee@
  set :: Proxy label -> value -> record -> record

-- | Modify a field by doing: @modify #salary (* 1.1) employee@
modify :: Has label value record => Proxy label -> (value -> value) -> record -> record
modify f g r = set f (g (get f r)) r
{-# INLINE modify #-}

--------------------------------------------------------------------------------
-- Cons a field onto a record

class Cons label value record where
  type Consed label value record
  -- | Cons a field onto a record by doing: @cons (#foo := 123) record@
  cons :: (label := value) -> record -> Consed label value record

instance Cons label value () where
  type Consed label value () = (label := value)
  cons field () = field
  {-# INLINE cons #-}

instance Cons label value (label' := value') where
  type Consed label value (label' := value') = (label := value,label' := value')
  cons field field2 = (field,field2)
  {-# INLINE cons #-}

--------------------------------------------------------------------------------
-- Labels

#if __GLASGOW_HASKELL__ >= 800
instance l ~ l' =>
         IsLabel (l :: Symbol) (Proxy l') where
    fromLabel _ = Proxy
    {-# INLINE fromLabel #-}
#endif

instance IsString (Q Exp) where
  fromString str = [|Proxy :: Proxy $(litT (return (StrTyLit str)))|]

--------------------------------------------------------------------------------
-- TH-derived instances

$(let makeInstance size =
        [d|instance Cons $(varT label_tyvar) $(varT value_tyvar) $tupTy where
             type Consed $(varT label_tyvar) $(varT value_tyvar) $tupTy = $newTupTy
             cons $(varP field_name) $tupPat = $tupEx
             {-# INLINE cons #-}|]
        where label_tyvar = mkName "label"
              value_tyvar = mkName "value"
              field_name = mkName "field"
              tupPat = tupP (map (\j -> varP (mkName ("v" ++ show j))) [1..size])
              tupEx = tupE (varE field_name : map (\j -> varE (mkName ("v" ++ show j))) [1..size])
              newTupTy =
                  foldl
                      appT
                      (tupleT (size+1))
                      ((appT (appT (conT ''(:=)) (varT label_tyvar)) (varT value_tyvar)) :
                       (map
                            (\j ->
                                  varT (mkName ("u" ++ show j)))
                            [1 .. size]))
              tupTy =
                foldl
                    appT
                    (tupleT size)
                    (map
                         (\j ->
                               varT (mkName ("u" ++ show j)))
                         [1 .. size])
  in fmap concat (mapM makeInstance [2 .. 24]))

$(let makeInstance size slot =
          [d|instance Has $(varT l_tyvar) $(varT a_tyvar) $instHead
               where get _ = $getImpl
                     {-# INLINE get #-}
                     set _ = $setImpl
                     {-# INLINE set #-}
                     |]
        where
          l_tyvar = mkName "l"
          a_tyvar = mkName "a"
          getImpl =
              lamE
                  [ tupP (map (\j -> if j == slot then infixP wildP '(:=) (varP a_var) else wildP)
                              [1 .. size])]
                  (varE a_var)
            where a_var = mkName "a"
          setImpl =
            lamE
                [ varP v_var
                ,tupP
                     (map
                          (\j ->
                                if j == slot
                                   then infixP
                                            (varP (nth_proxy_var j))
                                            '(:=)
                                            wildP
                                   else varP (nth_var j))
                          [1 .. size])]
                (tupE
                      (map
                           (\j ->
                                 if j == slot
                                    then appE
                                             (appE (conE '(:=)) (varE (nth_proxy_var j)))
                                             (if j == slot
                                                  then varE v_var
                                                  else varE (nth_var j))
                                    else varE (nth_var j))
                           [1 .. size]))
            where nth_var i = mkName ("u_" ++ show i)
                  nth_proxy_var i = mkName ("p_" ++ show i)
                  v_var = mkName "v"
          instHead =
              foldl
                  appT
                  (tupleT size)
                  (map
                       (\j ->
                             if j == slot
                                 then appT (appT (conT ''(:=)) (varT l_tyvar)) (varT a_tyvar)
                                 else varT (mkName ("u" ++ show j)))
                       [1 .. size])
  in fmap
         (concat . concat)
         (mapM (\size -> mapM (\slot -> makeInstance size slot)
                              [1 .. size])
                [1 .. 24]))
