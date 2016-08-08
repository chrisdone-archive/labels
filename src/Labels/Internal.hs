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
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides a way to name the fields in a regular
-- Haskell tuple and then look them up later, statically.

module Labels.Internal where

import Data.Data
import Data.String
import GHC.OverloadedLabels
import GHC.TypeLits
import Language.Haskell.TH

-- | Field named @l@ labels value of type @t@.
-- Example: @(#name := \"Chris\") :: (\"name\" := String)@
data label := value = KnownSymbol label => Proxy label := value
deriving instance Typeable (:=)
deriving instance Typeable (label := value)
infix 6 :=

instance (Eq value) => Eq (label := value) where
  _ := x == _ := y = x == y

instance (Ord value) => Ord (label := value) where
  compare (_ := x) (_ := y) = x `compare` y

instance (Show t) => Show (l := t) where
  show (l := t) = "#" ++ (symbolVal l) ++ " := " ++ show t

class Has (label :: Symbol) record value | label record -> value where
  -- | Get a field by doing: @get #salary employee@
  get :: Proxy label -> record -> value
  -- | Set a field by doing: @set #salary 54.00 employee@
  set :: Proxy label -> value -> record -> record

-- | Modify a field by doing: @modify #salary (* 1.1) employee@
modify :: Has label record t => Proxy label -> (t -> t) -> record -> record
modify f g r = set f (g (get f r)) r

class Cons label value record where
  type Consed label value record
  -- | Cons a field onto a record by doing: @cons (#foo := 123) record@
  cons :: (label := value) -> record -> Consed label value record

instance Cons label value () where
  type Consed label value () = (label := value)
  cons field () = field

instance Cons label value (label' := value') where
  type Consed label value (label' := value') = (label := value,label' := value')
  cons field field2 = (field,field2)

$(let makeInstance size =
        [d|instance Cons $(varT label_tyvar) $(varT value_tyvar) $tupTy where
             type Consed $(varT label_tyvar) $(varT value_tyvar) $tupTy = $newTupTy
             cons $(varP field_name) $tupPat = $tupEx|]
        where label_tyvar = mkName "label"
              value_tyvar = mkName "value"
              field_name = mkName "field"
              tupPat = tupP (map (\j -> varP (mkName ("v" ++ show j))) [1..size])
              tupEx = tupE (varE field_name : map (\j -> varE (mkName ("v" ++ show j))) [1..size])
              newTupTy =
                  foldl
                      appT
                      (tupleT (size+1))
                      ((infixT (varT label_tyvar) ''(:=) (varT value_tyvar)) :
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

instance IsString (Q Exp) where
  fromString str = [|Proxy :: Proxy $(litT (return (StrTyLit str)))|]

instance l ~ l' => IsLabel (l :: Symbol) (Proxy l') where
  fromLabel _ = Proxy

-- instance Has l r a => IsLabel (l :: Symbol) (r -> a) where
--   fromLabel _ = get @l

$(let makeInstance size slot =
          [d|instance Has $(varT l_tyvar) $instHead $(varT a_tyvar)
               where get _ = $getImpl
                     set _ = $setImpl|]
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
                                 then infixT (varT l_tyvar) ''(:=) (varT a_tyvar)
                                 else varT (mkName ("u" ++ show j)))
                       [1 .. size])
  in fmap
         (concat . concat)
         (mapM (\size -> mapM (\slot -> makeInstance size slot)
                              [1 .. size])
                [1 .. 16]))
