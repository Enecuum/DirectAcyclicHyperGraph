{-# LANGUAGE TemplateHaskell #-}

-- :set -XTemplateHaskell

module Data.Hyper.Template.Templates where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- import HyperGraphTypes

-- template1 :: a -> HGr -> HGr

-- template2 :: Super
genSuper :: Name -> Q [Dec]
genSuper t = do undefined
