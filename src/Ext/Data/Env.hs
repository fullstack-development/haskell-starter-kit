module Ext.Data.Env
  ( Env(..)
  ) where

data Env
  = Prod
  | Test
  | Dev
  deriving (Eq)

instance Show Env where
  show Prod = "prod"
  show Test = "test"
  show Dev = "dev"

instance Read Env where
  readsPrec _ "prod" = [(Prod, "")]
  readsPrec _ "test" = [(Test, "")]
  readsPrec _ "dev" = [(Dev, "")]
  readsPrec _ _ = []
