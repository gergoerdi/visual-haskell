{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Vis.Node (
  Name(..), BuiltinFun(..), 
  Node(..), Alt(..), Payload(..),
  Serial(unSerial), firstSerial
  ) where

import Language.Haskell.Syntax (HsName(..), HsSpecialCon, HsPat)
import Data.STRef
import Data.Function (on)
import Language.Haskell.Pretty

data Name = Name HsName
          | Special HsSpecialCon
          deriving (Eq, Ord)

instance (Show Name) where
  show (Name name) = prettyPrint name
  show (Special special) = show special

newtype Serial = Serial { unSerial :: Int } deriving (Show, Eq, Ord, Enum)
firstSerial = Serial 0

data Node s = Node { nodeSerial :: Serial, 
                     nodePayload :: STRef s (Payload (Node s)) }

data Alt node = Alt { altPatterns :: [HsPat],
                      altBody :: node }
              deriving Functor
               
data BuiltinFun = IntPlus
                | IntMinus
                deriving Show
               
data Payload node = Uninitialized
                  | ParamRef Name
                  | IntLit Integer
                  | App node node
                  | BuiltinFunApp BuiltinFun [node]
                  | CaseApp Int [Alt node] [node]
                  | ConApp Name [node]
                  deriving Functor

instance Eq (Node s) where
  (==) = (==) `on` nodeSerial

instance Ord (Node s) where
  compare = compare `on` nodeSerial
                     
