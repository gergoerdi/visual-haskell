module Vis.Node where

import Language.Haskell.Syntax (HsName)
import Data.STRef
import Data.Function (on)

type Name = HsName

data Node s = Node { nodeSerial :: Int, 
                     nodePayload :: STRef s (Payload s) }

data Payload s = Uninitialized
               | VarRef Name
               | IntLit Integer
               | App (Node s) (Node s)
               | PartialFunApp Name [Node s]               
               | PartialConApp Name [Node s]

instance Eq (Node s) where
  (==) = (==) `on` nodeSerial

instance Ord (Node s) where
  compare = compare `on` nodeSerial
                     
