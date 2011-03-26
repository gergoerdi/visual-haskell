{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vis.Node (
  FPayload, FNode(..),
  Bind(..), Alt(..), Pat(..), Payload(..), 
  FName(..),
  Serial(unSerial), firstSerial
  ) where

import PrimOp
import Literal

newtype Serial = Serial { unSerial :: Int } deriving (Show, Eq, Ord, Enum)
firstSerial :: Serial
firstSerial = Serial 0

data Pat name = PConApp name [Pat name]
              | PVar name
              | PWildcard
              | PLiteral Literal
              | PAsPat name (Pat name)
              deriving Show
                    
data Bind name = Bind (FName name) (FNode name)
               deriving Show

data FName name = Generated Serial
                | Given name
                deriving Show

data Alt name node = Alt { altPattern :: Pat name,
                           altBody :: node }
              deriving Show
               
data Payload name node = Lambda [name] node
                       | ParamRef name
                       | Literal Literal
                       | App node [node]
                       | Case node [Alt name node]
                       | PrimApp PrimOp [node]
                       | ConApp name [node]
                       deriving Show
                                
type FPayload name = Payload name (FNode name)

-- A node in the flattened representation               
data FNode name = FNode (FPayload name)
                | FLet [Bind name] (FNode name)
                | FVarRef (FName name)
                deriving Show
                    
