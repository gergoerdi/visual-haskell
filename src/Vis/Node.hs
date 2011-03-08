{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Vis.Node (
  Name(..), BuiltinFun(..), 
  CNode(..), FNode(..),
  Bind(..), Alt(..), Payload(..),
  FName(..),
  Serial(unSerial), firstSerial
  ) where

import Language.Haskell.Syntax (HsName(..), HsSpecialCon, HsPat)
import Data.STRef
import Data.Function (on)
import Data.Foldable
import Data.Traversable
import Language.Haskell.Pretty

data Name = Name HsName
          | Special HsSpecialCon
          deriving (Eq, Ord)

instance (Show Name) where
  show (Name name) = prettyPrint name
  show (Special special) = show special

newtype Serial = Serial { unSerial :: Int } deriving (Show, Eq, Ord, Enum)
firstSerial = Serial 0

-- A node in the cyclic representation
data CNode s = CNode { cnodeSerial :: Serial, 
                       cnodeName :: Maybe Name,
                       cnodePayload :: STRef s (Payload (CNode s)) }
               
-- A node in the flattened representation               
data FNode = FNode (Payload FNode)
           | FLet [Bind] FNode
           | FVarRef FName
           deriving Show
                    
type Pat = HsPat
                    
data Bind = Bind FName FNode
          deriving Show

data FName = Generated Serial
           | Given Name
           deriving Show

data Alt node = Alt { altPatterns :: [Pat],
                      altBody :: node }
              deriving (Show, Functor, Foldable, Traversable)
               
data BuiltinFun = IntPlus
                | IntMinus
                deriving Show
               
data Payload node = Uninitialized
                  | Knot node
                  | ParamRef Name
                  | IntLit Integer
                  | App node node
                  | BuiltinFunApp BuiltinFun [node]
                  | CaseApp Int [Alt node] [node]
                  | ConApp Name [node]
                  deriving (Show, Functor, Foldable, Traversable)

instance Eq (CNode s) where
  (==) = (==) `on` cnodeSerial

instance Ord (CNode s) where
  compare = compare `on` cnodeSerial
                     
