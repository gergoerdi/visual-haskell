module Vis.Node where

import Language.Haskell.Syntax (HsName, HsSpecialCon, HsPat)
import Data.STRef
import Data.Function (on)
import Language.Haskell.Pretty

data Name = Name HsName
          | Special HsSpecialCon
          deriving (Eq, Ord)

instance (Show Name) where
  show (Name name) = prettyPrint name
  show (Special special) = show special

data Node s = Node { nodeSerial :: Int, 
                     nodePayload :: STRef s (Payload s) }

data Match s = Match { matchPatterns :: [HsPat],
                       matchBody :: Node s }
               
data Function s = BuiltinFun BuiltinFun               
                | Matches Name Int -- [Match s]

data BuiltinFun = IntPlus
                deriving Show
               
data Payload s = Uninitialized
               | ParamRef Name
               | IntLit Integer
               | App (Node s) (Node s)
               | PartialFunApp (Function s) [Node s]               
               | PartialConApp Name [Node s]

instance Eq (Node s) where
  (==) = (==) `on` nodeSerial

instance Ord (Node s) where
  compare = compare `on` nodeSerial
                     
