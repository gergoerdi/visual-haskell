{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vis.Node (
  BuiltinFun(..), 
  CPayload, CNode(..), FPayload, FNode(..),
  Bind(..), Alt(..), Pat(..), Payload(..), Lit(..),  
  FName(..),
  Serial(unSerial), firstSerial
  ) where

import Language.Haskell.Syntax (HsName(..), HsSpecialCon, HsPat)
import Data.STRef
import Data.Function (on)
import Data.Foldable
import Data.Traversable
import Language.Haskell.Pretty

newtype Serial = Serial { unSerial :: Int } deriving (Show, Eq, Ord, Enum)
firstSerial = Serial 0

data Pat name = PConApp name [Pat name]
              | PVar name
              | PWildcard
              | PLit Lit
              | PAsPat (Pat name)
              deriving Show
                    
data Lit = IntLit Integer                  
         | CharLit Char
         deriving Show
                       
data Bind name = Bind (FName name) (FNode name)
               deriving Show

data FName name = Generated Serial
                | Given name
                deriving Show

data Alt name node = Alt { altPatterns :: [Pat name],
                           altBody :: node }
              deriving Show
               
data BuiltinFun = IntPlus
                | IntMinus
                deriving Show
               
data Payload name node = Uninitialized
                       | Knot node
                       | Lambda (Pat name) node
                       | ParamRef name
                       | Lit Lit
                       | App node [node]
                       | BuiltinFunApp BuiltinFun [node]
                       | Case [Alt name node] [node]
                       | ConApp name [node]
                       deriving Show
                                
type CPayload s name = Payload name (CNode s name)
type FPayload name = Payload name (FNode name)

-- A node in the cyclic representation
data CNode s name = CNode { cnodeSerial :: Serial, 
                            cnodeName :: Maybe name,
                            cnodePayload :: STRef s (CPayload s name) }
               
-- A node in the flattened representation               
data FNode name = FNode (FPayload name)
                | FLet [Bind name] (FNode name)
                | FVarRef (FName name)
                deriving Show
                    
instance Eq (CNode s name) where
  (==) = (==) `on` cnodeSerial

instance Ord (CNode s name) where
  compare = compare `on` cnodeSerial
                     
