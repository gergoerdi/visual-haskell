{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vis.Node (
  BuiltinFun(..), 
  FPayload, FNode(..),
  Bind(..), Alt(..), Pat(..), Payload(..), Lit(..),  
  FName(..),
  Serial(unSerial), firstSerial
  ) where

newtype Serial = Serial { unSerial :: Int } deriving (Show, Eq, Ord, Enum)
firstSerial :: Serial
firstSerial = Serial 0

data Pat name = PConApp name [Pat name]
              | PVar name
              | PWildcard
              | PLit Lit
              | PAsPat name (Pat name)
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
               
data Payload name node = Knot node
                       | Lambda [name] node
                       | ParamRef name
                       | Lit Lit
                       | App node [node]
                       | BuiltinFunApp BuiltinFun [node]
                       | Case [Alt name node] [node]
                       | ConApp name [node]
                       deriving Show
                                
type FPayload name = Payload name (FNode name)

-- A node in the flattened representation               
data FNode name = FNode (FPayload name)
                | FLet [Bind name] (FNode name)
                | FVarRef (FName name)
                deriving Show
                    
