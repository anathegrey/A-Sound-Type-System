module SoundData where

       data Security = High
                     | Low
                     deriving (Show, Eq)

       data Type = TypeT Security
                 | TypeVar Security
                 | TypeCmd Security
                 | Free
                 deriving (Show, Eq)

       type Var = String
       type Address = Int

       data Expr = VarE Var
                 | LocE Address 
                 | Literal Int
                 | Add Expr Expr
                 | Sub Expr Expr
                 | EqE Expr Expr
                 | Less Expr Expr
                 deriving (Show, Eq)

       data Cmd = EqC Expr Expr
                | Seq Cmd Cmd
                | If Expr Cmd Cmd
                | While Expr Cmd
                | LetVar Var Expr Cmd
                deriving (Show, Eq)

       data Phrases = PhrasesE Expr
                    | PhrasesC Cmd
                    deriving (Show, Eq)

       data Basis = Gama String Type
                  deriving (Show, Eq)

       data Loc = Lambda Address Security
                deriving (Show, Eq)

       type Mem = (Address, Int) --Var or Int?

       isElemMem :: Address -> [Mem] -> Bool
       isElemMem _ [] = False
       isElemMem l ((u1, u2) : us) = if l == u1 then True else (isElemMem l us)       

       getMemValue :: Address -> [Mem] -> Int --before entering this function, the program is supposed to check isElem
       getMemValue l ((u1, u2) : us) = if l == u1 then u2 else (getMemValue l us)

       setMemValue :: Address -> [Mem] -> Expr -> [Mem]
       setMemValue l ((u1, u2) : us) (Literal n) = if l == u1 then ((u1, n) : us) else (setMemValue l us (Literal n))