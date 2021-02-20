module SoundData where

--fig 3
       type Var = String

       data Expr = VarE Var
                 | LocE Int
                 | Literal Int
                 | Add Expr Expr
                 | Sub Expr Expr
                 | EqE Expr Expr
                 | Less Expr Expr
                 deriving (Show, Eq, Ord)

       data Cmd = EqC Expr Expr
                | Seq Cmd Cmd
                | If Expr Cmd Cmd
                | While Expr Cmd
                | Letvar Expr Expr Cmd
                deriving (Show, Eq)

       checkBoolExpr :: Expr -> Bool
       checkBoolExpr (EqE e1 e2) = if (e1 == e2) then True else False
       checkBoolExpr (Less e1 e2) = if (e1 < e2) then True else False
       
       type Mem = (Int, Int)

       addLiteral :: Expr -> Expr -> Expr
       addLiteral (Literal n1) (Literal n2) = Literal (n1 + n2)
       addLiteral n1 n2 = Add n1 n2
       
       isElemMem :: Int -> [Mem] -> Bool
       isElemMem _ [] = False
       isElemMem l ((u1, u2) : us) = if l == u1 then True else (isElemMem l us)

       getMemValue :: Int -> [Mem] -> Int
       getMemValue l ((u1, u2) : us)
                   | isElemMem l ((u1, u2) : us) = if l == u1 then u2 else (getMemValue l us)

       setMemValue :: Int -> [Mem] -> Expr -> [Mem]
       setMemValue l ((u1, u2) : us) (Literal n) = if l == u1 then ((u1, n) : us) else (u1, u2) : (setMemValue l us (Literal n))

       maxMem :: [Mem] -> Int
       maxMem ((u1, u2) : []) = u1
       maxMem ((u1, u2) : (u3, u4) : us) = if u1 >= u3 then (maxMem ((u1, u2) : us)) else (maxMem ((u3, u4) : us))
      
       generateAddress :: [Mem] -> Int
       generateAddress [] = 0
       generateAddress u = (maxMem u) + 1
       
       insertPair :: Int -> [Mem] -> Expr -> [Mem]
       insertPair l [] (Literal n) = [(l, n)]
       insertPair l u n = (head u) : (insertPair l (tail u) n)

       except :: [Mem] -> Int -> [Mem]
       except [] _ = []
       except ((u1, u2) : us) l = if u1 == l then (except us l) else (u1, u2) : (except us l)

--fig 5
       data DataTypes = High
                      | Low
                      deriving (Show, Eq)
       instance Ord DataTypes where
           Low  <= High = True
           High <= High = True
           Low  <= Low  = True
           _    <= _    = False 

       data PhraseTypes = TypeT DataTypes
                        | TypeVar DataTypes
                        | TypeCmd DataTypes
                        | None
                        deriving Show

       type LocTyping = (Mem, DataTypes)
       type IDTyping = (Var, PhraseTypes)

       searchID :: Expr -> [IDTyping] -> DataTypes
       searchID (VarE x) ((y, TypeVar t) : ys) = if x == y then t else (searchID (VarE x) ys)
       --searchID expr ((y, TypeT t) : ys) = if expr == y then t else (searchID expr ys)