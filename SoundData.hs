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
       
       data Address = LocA Int
                    | Empty
                    deriving (Show, Eq)

       data Expr = VarE Var
                 | LocE Int
                 | Literal Address
                 | Add Expr Expr
                 | Sub Expr Expr
                 | EqE Expr Expr
                 | Less Expr Expr
                 deriving (Show, Eq)

       data Cmd = EqC Expr Expr
                | Seq Cmd Cmd
                | If Expr Cmd Cmd
                | While Expr Cmd
                | Letvar Expr Expr Cmd
                deriving (Show, Eq)

       data Phrases = PhrasesE Expr
                    | PhrasesC Cmd
                    deriving (Show, Eq)

       data Basis = Gama String Type
                  deriving (Show, Eq)

       data Loc = Lambda Address Security
                deriving (Show, Eq)

       type Mem = (Int, Address)

       getLiteral :: Expr -> Int
       getLiteral (Literal (LocA n)) = n
       getLiteral _ = -1

       isElemMem :: Int -> [Mem] -> Bool
       isElemMem _ [] = False
       isElemMem l ((u1, u2) : us) = if l == u1 then True else (isElemMem l us)       

       getMemValue :: Int -> [Mem] -> Address
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
       
       insertLocation :: Int -> [Mem] -> [Mem]
       insertLocation l [] = [(l, Empty)]
       insertLocation l u = (head u) : (insertLocation l (tail u)) 

       except :: [Mem] -> Int -> [Mem]
       except [] _ = []
       except ((u1, u2) : us) l = if u1 == l then (except us l) else (u1, u2) : (except us l)