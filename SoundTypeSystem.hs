module SoundTypeSystem where
       import SoundData

       evalExpr :: [Mem] -> Expr -> Expr
       evalExpr u (Literal n) = (Literal n)                                                                             --Base
       evalExpr u (LocE l) = if (isElemMem l u) then (Literal (getMemValue l u)) else (LocE l)                          --Contents
       evalExpr u (Add e1 e2) = let n1 = evalExpr u e1                                                                  --Add
                                    n2 = evalExpr u e2
                                in Add n1 n2

       evalCmd :: [Mem] -> Cmd -> [Mem]
       evalCmd u (EqC (LocE l) expr) = let n = evalExpr u expr                                                          --Update
                                       in if (isElemMem l u) then (setMemValue l u n) else u
       evalCmd u (Seq c1 c2) = u2                                                                                       --Sequence
                              where u1 = evalCmd u c1
                                    u2 = evalCmd u1 c2
       evalCmd u (If expr c1 c2) = let n = evalExpr u expr                                                              --Branch
                                       u2 = evalCmd u c1
                                   in if n == (Literal 1) then u2 else u
       evalCmd u (While expr c)                                                                                         --Loop
               | n == (Literal 0) = u
               | n == (Literal 1) = u2
               where n = evalExpr u expr
                     u1 = evalCmd u c
                     u2 = evalCmd u1 (While expr c)
       --evalCmd u (Letvar x expr c) = let n = evalExpr u expr
                                         