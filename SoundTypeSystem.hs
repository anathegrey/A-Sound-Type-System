module SoundTypeSystem where
       import SoundData

       substExpr :: Address -> String -> Expr -> Expr
       substExpr l x (VarE y) = if x == y then (LocE l) else (VarE y)
       substExpr l x (LocE a) = LocE a
       substExpr l x (Literal n) = Literal n
       substExpr l x (Add e1 e2) = Add (substExpr l x e1) (substExpr l x e2)
       substExpr l x (Sub e1 e2) = Sub (substExpr l x e1) (substExpr l x e2)
       substExpr l x (EqE e1 e2) = EqE (substExpr l x e1) (substExpr l x e2)
       substExpr l x (Less e1 e2) = Less (substExpr l x e1) (substExpr l x e2)

       substCmd :: Address -> String -> Cmd -> Cmd
       substCmd l x (EqC e1 e2) = EqC (substExpr l x e1) (substExpr l x e2)
       substCmd l x (Seq c1 c2) = Seq (substCmd l x c1) (substCmd l x c2)
       substCmd l x (If expr c1 c2) = If (substExpr l x expr) (substCmd l x c1) (substCmd l x c2)
       substCmd l x (While expr c) = While (substExpr l x expr) (substCmd l x c)
       substCmd l x (Letvar (VarE y) expr c) = Letvar (VarE y) (substExpr l x expr) (substCmd l x c)

       evalExpr :: [Mem] -> Expr -> Expr
       evalExpr u (Literal n) = (Literal n)                                                                             --Base
       evalExpr u (LocE l) = if (isElemMem l u) then (Literal (getMemValue l u)) else (LocE l)                          --Contents
       evalExpr u (Add e1 e2)
                | getLiteral n == -1 && getLiteral n1 /= -1 = n1
                | getLiteral n /= -1 && getLiteral n1 == -1 = n
                | getLiteral n /= -1 && getLiteral n1 /= -1 = Literal (getLiteral n + getLiteral n1)
                | getLiteral n == -1 && getLiteral n1 == -1 = Add n n1
                where n = evalExpr u e1
                      n1 = evalExpr u e2
                     
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
       evalCmd u (Letvar (VarE x) expr c) = let n = evalExpr u expr
                                                l = generateAddress u
                                                u1 = insertLocation l u
                                                u2 = evalCmd (evalCmd u1 (EqC (LocE l) n)) (substCmd l x c)
                                            in except u2 l
       evalCmd u _ = u