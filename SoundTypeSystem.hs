module SoundTypeSystem where
       import SoundData

       substExpr :: Int -> String -> Expr -> Expr
       substExpr l x (VarE y) = if x == y then (LocE l) else (VarE y)
       substExpr l x (LocE a) = LocE a
       substExpr l x (Literal n) = Literal n
       substExpr l x (Add e1 e2) = Add (substExpr l x e1) (substExpr l x e2)
       substExpr l x (Sub e1 e2) = Sub (substExpr l x e1) (substExpr l x e2)
       substExpr l x (EqE e1 e2) = EqE (substExpr l x e1) (substExpr l x e2)
       substExpr l x (Less e1 e2) = Less (substExpr l x e1) (substExpr l x e2)

       substCmd :: Int -> String -> Cmd -> Cmd
       substCmd l x (EqC e1 e2) = EqC (substExpr l x e1) (substExpr l x e2)
       substCmd l x (Seq c1 c2) = Seq (substCmd l x c1) (substCmd l x c2)
       substCmd l x (If expr c1 c2) = If (substExpr l x expr) (substCmd l x c1) (substCmd l x c2)
       substCmd l x (While expr c) = While (substExpr l x expr) (substCmd l x c)
       substCmd l x (Letvar (VarE y) expr c) = Letvar (VarE y) (substExpr l x expr) (substCmd l x c)

--fig 4
       evalExpr :: [Mem] -> Expr -> Expr
       evalExpr u (Literal n) = (Literal n)                                                                             --Base
       evalExpr u (LocE l) = if (isElemMem l u) then (Literal (getMemValue l u)) else (LocE l)                          --Contents
       evalExpr u (Add e1 e2) = addLiteral n n1
                where n = evalExpr u e1
                      n1 = evalExpr u e2
                     
       evalCmd :: [Mem] -> Cmd -> [Mem]
       evalCmd u (EqC (LocE l) expr) = let n = evalExpr u expr                                                          --Update
                                       in if (isElemMem l u) then (setMemValue l u n) else u
       evalCmd u (Seq c1 c2) = u2                                                                                       --Sequence
                              where u1 = evalCmd u c1
                                    u2 = evalCmd u1 c2
       evalCmd u (If expr c1 c2)  --Branch
                 | checkBoolExpr expr = u1
                 | otherwise = u2
                 where u1 = evalCmd u c1
                       u2 = evalCmd u c2                
       evalCmd u (While expr c)                                                                                         --Loop
               | checkBoolExpr expr = u1
               | otherwise = u2 --enters loop 
               where u1 = evalCmd u c
                     u2 = evalCmd u1 (While expr c)
       evalCmd u (Letvar (VarE x) expr c) = let n = evalExpr u expr 
                                                l = generateAddress u
                                                u1 = insertPair l u n
                                                u2 = evalCmd u1 (substCmd l x c)
                                            in except u2 l 
       evalCmd u _ = u

--fig 5

       typingRulesExpr :: [LocTyping] -> [IDTyping] -> Expr -> PhraseTypes -> Bool
       typingRulesExpr lambda gama expr (TypeT t) = if t1 <= t then True else False
                                                  where t1 = searchID expr gama

       typingRulesCmd :: [LocTyping] -> [IDTyping] -> Cmd -> PhraseTypes -> Bool
       

--exemplos

       ex1 :: Cmd
       ex1 = Letvar (VarE "x") (Add (Literal 2) (Literal 1)) (EqC (LocE 2) (Literal 9))

       ex2 :: Cmd
       ex2 = While (EqE (Literal 1) (Literal 1)) (EqC (LocE 2) (Literal 1))

       ex3 :: Cmd
       ex3 = While (Less (Literal 3) (Literal 1)) (EqC (LocE 2) (Literal 2))

       ex4 :: Cmd
       ex4 = Letvar (VarE "x") (Literal 9) (If (Less (Literal 4) (Literal 0)) (EqC (LocE 2) (Literal 5)) (EqC (LocE 1) (Literal 4)))

       ex5 :: Cmd
       ex5 = Seq ex1 (EqC (LocE 3) (Literal 10))