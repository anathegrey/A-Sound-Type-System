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

--fig 3
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
       evalCmd u (Letvar (VarE x) expr c) = let n = evalExpr u expr                                                     --BindVar
                                                l = generateAddress u
                                                u1 = insertPair l u n
                                                u2 = evalCmd u1 (substCmd l x c)
                                            in except u2 l 
       evalCmd u _ = u

--fig 5

       typingRulesExpr :: [LocTyping] -> [IDTyping] -> Expr -> PhraseTypes -> Bool
       typingRulesExpr lambda gama expr (TypeT t) = let t1 = searchIDExpr expr gama                                     --R-Val'
                                                   in t1 <= t

      {- typingRulesCmd :: [LocTyping] -> [IDTyping] -> Cmd -> PhraseTypes -> Bool
       typingRulesCmd lambda gama (EqC e1 e2) (TypeCmd t1) = let tVar = searchIDExpr e1 gama                            --Assign'
                                                                 t    = searchLoc e2 lambda
                                                            in (tVar == t && t1 <= t) 
       typingRulesCmd lambda gama (If e c1 c2) (TypeCmd t1) = let t     = searchLoc e lambda                            --If'
                                                                  tCmd1 = searchIDCmd c1 gama
                                                                  tCmd2 = searchIDCmd c2 gama
                                                              in (t == tCmd1 && t == tCmd2 && t1 <= t)
       typingRulesCmd lambda gama (While e c) (TypeCmd t1) = let t    = searchLoc e lambda                              --While'
                                                                 tCmd = searchCmd c gama
                                                             in (t == tCmd && t1 <= t) -}
                                                            
       

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