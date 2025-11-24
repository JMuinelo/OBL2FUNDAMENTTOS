{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}

module Polinomios where

type Monomio = (Int, Int)
type Polinomio = [Monomio]

-- ======================
-- POLINOMIOS
-- ======================

--1)
agregarMon :: Monomio -> Polinomio -> Polinomio
agregarMon = \m p -> case m of{
    (0,g)->p;
    (c,g)->case p of{
        []->[m];
        (c2,g2):ps-> case (g2<g) of{
            True -> m:p; 
            False -> case g2==g of {
                True -> case (c+c2) of{
                    0-> ps;
                    k->(c+c2,g):ps;
                };
                False -> (c2,g2) : agregarMon m ps;
            }
        }
    }
}

--2)
redPol :: Polinomio -> Polinomio
redPol = \ p -> case p of{
    []->[];
    x:xs-> agregarMon x (redPol xs);
}	

--3)
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = \ p1 p2 -> case p1 of{
    [] -> p2;
    x:xs -> agregarMon x (sumPol xs p2);
} 
    



--4)
--Auxiliar
mulMon ::Monomio -> Monomio ->  Monomio
mulMon = \m1 m2 -> case m1 of{
    (c1,g1) -> case m2 of{
        (c2,g2) -> (c1*c2,g1+g2);
    } 
} 


mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = \ p1 p2 -> case p1 of{
    []-> [];
    x:xs-> sumPol (map (mulMon x) p2) (mulPol xs p2);
} 

--5)
derPol :: Polinomio -> Polinomio
derPol = \ p1 -> case p1 of{
    [] ->[];
    x:xs -> case x of{
        (c,g)->agregarMon (c*g,g-1) (derPol xs);
    }
}

--6)
evalPol :: Polinomio -> Int -> Int
evalPol = \ p n -> case p of{
    []-> 0;
    (c,g):xs->c*(n^g) + evalPol xs n;
}

--7)
gradoPol::Polinomio -> Int
gradoPol = \ p -> case p of{
    [] -> 0;
    (c,g):xs -> g; 
}
																	
																	
-- ======================
-- SHOW
-- ======================

--8)
showMon :: Monomio -> String
showMon = \ m -> case m of{
    (c,g) -> case c of{
        0-> "";
        -1 -> case g of{
            0->show c;
            1-> "-x";
            h-> "-x^" ++show h;
        };
        1-> case g of{
            0->show c;
            1-> "x";
            h-> "x^" ++show h;
        };
        k->case g of{
            0->show k;
            1-> show k ++ "x";
            h-> show k ++ "x^" ++show h;
        }
    }
}

--9)
--Anda mal
showPol :: Polinomio -> String
showPol =  \ p -> case p of{
    []-> "";
    x:xs -> case xs of{
        []-> showMon x;
        (c,g):ys-> case c of{
            0 -> showMon x ++ showPol ys;
            h -> case h < 0 of {
                True -> showMon x ++ showPol xs;
                False -> showMon x ++ "+"  ++ showPol xs;
            }
        }
    }
}






