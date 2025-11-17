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
    (c,g)->case p of{
        []->case c of{
            0->[];
            k->[m];
        }
        (c2,g2):ps-> case g2<g of{
            True -> m:p; 
            False -> case g2==g of {
                True -> case c+c2 of{
                    0-> ps;
                    k->(c+c2,g):ps;
                }
                False->(c2,g2) : agregarMon m ps;
            }
        }
    }
}



--2)
redPol :: Polinomio -> Polinomio
redPol = undefined	

--3)
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = undefined

--4)
mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = undefined 

--5)
derPol :: Polinomio -> Polinomio
derPol = undefined

--6)
evalPol :: Polinomio -> Int -> Int
evalPol = undefined

--7)
gradoPol::Polinomio -> Int
gradoPol = undefined
																	
																	
-- ======================
-- SHOW
-- ======================

--8)
showMon :: Monomio -> String
showMon = undefined

--9)
showPol :: Polinomio -> String
showPol = undefined  
