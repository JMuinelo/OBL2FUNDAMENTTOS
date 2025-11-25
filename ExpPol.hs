{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant lambda" #-}

module ExpPol where

import Polinomios;

-- ========================
-- EXPRESIONES POLINOMICAS
-- ========================

data ExPol where 
		Pol  :: Polinomio -> ExPol 
		Der  :: ExPol -> ExPol 
		Eval :: ExPol -> Int -> ExPol 
		Sum  :: [ExPol] -> ExPol 
		Prod :: [ExPol] -> ExPol 
				deriving Show

																	
--10) 
cantPol :: ExPol -> Int
cantPol = \x -> case x of {
	
	 	Pol f -> 1;
	
		Der a -> cantPol a;
	
		Eval a n -> cantPol a;
	
		Sum xs -> sum (map cantPol xs) ;
	
		Prod xs -> sum (map cantPol xs)
	
	}




--11)
cantx:: ExPol -> Int
cantx = \x -> case x of {
	
	 	Pol f -> case f of{
			[] -> 0;
			(c,g) : fs -> case g of {
				0 -> cantx (Pol fs);
				x -> 1 + cantx (Pol fs);
			} 
		} ;
	
		Der a -> cantx a;
	
		Eval a n -> cantx a;
	
		Sum xs -> case xs of{
			[] -> 0;
			y:ys -> cantx y +  cantx(Sum ys);
		} ;
	
		Prod xs -> case xs of{
			[] -> 0;
			y:ys -> cantx y +  cantx(Prod ys);
		} 
	
		

	
	}


--12)
maxProd :: ExPol -> Int
maxProd  = \x -> case x of {
	
	 	Pol f -> 1;
	
		Der a -> maxProd a;
	
		Eval a n -> maxProd a;
	
		Sum xs -> case xs of{
			[] -> 0;--"sera cero o no nunca lo sabremos"mud wizard 2025
			y:ys -> max (maxProd y) (maxProd (Sum ys));
		};
	
		Prod xs -> case xs of{
			[] -> 0;
			y:ys -> max (maxProd y) (maxProd (Prod ys));
			
		} 
		
		

	
		}


--13)
gradoEP :: ExPol -> Int
gradoEP = undefined 
	
--14)	
calcEP :: ExPol -> Polinomio
calcEP = undefined  

--15)
resultado :: ExPol -> String
resultado = undefined

