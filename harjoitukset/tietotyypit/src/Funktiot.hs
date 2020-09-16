module Funktiot where

-- Int -> Int -> Int -> Int
{-summa :: Int -> Int -> Int -> Int
summa x y z = x + y + z


summa :: Int -> Int -> (Int -> Int)
summa x y =
    where 
        funktio z = x +  y + z-}

pariJossaOnFunktio :: ( Natural -> Bool , Bool)
pariJossaOnFunktio = (even, True)

pariJossaOnFunktio2 :: ( Natural -> Bool , Bool)
pariJossaOnFunktio2 = 
    let
     f x = case x of
            0 -> True
            _ -> False
    in (f,True)

pariJossaOnFunktio3 :: ( Natural -> Bool , Bool)
pariJossaOnFunktio3 = (f,True)
    where
     f x = x > 10

yhdiste :: (b -> c) -> (a -> b) -> (a -> c)
yhdiste funktio1 funktio2 x = funktio1 (funktio2 x)

yhdistä :: (b -> c) -> (a -> b) -> ( a -> c)
yhdistä funA funB = let
                     out x = funA ( funB x)
                    in out

pariJossaOnFunktio4 :: ( Natural -> Bool , Bool)
pariJossaOnFunktio4 = ((\x -> x > 20), True)

-- (\x -> z > 20)
-- 

--                      Natural -> Natural -> Bool
funktioArgumenttina :: (Natural -> Natural) -> Bool
funktioArgumenttina seFunktio = seFunktio 100 > 1000

-- funktioArgumenttina (\x -> x+1)
-- | seFunktio :=  (\x -> x+1)
--  (\x -> x+1) 100 > 1000
-- | x:= 100
--  100 0 1 > 1000
-- 101 > 1000
--kutsu3Kertaa f x = f(f x))
kutsu3Kertaa :: forall a. (a -> a) -> a -> a
kutsu3Kertaa seFunktio seA 
    = let
        kutsuttuKerran = seFunktio seA
        kutsuttuToisenKerran = seFunktio kutsuttuKerran
        kutsuttuKolmeKertaa = seFunktio kutsuttuToisenKerran
    in kutsuttuKolmeKertaa 

kutsu3Kertaa1 :: forall a. (a -> a) -> a -> a
kutsu3Kertaa1 f x = (f (f (f x)))


kutsuParia :: forall a b. (a,a -> b) -> b
kutsuParia sePari 
  = case sePari of
      (seA,seFunktio) -> seFunktio seA

molemmille :: forall a t. (a -> t) -> (a, a) -> (t, t)
molemmille seFunktio (a,b)
    = let
        eka = seFunktio a
        toka = seFunktio b
    in (eka,toka)


kutsuNKertaa1 :: forall a. Int -> (a -> a) -> (a -> a)
kutsuNKertaa1 n seFunktio =
     case n of
         1 -> seFunktio
         _ -> kutsuNKertaa(n-1) (\x -> seFunktio x)
-- ei toimi kun n <= 0

kutsuNKertaa :: forall a. Int -> (a -> a) -> (a -> a)
kutsuNKertaa n seFunktio =
     case n of
         0 -> (\seA -> seA)
         m -> let
               kutsutaanNMinus1Kertaa = kutsuNKertaa (m-1) seFunktio
              in (\seA -> kutsutaanNMinus1Kertaa (seFunktio seA))
-- ei toimi kun n <= 0

funktioPaluuarvona :: Natural -> (Natural -> Bool)
funktioPaluuarvona n 
    = case n of
        0 -> even 
        1 -> odd
        2 -> (\x -> x + 1 > 10)
        x 
          | even x -> f
          | odd x  -> g
        _ -> even
--      ↑
--     Tämä tarvitaan tai kääntäjä sanoo "non-exhaustive patterns"
--     Miksi? Koska se ei tiedä, että luku on aina parillinen tai pariton
--     (Yleisessä tapauksessa se ei voi tietää, koska pysähtymisongelma, joten
--     se ei edes yritä yksinkertaisia)
      where
        f x = x < 10
        g x = x > 10