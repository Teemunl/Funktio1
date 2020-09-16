module Johdanto where

-- Arvo,             tietotyyppi,  literaali
-- "Tekstiä tässä",  Text,         "tuplahipsuihin literaalit"
-- ['t','e','k','s'],[Char],       "teks"
--                   String
-- 14,               Int,          14
-- True,             Bool,         True
-- Suorakaide 4 5 6, Paketti,      Suorakaide 4 5 6
-- Set.singleton 5,  Set Natural,  --
-- 3               , Natural,      3



-- Määritelmä:
teePalindromi :: [Char] -> [Char] --Tyyppimääritelmä
teePalindromi teksti = teksti ++ reverse teksti
-- Lauseke: teksti, ++, reverse teksti 
taikaLuku :: Int  --Tyyppimääritelmä
taikaLuku = 15
