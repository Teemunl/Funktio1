module Main where

-- Ceasarin salakirjoitus: Korvaa viestin jokainen kirjain 4 merkkiä aakkosissa sen jälkeen tulevalla: 
ceasar :: Int -> Text -> Text
ceasar seLuku salattava
  = let
     lista = toString salattava
     salattu = map (eteenpain seLuku) lista
    in fromString salattu
  
-- apufunktio jolla pääsee eteenpäin aakkosissa
eteenpain :: Int -> Char -> Char
eteenpain 0 c = c
eteenpain n c 
 = let
    nMiinusYksiEteenpain = eteenpain (n-1) c
    in succ nMiinusYksiEteenpain

--ceaserPurku purkaa caesar tehdyn salauksen jos tiedetään salauksessa käytetty luku
ceaserPurku :: Int -> Text -> Text
ceaserPurku seLuku purettava
  = let
    purettu = map (taaksepain seLuku) (toString purettava)
    in fromString purettu
-- apufunktio jolla pääsee taaksepäin aakkosissa
taaksepain :: Int -> Char -> Char
taaksepain 0 c = c
taaksepain n c 
 = let
    nMiinusYksiTaaksepain = taaksepain (n-1) c
   in pred nMiinusYksiTaaksepain
--Vigeneren salakirjoitus Korvaa viestin kirjaimet 'lisäämällä' avaimen verran
vigenere :: [Int] -> Text -> Text
vigenere avain salattava 
 = let
  lista = toString salattava
  jatkettuAvain = cycle avain
  siirretty = zipWith eteenpain jatkettuAvain lista
  in fromString siirretty
-- purkuVi purkaa salauksen jos tiedetään avain
purkuVi :: [Int] -> Text -> Text
purkuVi avain purettu
 = let
   lista = toString purettu
   jatkettuAvain = cycle avain
   siirretty = zipWith taaksepain jatkettuAvain lista
   in fromString siirretty
postikulut :: [Int] -> Bool -> Double
postikulut mitat sarkyva
  = let
    
main :: IO ()
main = do
  putStrLn "Hello TIEA341"
