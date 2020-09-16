module Main where
{-Hinnoitellaan postipaketteja.

Paketti on voi olla

    Kirje, korkeus enintään 2cm ja leveys ja pituus enintään 20cm
    Pikkupaketti, max 20×20×20cm
    Suurpaketti, jos ei ole muita
    Merkattu Särkyväksi, jos asiakas niin haluaa

Kirje maksaa 3.90, pikkupaketti 5.95 ja suurpaketti 0.001€/cm³. Jos paketti on särkyvä, hinta nousee 5€ + 20% alkuperäisen paketin hinnasta.

Tehdään ohjelma, joka laskee postikulut paketille.-}
--Tehtävä
-- a) Korjaa käännosvirheet
-- b) lisää sylinterivaihtoehto
-- c) laita toimimaan
--  Tyypin nimi Konstruktori     Tietotyypin kentät
--    |            |              |
data Paketti = Suorakaide Natural Natural Natural
             | Sylinteri  Natural Natural
                deriving (Show, Eq)
--                  |     |
--           "autokoodaa" "Muuta merkkijonoksi" "Tee vertailuoperaattori"
-- f :: Paketti -> Bool
--Paketin hinnoittelija


data PakettiLuokka = Kirje
                   | PikkuPaketti
                   | SuurPaketti Natural--Kuutiosenttejä
                    deriving (Show, Eq)

luokittele :: Paketti -> PakettiLuokka
luokittele paketti =  case paketti of 
                       Sylinteri pituus sade 
                            -- -> SuurPaketti (pituus * (round (pi * (fromIntegral(sade*sade)))))
                        | (pituus <= 2) && (sade*2 <= 2) 
                            -> Kirje
                        | (pituus <= 20) && (sade*2 <= 20)
                            -> PikkuPaketti
                        |otherwise ->  let
                             pohjanAla = pi * (fromIntegral (sade*sade))
                             tilavuusDoublena = fromIntegral pituus* pohjanAla
                             pyoristettyTasamittaan = round tilavuusDoublena
                            in SuurPaketti pyoristettyTasamittaan
                       Suorakaide korkeus leveys pituus
                        | (korkeus <= 2) && (leveys <= 20) && (pituus <= 20) --onkoKirje paketti
                            -> Kirje
                        | (korkeus <= 20) && (leveys <= 20) && (pituus <= 20)-- onkoPikkuPaketti paketti
                            -> PikkuPaketti
                        | otherwise 
                            -> SuurPaketti (korkeus*leveys*pituus)


perusHinta :: PakettiLuokka -> Double
perusHinta luokka
     = case luokka of
        Kirje -> 3.90
        PikkuPaketti -> 5.95
        SuurPaketti tilavuus -> 0.001 * fromIntegral(tilavuus)

data Lahetys = Sarkyva       Paketti 
             | YkkosLuokka   Paketti
             | KakkosLuokka  Paketti
            deriving (Show,Eq)
-- Laskee paketin lähetys hinnan 
hinta :: Lahetys -> Double
hinta lahetys
    = case lahetys of
        YkkosLuokka  p  ->  perus p
        KakkosLuokka p  ->  0.9 * (perus p) 
        Sarkyva      p  ->  5 + ((perus p) * 1.2)
  where 
    perus p = perusHinta (luokittele p)                 


main :: IO ()
main = do
  putStrLn "Hello TIEA341"
