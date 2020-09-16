module Main where

-- kerroKuudella kertoo luvun kuudella 
kerroKuudella :: Natural -> Natural
kerroKuudella seLuku = seLuku * 6

-- onkoAnagrammi tarkistaa onko kaksi sanaa anagrammeja
onkoAnagrammi :: Text -> Text -> Bool
onkoAnagrammi teksti1 teksti2
  = (sort (toString teksti2)) 
     ==  (sort (toString teksti1))
-- Summa funktio rekursiolla
summa :: [Natural] -> Natural
summa [] = 0
summa (eka : loput) = eka + (summa loput)

-- tulo rekursiolla
tulo :: [Natural] -> Natural
tulo [] = 1
tulo (alku : loppu) = alku * (tulo loppu)

main :: IO ()
main = do
  putStrLn "Hello TIEA341"
