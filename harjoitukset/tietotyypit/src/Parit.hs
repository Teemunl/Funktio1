module Parit where

esimerkkiPari :: (Text, Natural)
esimerkkiPari = (nimi,ika)

esimerkkiPari2 :: (Text,Natural,Bool)
esimerkkiPari2 = (nimi,ika,False)

esimerkinEkaAlkio = fst esimerkkiPari
esimerkinTokaAlkio = snd esimerkkiPari

tulostaNimiPari :: (Text,Natural) -> Text
tulostaNimiPari nimiPari 
    = case nimiPari of
        (nimi,ika) -> "Henkilö " <> nimi <> " on " <> show ika <> " vuotta vanha"

x :: (Bool,Int,Int)
x = (True,42,10)
--Ensimmäinen alkio kolmikosta
fstOfTriple :: forall a b c. (a ,b ,c ) -> a 
fstOfTriple kolmikko 
    = case kolmikko of
        (a,_,_) -> a
--Toinen alkio kolmikosta
sndOfTriple :: forall a b c. (a ,b ,c ) -> b 
sndOfTriple kolmikko 
    = case kolmikko of
        (_,b,_) -> b
--TKolmas alkio kolmikosta
thdOfTriple :: forall a b c. (a ,b ,c ) -> c 
thdOfTriple kolmikko 
    = case kolmikko of
        (_,_,c) -> c

yhteenLasku :: (Bool,Int,Int) -> (Bool,Int)
yhteenLasku yhteen 
    = case yhteen of
        (a,b,c) -> (a,b+c)

yhteenLasku3 :: (Bool,Int,Int) -> (Bool,Int)
yhteenLasku3 yhteen 
    = case yhteen of
        yhteen -> (fstOfTriple(yhteen),sndOfTriple(yhteen)+thdOfTriple(yhteen))

yhteenLasku4 :: (Bool,Int,Int) -> (Bool,Int)
yhteenLasku4 x = (fstOfTriple(x),sndOfTriple(x)+thdOfTriple(x))

yhteenLasku2 :: (Bool,Int,Int) -> (Bool,Int)
yhteenLasku2 (a,b,c ) = (a,b+c)

nimi = "Ville"
ika = 40