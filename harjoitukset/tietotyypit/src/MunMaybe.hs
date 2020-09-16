module MunMaybe where

-- Just 'x' :: Maybe Char
-- Nothing :: Maybe Char

data MunMaybe a = EiOle -- Nothing
                | OnVaan a -- Just 
    deriving (Eq,Ord,Show)

--  etsi :: (Natural -> Bool) -> [Natural] -> Maybe Natural 
--  etsi :: forall a. (a -> Bool) -> [a] -> Maybe a
--- etsi (\x -> x > 5) [1,2,3,7,2,9]
--  ↓
--  7
--- etsi (\x -> x > 5) []

etsi :: forall a. (a -> Bool) -> [a] -> Maybe a
etsi p lista 
  = case lista of
     [] -> Nothing
     (eka : loput) 
       | p eka     -> Just eka
       | otherwise -> etsi p loput

onko :: forall a. (a -> Bool) -> [a] -> Bool
onko ehto lista = case etsi ehto lista of
                    Just _x -> True
                    Nothing -> False

teeMaybe :: forall a. a -> Maybe a
teeMaybe a = Just a

onkoTäysi :: forall a. Maybe a -> Bool
onkoTäysi seMaybe = case seMaybe of 
               Just _ -> True
               Nothing -> False

onkoTyhjä :: forall a. Maybe a -> Bool
onkoTyhjä = not . onkoTäysi

oletusArvolla :: forall a. a -> Maybe a -> a
oletusArvolla arg maybeArg = case maybeArg of 
               Nothing -> arg
               Just jotain ->  jotain

kutsuMaybella1 :: forall a b. (a -> b) -> Maybe a -> Maybe b
kutsuMaybella1 funktio maybeArg = case maybeArg of
                                   Nothing -> Nothing
                                   Just jotain -> Just (funktio jotain)

kutsuMaybellä2 :: forall a b c. (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
kutsuMaybellä2 funktio maybeArg1 maybeArg2 = case maybeArg1 of
                                              Nothing -> Nothing
                                              Just eka -> case maybeArg2 of
                                                            Nothing -> Nothing
                                                            Just toka -> Just (funktio eka toka)

kutsuMaybellä3 :: forall a b c d. (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d 
kutsuMaybellä3 funktio ehk1 ehk2 ehk3 = case ehk1 of
                                         Nothing -> Nothing
                                         Just eka -> case ehk2 of
                                                       Nothing -> Nothing
                                                       Just toka -> case ehk3 of
                                                                     Nothing -> Nothing
                                                                     Just koka -> Just(funktio eka toka koka) 

kutsuMaybellä2B :: forall a b c. (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c 
kutsuMaybellä2B seFunc ehk1 ehk2 = case (ehk1,ehk2) of
                                    (Just x, Just y) -> Just(seFunc x y)
                                    _ -> Nothing

kutsuMaybellä3b ::forall a b c d. (a -> b -> c -> d)-> Maybe a -> Maybe b -> Maybe c -> Maybe d
kutsuMaybellä3b seFunc ehk1 ehk2 ehk3 = case (ehk1,ehk2,ehk3) of
                                         (Just x,Just y, Just z) -> Just ( seFunc x y z)
                                         _ -> Nothing