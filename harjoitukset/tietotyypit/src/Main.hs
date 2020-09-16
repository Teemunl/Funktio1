module Main where

import Johdanto
import Parit
import Funktiot
import MunMaybe

main :: IO ()
main = do
  putTextLn (tulostaNimiPari esimerkkiPari)
