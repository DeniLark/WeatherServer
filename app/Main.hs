module Main where

import Weather (runWeather)

main :: IO ()
main = do
  _ <- runWeather
  putStrLn "Done!"
