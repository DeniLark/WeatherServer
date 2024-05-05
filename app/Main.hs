module Main where

import Config (getConfig)

-- import Weather (runWeather)

main :: IO ()
main = do
  -- _ <- runWeather
  c <- getConfig
  case c of
    Left e -> print e
    Right a -> print a
  putStrLn "Done!"
