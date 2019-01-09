module Main where

import Control.Monad

main :: IO ()
-- !!! main = putStrLn $ "Въведохте: " ++ getLine
{-
main = do line <- getLine
          let newLine = "Въведохте: " ++ line
          putStrLn newLine
-}
{-
main = do putStrLn "Моля, въведете палиндром: "
          line <- getLine
          let revLine = reverse line
          if revLine == line then putStrLn "Благодаря!"
          else do putStrLn $ line ++ " не е палиндром!"
                  main
-}

getInt :: IO Int
getInt = do line <- getLine
            return $ read line

findAverage :: IO Double
findAverage = do putStr "Моля, въведете брой: "           -- IO ()
                 n <- getInt                              -- IO Int
                 s <- readAndSum n                        -- IO Int
                 return $ fromIntegral s / fromIntegral n -- IO Double

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do putStr "Моля, въведете число: "
                  x <- getInt
                  s <- readAndSum (n - 1)
                  return $ x + s

getInts n = sequence $ replicate n getInt

readInt :: String -> IO Int
readInt s = do putStr $ "Моля, въведете " ++ s ++ ": "
               getInt

findAverage2 :: IO Double
findAverage2 = do n <- readInt "брой"
                  l <- mapM (readInt . ("число #"++) . show) [1..n]
                  let s = sum l
                  return $ (fromIntegral s) / (fromIntegral n)

{-
main = do l <- getContents
          putStr l
-}
main = interact id
