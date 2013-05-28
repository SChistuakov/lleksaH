import Data.List
import System.IO

f :: [Int] -> IO ()	
f xs = do
              putStr ("Try one \n 0 - exit \n 1 - insert \n 2 - delete \n 3 - print \n ")
              command <- getLine
              case command of

			  "0" -> return ()

                          "1" -> do   
                                    putStr ("insert - ")
                                    value <- readLn
                                    f (insert value xs)

                          "2" -> do
                                    putStrLn ("delete - ")
                                    value <- readLn
                                    f (delete value xs)

                          "3" -> do
                                    putStr (show xs)
				    putStr ("\n")
                                    f xs

                          otherwise -> f xs
main = do
    f []