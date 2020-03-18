import System.Random
import Control.Monad(unless)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randomNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
  putStrLn "Guess which number in the range of 1 to 10 I'm thinking of?"
  inputNumber <- getLine
  unless (null inputNumber) $ do
    if read inputNumber == randomNumber then
      putStrLn "You're right!"
    else
      putStrLn $ "Wrong! The number was " ++ show randomNumber
    askForNumber newGen
