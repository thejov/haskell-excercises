import System.Random

tossCoinThrice :: StdGen -> (Bool, Bool, Bool)
tossCoinThrice gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)
