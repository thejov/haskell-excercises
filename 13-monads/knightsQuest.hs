import Control.Monad

type KnightPosition = (Int, Int)

moveKnight :: KnightPosition -> [KnightPosition]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1)
             , (c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)
             ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

in3 :: KnightPosition -> [KnightPosition]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

in3' :: KnightPosition -> [KnightPosition]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight
