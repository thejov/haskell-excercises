type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) 
  | abs (left + n - right) < 4 = Just (left + n, right)
  | otherwise                  = Nothing 

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs(left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                 = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing 

routine :: Maybe Pole
routine = landLeft 2 (0,0) >>= landRight 2 >>= landLeft 1

routine' :: Maybe Pole
routine' = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

x -: f = f x