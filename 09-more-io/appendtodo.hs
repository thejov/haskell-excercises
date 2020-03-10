import System.IO
import Data.Char

main = do
  todoItem <- getLine
  appendFile "todo.txt" $ todoItem ++ "\n"
