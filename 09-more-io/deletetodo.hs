import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
  contents <- readFile "todo.txt"
  let todoItems = lines contents
      numberedItems = zipWith (\i todoItem -> show i ++ " - " ++ todoItem) [0..] todoItems
  
  putStrLn "These are your TODO items:"
  mapM_ putStrLn numberedItems
  
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = unlines $ delete (todoItems !! number) todoItems
  
  bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle) -> do
          hClose tempHandle
          removeFile tempName)
      (\(tempName, tempHandle) -> do
          hPutStr tempHandle newTodoItems
          hClose tempHandle
          removeFile "todo.txt"
          renameFile tempName "todo.txt")
