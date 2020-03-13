import Control.Exception
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesNotExist command

main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n"
add _ = putStrLn "The add command takes exactly two arguments"

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoItems = lines contents
      numberedItems = zipWith (\i todoItem -> show i ++ " - " ++ todoItem) [0..] todoItems
  mapM_ putStrLn numberedItems
view _ = putStrLn "The view command takes exactly one argument"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoItems = lines contents
      numberedItems = zipWith (\i todoItem -> show i ++ " - " ++ todoItem) [0..] todoItems
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
remove _ = putStrLn "The add command takes exactly two arguments"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let todoItems = lines contents
      numberedItems = zipWith (\i todoItem -> show i ++ " - " ++ todoItem) [0..] todoItems
  let number = read numberString
      itemToBump = todoItems !! number  
      newTodoItems = unlines . nub $ itemToBump : todoItems
  bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle) -> do
          hClose tempHandle
          removeFile tempName)
      (\(tempName, tempHandle) -> do
          hPutStr tempHandle newTodoItems
          hClose tempHandle
          removeFile "todo.txt"
          renameFile tempName "todo.txt")
bump _ = putStrLn "The add command takes exactly two arguments"

doesNotExist :: String -> [String] -> IO ()
doesNotExist command _ = putStrLn $ "The command " ++ command ++ " does not exist"