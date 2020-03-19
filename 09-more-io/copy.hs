import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as B

main = do
  (fromFile:toFile:_) <- getArgs
  copy fromFile toFile

copy :: String -> String -> IO ()
copy fromFile toFile = do
  contents <- B.readFile fromFile
  bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle) -> do
          hClose tempHandle
          removeFile tempName)
      (\(tempName, tempHandle) -> do
          B.hPutStr tempHandle contents
          hClose tempHandle
          renameFile tempName toFile)
