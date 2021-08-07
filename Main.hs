import System.Directory
-- import Control.Monad.Writer.Strict (mapM_)
-- import Control.Monad.Writer.Strict (mapM_)
import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode( NoBuffering ), hReady, stdout)
import Control.Monad (when, mapM_)
import System.Console.ANSI
--setSGR [SetColor Foreground Vivid Red]

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = zip [0..]

view selected cwd files = 
  let fileEntryPrinter idx name = do
      when (idx == selected) (setSGR [SetSwapForegroundBackground True])
      putStrLn $ "  " ++ name ++ replicate (30 - length name) ' '
      when (idx == selected) (setSGR [SetSwapForegroundBackground False])
  in do
    
    putStr "Current Dir: "
    putStrLn cwd
    putStrLn "Contents: "
    mapM_ (uncurry fileEntryPrinter) $ enumerate files
    putChar '\n'

loop :: Int -> IO ()
loop selected = do
    cwd <- getCurrentDirectory 
    files <- getDirectoryContents cwd
    let fileCount = length files

    clearScreen
    setCursorPosition 0 0
    view selected cwd files

    key <- getKey
    when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> loop $ mod (selected - 1) fileCount -- up
      "\ESC[B" -> loop $ mod (selected + 1) fileCount -- down
      "\ESC[C" -> loop $ mod (selected + 1) fileCount -- right
      "\ESC[D" -> loop $ mod (selected - 1) fileCount -- left
      "\n"     -> do
        setCurrentDirectory $ cwd ++ "/" ++ files !! selected
        loop 0
      -- "\DEL"   -> putStr "⎋"
      _        -> loop selected
    -- loop selected

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering 
    hSetEcho stdin False
    putStrLn "hello world"
    

    loop 0