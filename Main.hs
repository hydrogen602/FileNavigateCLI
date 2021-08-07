import System.Directory
-- import Control.Monad.Writer.Strict (mapM_)
-- import Control.Monad.Writer.Strict (mapM_)
import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode( NoBuffering ), hReady, stdout)
import Control.Monad (when, mapM_)
import System.Console.ANSI
import Data.List (partition, sort)
import System.Console.Terminal.Size
import GHC.Real (Integral)
import Data.List.Split (chunksOf)
import Data.Char (toLower)

up = -1
down = 1

data Location = File String | Directory String deriving (Show, Ord, Eq)

getFileName :: Location -> String
getFileName (File s) = s
getFileName (Directory s) = s

isDirectory :: Location -> Bool 
isDirectory (File _) = False
isDirectory (Directory _) = True

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = zip [0..]

view :: (Integral a) => a -> String -> [Location] -> Window Int -> Int -> IO ()
view selected cwd files win startIndex =
  let fileEntryPrinter idx location = do
        let name = getFileName location
        let path = cwd ++ "/" ++ name
        let widthAvailable = width win - 2
        when (isDirectory location) (setSGR [SetColor Foreground Vivid Green])
        when (idx == selected) (setSGR [SetSwapForegroundBackground True])
        putStrLn $ "  " ++ take widthAvailable name ++ replicate (widthAvailable - length name) ' '
        when (idx == selected) (setSGR [SetSwapForegroundBackground False])
        setSGR [Reset]
      
      controlledPrint preStr content = do
        let availableTitleSpace = width win - length preStr - 1
        putStr preStr
        putStrLn (take availableTitleSpace content)
        mapM_ putStrLn (chunksOf (width win - 1) (drop availableTitleSpace content))
  in do
    
    controlledPrint "Current Dir: " cwd 

    putStrLn "Contents: "
    mapM_ (uncurry fileEntryPrinter) (take (height win - 5) (drop startIndex (enumerate files)))
    putChar '\n'

getDirContents :: FilePath -> IO [Location]
getDirContents cwd = 
  let
    determine :: String -> IO Location 
    determine name = do 
      isDir <- doesDirectoryExist newPath
      return $ if isDir then Directory name else File name
      where newPath = cwd ++ '/':name
    
    sortBoth (a, b) = (sort a, sort b)
  in do
    files <- fmap ("..":) (listDirectory cwd)
    names <- mapM determine files 
    let dirsAndFiles = sortBoth (partition isDirectory names)
    return (uncurry (++) dirsAndFiles)


getWindowSize :: IO (Window Integer)
getWindowSize = do
  s <- size
  return (case s of
          Just s -> s
          Nothing -> error "Couldn't determine screen size")


scrollHandler :: Int -> Int -> (Int, Int) -> Int -> (Int, Int)
scrollHandler lineCount fileCount (selected, startIndex) dir
  | newSelected < startIndex = (newSelected, newSelected)
  | newSelected > startIndex + lineCount = (newSelected, newSelected - lineCount)
  | otherwise = (newSelected, startIndex)
  where 
    newSelected = mod (selected + dir) fileCount


searchFirst :: Char -> [Location] -> Maybe Int
searchFirst _ [] = Nothing
searchFirst c (e:ls)
  | toLower (head name) == c = Just 0
  | otherwise = case searchFirst c ls of
      Nothing -> Nothing
      Just n -> Just $ n + 1
  where name = getFileName e


loop :: Int -> Int -> IO ()
loop selected startIndex = do
    win <- fmap (\w -> Window {height=fromIntegral (height w), width=fromIntegral (width w)}) getWindowSize

    cwd <- getCurrentDirectory
    files <- getDirContents cwd

    let fileCount = length files
    let thisScrollHandler = scrollHandler (height win - 6) fileCount (selected, startIndex)

    clearScreen
    setCursorPosition 0 0
    view selected cwd files win startIndex

    -- putStr $ show (selected, startIndex)

    key <- getKey
    when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> uncurry loop (thisScrollHandler up) -- up
      "\ESC[B" -> uncurry loop (thisScrollHandler down) -- down
      "\ESC[C" -> uncurry loop (thisScrollHandler down) -- right
      "\ESC[D" -> uncurry loop (thisScrollHandler up) -- left
      "\n"     -> 
        let newPath = cwd ++ "/" ++ getFileName (files !! selected)
        in do
          exists <- doesDirectoryExist newPath
          if exists then do
            setCurrentDirectory newPath
            loop 0 0
          else
            loop selected startIndex
      -- "\DEL"   -> putStr ""
      c -> case searchFirst (head c) files of
        Nothing -> loop selected startIndex
        Just n -> uncurry loop (thisScrollHandler (n - selected))

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering 
    hSetEcho stdin False

    loop 0 0
    clearScreen
    setCursorPosition 0 0
    cwd <- getCurrentDirectory 
    putStrLn cwd
