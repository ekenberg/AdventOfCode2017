import Data.Char
import Data.List
import System.Console.ANSI
import Control.Concurrent
import Debug.Trace

debug = not True
dtrace s = debug && trace s False

data MazeRunner = MazeRunner { maze    :: [String],
                               x       :: Int,
                               y       :: Int,
                               dir     :: Dir,
                               letters :: String,
                               nsteps  :: Int,
                               running :: Bool -- game over?
                             }


instance Show MazeRunner where
  show mz@(MazeRunner maze x y dir letters nsteps running) =
    "[" ++ (show (cc mz)) ++ "] " ++ (show dir)
    ++ " @(x:" ++ (show x) ++ ", y:" ++ (show y) ++ ")"
    ++ " [" ++ (show nsteps) ++ "] " ++ " (" ++ letters ++ ")"

data Dir = UP | DOWN | RIGHT | LEFT deriving (Show, Eq)
allDirections = [UP, DOWN, RIGHT, LEFT]

emptyMaze = MazeRunner [" "] 0 0 DOWN "" 1 True

testMaze = emptyMaze { maze =
                       [ "     |          ",
                         "     |  +--+    ",
                         "     A  |  C    ",
                         " F---|----E|--+ ",
                         "     |  |  |  D ",
                         "     +B-+  +--+ ",
                         "                "] }

runTest = putStrLn $ show $ run testMaze

runReal = do
  realMaze <- loadMazeFromDisk
  putStrLn $ show $ run realMaze

loadMazeFromDisk = do
  indata <- readFile "data1.txt"
  let m = lines indata
  return (emptyMaze { maze = m })


animateTest = animate testMaze

animateReal = do
  realMaze <- loadMazeFromDisk
  animate realMaze (80,40)

main = animateReal

animate mz' mapSize = do
  hideCursor
  go (findFirst mz')
  showCursor
  ansiPut [Reset] "\n"
    where
      go mz | (not . running) nextState = return mz
            | otherwise = do
                setCursorPosition 0 0
                clearScreen
                ansiPut [normal, vividcolor Cyan] $ (modeline mz) ++ "\n\n"
                ansiPut [normal, vividcolor Green] mapTop
                ansiPut [bold, vividcolor Red, inverse] $ [head mapBottom]
                ansiPut [normal, vividcolor Green, notinverse] $ tail mapBottom

                threadDelay (20 * 1000);
                go nextState
                  where
                    nextState = next mz
                    mzmap     = showMap mapSize mz
                    (mapTop, mapBottom) = splitAt ((length mzmap) `div` 2 - 1) mzmap

showMap (w,h) mz = unlines [[at x y mz | x <- [x'-wh .. x'+wh]] |  y <- [y'-hh..y'+hh]]
  where
    x' = x mz
    y' = y mz
    wh = w `div` 2
    hh = h `div` 2

modeline mz = show mz

run mz' = go (findFirst mz')
  where
    go mz | (not . running) nextState = mz
          | otherwise = go nextState
      where nextState = next mz

next mz | dtrace (show mz) = undefined
        | isAsciiUpper c   = step $ collect mz
        | c == '+'         = step $ turn mz
        | c == ' '         = mz { running = False }  -- end of maze
        | otherwise        = step mz
  where c                  = cc mz

findFirst mz = ff 0
  where ff n | at n 0 mz == ' ' = ff (n+1)
             | otherwise        = mz {y = 0, x = n}

at x y mz = ((maze mz) !! y') !! x' -- char at pos, or at (0,0) if outside map
  where
    y'   | y < 0 || y > maxy || x < 0 || x > maxx = 0
         | otherwise = y
    x'   | y < 0 || y > maxy || x < 0 || x > maxx = 0
         | otherwise = x
    maxy = (length (maze mz)) - 1
    maxx = (length (head (maze mz))) - 1

cc mz       = at (x mz) (y mz) mz      -- current char
nc mz       = cc (step mz)             -- next char in current direction
look d mz   = nc mz {dir = d}          -- next char in direction d
turn mz     = mz { dir = newDir }
  where newDir = head [d | d <- allDirections,
                       d /= (opposite (dir mz)), (look d mz) /= ' ']

step mz = mz { x = cx + dx d, y = cy + dy d, nsteps = (nsteps mz) + 1 }
  where cx = x mz
        cy = y mz
        d  = dir mz

collect mz = mz { letters = (letters mz) ++ [cc mz]} -- collect letter at point

opposite d | d == DOWN = UP
           | d == UP   = DOWN
           | d == LEFT = RIGHT
           | otherwise = LEFT

dx d | d == UP || d == DOWN    = 0
     | d == RIGHT              = 1
     | otherwise               = -1
dy d | d == RIGHT || d == LEFT = 0
     | d == UP                 = -1
     | otherwise               = 1


----------------------------------------------------------------------

bold         = SetConsoleIntensity BoldIntensity
normal       = SetConsoleIntensity NormalIntensity
color c      = SetColor Foreground Dull c
vividcolor c = SetColor Foreground Vivid c
inverse      = SetSwapForegroundBackground True
notinverse   = SetSwapForegroundBackground False

ansiPut style s = do
  setStyle
  putStr s
  where setStyle = if null style then return () else setSGR style
