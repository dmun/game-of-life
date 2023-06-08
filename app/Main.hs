module Main where
import Data.Array
import Control.Concurrent (threadDelay)
import System.Process
import Debug.Trace

type Grid = Array (Int, Int) Bool

gridSize :: Int
gridSize = 32

emptyGrid :: Grid
emptyGrid = array ((1, 1), (gridSize, gridSize)) [((i, j), False) | i <- [1..gridSize], j <- [1..gridSize]]

main :: IO ()
main = do
    let grid = emptyGrid // [((1, 2), True), ((2, 3), True), ((3, 1), True), ((3, 2), True), ((3, 3), True)]
    _ <- system "clear"
    gameLoop grid

gameLoop :: Grid -> IO ()
gameLoop grid = do
    drawGrid grid
    threadDelay 500000
    _ <- system "clear"
    let newGrid = updateGrid grid
    gameLoop newGrid

drawGrid :: Grid -> IO ()
drawGrid grid = do
        let rows = map drawRow [1..gridSize]
        mapM_ putStrLn rows
    where
        drawCell :: Bool -> String
        drawCell True = "██"
        drawCell False = "  "

        drawRow :: Int -> String
        drawRow r = concatMap(\c -> drawCell (grid ! (c, r))) [1..gridSize]

updateGrid :: Grid -> Grid
updateGrid grid = grid // [((x, y), updateCell grid (x, y)) | x <- [1..gridSize], y <- [1..gridSize]]

updateCell :: Grid -> (Int, Int) -> Bool
updateCell grid pos
    | alive && (neighborCount < 2 || neighborCount > 3) = False
    | not alive && (neighborCount == 3) = True
    | otherwise = alive
  where
    alive = grid ! pos
    neighborCells = filter (\(x, y) -> x >= 1 && x <= gridSize && y >= 1 && y <= gridSize) (getNeighborCells pos)
    neighborCount = length $ filter (grid !) neighborCells

getNeighborCells :: (Int, Int) -> [(Int, Int)]
getNeighborCells pos = do
    map (addIndex pos) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]
  where
    addIndex :: (Int, Int) -> (Int, Int) -> (Int, Int)
    addIndex (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

