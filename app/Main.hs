module Main where

import System.Random
import System.IO.Unsafe
import Data.List
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe)
import Data.Text (splitOn, pack, unpack)
newRand :: Int
newRand = unsafePerformIO randomIO
randomList :: [Double]
randomList = randoms (mkStdGen newRand)
data Cell = Numeric Int | Empty
    deriving Eq
instance Show Cell where
    show (Numeric i) = show i
    show (Empty) = " "

newtype Region = Region [[Cell]]
instance Show Region where
    show (Region cells) = unlines $ map (\x -> show x) cells

newtype SudokuBoard = Board [[Region]]
instance Show SudokuBoard where
    show (Board regions) = unlines (topRegion ++ [midSeparator] ++ midRegion ++ [midSeparator] ++ botRegion)
        where
            regionRows = concatMap regionsInRowToCells regions
            first3 = map (take 3) regionRows
            mid3 = map (take 3 . drop 3) regionRows
            last3 = map (take 3 . drop 6) regionRows
            [one, four, seven] = [take 3 first3, (take 3 . drop 3) first3, (take 3 . drop 6) first3]
            [two, five, eight] = [take 3 mid3, (take 3 . drop 3) mid3, (take 3 . drop 6) mid3]
            [three, six, nine] = [take 3 last3, (take 3 . drop 3) last3, (take 3 . drop 6) last3]
            topRegion = map (map removeListBounds) $ zipWith3 (\x y z -> show x ++ "|" ++ show y ++ "|" ++ show z) one two three
            midRegion = map (map removeListBounds) $ zipWith3 (\x y z -> show x ++ "|" ++ show y ++ "|" ++ show z) four five six
            botRegion = map (map removeListBounds) $ zipWith3 (\x y z -> show x ++ "|" ++ show y ++ "|" ++ show z) seven eight nine
            midSeparator = "-------+-------+-------"
            removeListBounds '[' = ' '
            removeListBounds ']' = ' '
            removeListBounds c = c

-- converts regions in a row to 3 rows of cells
regionsInRowToCells :: [Region] -> [[Cell]]
regionsInRowToCells [Region region] = region
regionsInRowToCells ((Region top):regions) = [top!!0 ++ rest!!0, top!!1 ++ rest!!1, top!!2 ++ rest!!2]
    where
        rest = regionsInRowToCells regions

-- converts 3 rows of cells to row of regions
cellsInRowToRegions :: [[Cell]] -> [Region]
cellsInRowToRegions cellRows = [Region first3, Region mid3, Region last3]
    where
        first3 = map (take 3) cellRows
        mid3 = map (take 3 . drop 3) cellRows
        last3 = map (take 3 . drop 6) cellRows

boardToCells :: SudokuBoard -> [[Cell]]
boardToCells (Board board) = concatMap regionsInRowToCells board

cellsToBoard :: [[Cell]] -> SudokuBoard
cellsToBoard cells = Board $ map cellsInRowToRegions [first3, mid3, last3]
    where
        first3 = take 3 cells
        mid3 = (take 3 . drop 3) cells
        last3 = (take 3 . drop 6) cells

exRegion :: Region
exRegion = Region [
    [Numeric 1, Empty, Empty],
    [Empty, Numeric 2, Empty],
    [Empty, Empty, Numeric 3]
    ]
exRegion2 :: Region
exRegion2 = Region [
    [Numeric 4, Empty, Empty],
    [Empty, Numeric 5, Empty],
    [Empty, Empty, Numeric 6]
    ]
exRegion3 :: Region
exRegion3 = Region [
    [Numeric 7, Empty, Empty],
    [Empty, Numeric 8, Empty],
    [Empty, Empty, Numeric 9]
    ]
exRegionRow = [exRegion, exRegion2, exRegion3]
exBoard :: SudokuBoard
exBoard = Board [
    [exRegion, emptyRegion, emptyRegion],
    [emptyRegion, exRegion2, emptyRegion],
    [emptyRegion, emptyRegion, exRegion3]
    ]
emptyRegion :: Region
emptyRegion = Region [
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty]
    ]
emptyBoard :: SudokuBoard
emptyBoard = Board [
    [emptyRegion, emptyRegion, emptyRegion],
    [emptyRegion, emptyRegion, emptyRegion],
    [emptyRegion, emptyRegion, emptyRegion]
    ]

main :: IO ((), SudokuBoard)
main = do
    runStateT runSudoku exBoard
    --print $ putValueOnBoard exBoard (0,2) 9
    --print $ putValueOnBoard (putValueOnBoard exBoard (0,2) 9) (3,2) 8

runSudoku :: StateT SudokuBoard IO ()
runSudoku = do
    board <- get
    liftIO $ print board
    raw <- liftIO getLine
    let inp = map (read . unpack) $ splitOn (pack " ") (pack raw) :: [Int]
    takeSudokuTurn (inp!!0, inp!!1) (inp!!2)
    runSudoku

takeSudokuTurn :: (Int,Int) -> Int -> StateT SudokuBoard IO ()
takeSudokuTurn placeToPut numToPlace = do
    board <- get
    let newBoard = fromMaybe board (putValueOnBoard board placeToPut numToPlace) 
    put newBoard

-- Kommer behöva vara maybe SudokuBoard senare
putValueOnBoard :: SudokuBoard -> (Int, Int) -> Int -> Maybe SudokuBoard
putValueOnBoard board (row,col) numberToPut 
    | isPlaceableOnBoard board (row,col) numberToPut = Just $ cellsToBoard newCellRows
    | otherwise = Nothing 
    where
        cellRows = boardToCells board
        newRow = replaceInList (cellRows!!row) (Numeric numberToPut) col
        newCellRows = replaceInList cellRows newRow row

replaceInList :: [a] -> a -> Int -> [a]
replaceInList [] _ _ = []
replaceInList (x:xs) a 0 = a : xs
replaceInList (x:xs) a i = x : replaceInList xs a (i-1)

isPlaceableOnBoard :: SudokuBoard -> (Int, Int) -> Int -> Bool
isPlaceableOnBoard board@(Board regions) (row, col) numberToPut = isNotInCol && isNotInRow && isNotInRegion && isEmpty
    where
        cellRows = boardToCells board
        transposedCellRows = transpose cellRows
        (Region selectedRegion) = regions!!(row `div` 3)!!(col `div` 3)
        isNotInRow = Numeric numberToPut `notElem` (cellRows!!row)
        isNotInCol = Numeric numberToPut `notElem` (transposedCellRows!!col)
        isNotInRegion = Numeric numberToPut `notElem` concat selectedRegion
        isEmpty = cellRows!!row!!col == Empty
