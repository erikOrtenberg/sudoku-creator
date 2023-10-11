module Main where

import System.Random
import System.IO.Unsafe
import Data.List
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (splitOn, pack, unpack)
import Text.Read (readMaybe)
import GHC.Real (reduce)
import Data.Array.IO

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

getRegionIndex :: (Int, Int) -> (Int, Int)
getRegionIndex (row, col) = (row `div` 3, col `div` 3)

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


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
exFilledBoard ::SudokuBoard
exFilledBoard = Board [
    [Region [[Numeric 1,Numeric 6,Numeric 4],[Numeric 9,Numeric 5,Numeric 7],[Numeric 3,Numeric 8,Numeric 2]],
    Region [[Numeric 8,Numeric 9,Numeric 3],[Numeric 1,Numeric 2,Numeric 6],[Numeric 7,Numeric 4,Numeric 5]],
    Region [[Numeric 2,Numeric 7,Numeric 5],[Numeric 3,Numeric 8,Numeric 4],[Numeric 6,Numeric 1,Numeric 9]]],
    [Region [[Numeric 5,Numeric 1,Numeric 9],[Numeric 4,Numeric 7,Numeric 8],[Numeric 2,Numeric 3,Numeric 6]],
    Region [[Numeric 4,Numeric 6,Numeric 8],[Numeric 3,Numeric 1,Numeric 2],[Numeric 5,Numeric 7,Numeric 9]],
    Region [[Numeric 7,Numeric 3,Numeric 2],[Numeric 9,Numeric 5,Numeric 6],[Numeric 8,Numeric 4,Numeric 1]]],
    [Region [[Numeric 7,Numeric 2,Numeric 3],[Numeric 6,Numeric 4,Numeric 1],[Numeric 8,Numeric 9,Numeric 5]],
    Region [[Numeric 9,Numeric 5,Numeric 4],[Numeric 2,Numeric 8,Numeric 7],[Numeric 6,Numeric 3,Numeric 1]],
    Region [[Numeric 1,Numeric 6,Numeric 8],[Numeric 5,Numeric 9,Numeric 3],[Numeric 4,Numeric 2,Numeric 7]]]
    ]

main :: IO ()
main = do
    --(_, board) <- runStateT generateRandomBoard emptyBoard
    let board = exFilledBoard
    print board
    newBoard <- removeFully board [lastPossibleInRow, lastPossibleInCol, lastPossibleInRegion]
    --print newBoard
    (_, wowie) <- runStateT runSudoku newBoard
    return ()
    --print $ putValueOnBoard exBoard (0,2) 9
    --print $ putValueOnBoard (putValueOnBoard exBoard (0,2) 9) (3,2) 8

getInput :: IO [Int]
getInput = do
    putStrLn "Please enter a number on the board. [row] [col] [num]"
    raw <- getLine
    let inp = map (readMaybe . unpack) $ splitOn (pack " ") (pack raw) :: [Maybe Int]
    let containsNothing = any isNothing inp || length inp /= 3
    if containsNothing then do
        putStrLn "Incorrect input, try again"
        getInput
    else do
        let values = map (fromMaybe (-1)) inp
        if any (\x -> x < 1 || x > 9) values then do
            putStrLn "Incorrect input, try again"
            getInput
        else
            return [values!!0 - 1, values!!1 - 1, values!!2]

--- Sudoku runner 

runSudoku :: StateT SudokuBoard IO ()
runSudoku = do
    board <- get
    liftIO $ print board
    inp <- liftIO getInput
    takeSudokuTurn (inp!!0, inp!!1) (inp!!2)
    runSudoku

takeSudokuTurn :: (Int,Int) -> Int -> StateT SudokuBoard IO ()
takeSudokuTurn placeToPut numToPlace = do
    board <- get
    let newBoard = putValueOnBoard board placeToPut numToPlace
    if isNothing newBoard then
        liftIO $ putStrLn "Incorrect input, try again"
    else
        put $ fromMaybe board newBoard

removeValueOnBoard :: SudokuBoard -> (Int, Int) -> SudokuBoard
removeValueOnBoard board (row, col) = cellsToBoard newCellRows
    where
        cells = boardToCells board
        newRow = replaceInList (cells!!row) Empty col
        newCellRows = replaceInList cells newRow row

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

getPlaceableOnBoard :: SudokuBoard -> (Int, Int) -> [Int]
getPlaceableOnBoard board pos = filter (isPlaceableOnBoard board pos) [1..9]

--- Random generation ---
generateRandomBoard :: StateT SudokuBoard IO ()
generateRandomBoard = do
    board <- get
    let placeToPut = getEmptyCell board
    if placeToPut == (-1, -1) then
        return ()
    else do
        let placeable = getPlaceableOnBoard board placeToPut
        if null placeable then do
            put emptyBoard
        else do
            placeableIndex <- liftIO $ getStdRandom (randomR (0,length placeable - 1)) :: StateT SudokuBoard IO Int
            let newBoard = putValueOnBoard board placeToPut (placeable!!placeableIndex)
            if isNothing newBoard then
                put board
            else
                put $ fromMaybe board newBoard
        generateRandomBoard

getEmptyCell :: SudokuBoard -> (Int, Int)
getEmptyCell board = traverse 0 0
    where
        cells = boardToCells board
        traverse x y
            | y > 8 = (-1, -1)
            | cells!!x!!y == Empty = (x,y)
            | otherwise = traverse ((x + 1) `mod` 9) (y + ((x + 1) `div` 9))


--- Reduction design ---


-- Rules
type Removeable = SudokuBoard -> (Int, Int) -> Bool

lastInRegion :: Removeable
lastInRegion (Board regions) pos = Empty `notElem` concat cells
    where
        (regRow, regCol) = getRegionIndex pos
        (Region cells) = regions!!regRow!!regCol

lastInRow :: Removeable
lastInRow board (row, _) = Empty `notElem` selectedRow
    where
        cells = boardToCells board
        selectedRow = cells!!row

lastInCol :: Removeable
lastInCol board (_, col) = Empty `notElem` selectedCol
    where
        cells = transpose $ boardToCells board
        selectedCol = cells!!col

lastPossibleInRegion :: Removeable
lastPossibleInRegion board@(Board regions) pos@(row,col) = all (valueAtPos `elem`) rowsAndCols 
    where
        (regRow, regCol) = getRegionIndex pos
        cells = boardToCells board
        tCells = transpose cells
        valueAtPos = cells!!row!!col
        remainingRowIndecies = filter (row /=) [regRow * 3..regRow * 3 + 2]
        remainingColIndecies = filter (col /=) [regCol * 3..regCol * 3 + 2]
        rowsAndCols = map (cells!!) remainingRowIndecies ++ map (tCells!!) remainingColIndecies

lastPossibleInRow :: Removeable
lastPossibleInRow board pos@(row,col) = all myFunc [0..8]
    where   
        (Board regions) = removeValueOnBoard board pos
        rows = boardToCells board
        cols = transpose rows
        --thisRow = rows!!row
        numberAtPos = rows!!row!!col
        myFunc :: Int -> Bool
        myFunc column = rows!!row!!column /= Empty || numberAtPos `elem` (cols!!column ++ regionCells) 
            where 
                (regionRow, regionCol) = getRegionIndex (row, column)
                (Region currRegion) = regions!!regionRow!!regionCol
                regionCells = concat currRegion

lastPossibleInCol :: Removeable
lastPossibleInCol board (row,col) = lastPossibleInRow tBoard (col, row)
    where
        cells = boardToCells board
        tcells = transpose cells
        tBoard = cellsToBoard tcells



-- Apply rules function
removeFully :: SudokuBoard -> [Removeable] -> IO SudokuBoard
removeFully board rules = do
    let allIndecies = [(x,y) | x <- [0..8], y <- [0..8]]
    mixed <- shuffle allIndecies
    return $ removeFully' board rules mixed


removeFully' :: SudokuBoard -> [Removeable] -> [(Int, Int)] -> SudokuBoard
removeFully' board _ [] = board
removeFully' board rules (pos:xs)
    | checks = removeFully' (removeValueOnBoard board pos) rules xs
    | otherwise = removeFully' board rules xs
    where
        checks = any (\rule -> rule board pos) rules