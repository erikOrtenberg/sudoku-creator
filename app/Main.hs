module Main where

import System.Random
import System.IO.Unsafe
newRand :: Int
newRand = unsafePerformIO randomIO 
randomList :: [Double]
randomList = randoms (mkStdGen newRand)
data Cell = Numeric Int | Empty
instance Show Cell where
    show (Numeric i) = show i
    show (Empty) = " "
type Region = [[Cell]]
type SudokuBoard = [[Region]]

exRegion = [
    [Numeric 2, Empty, Empty],
    [Numeric 2, Empty, Empty],
    [Numeric 2, Empty, Empty]
    ]
exRegion2 = [
    [Numeric 3, Empty, Empty],
    [Numeric 3, Empty, Empty],
    [Numeric 3, Empty, Empty]
    ]
exRegion3 = [
    [Numeric 4, Empty, Empty],
    [Numeric 4, Empty, Empty],
    [Numeric 4, Empty, Empty]
    ]
exBoard = [
    [exRegion, exRegion, exRegion],
    [exRegion2, exRegion2, exRegion2],
    [exRegion3, exRegion3, exRegion3]
    ]
emptyRegion :: Region
emptyRegion = [
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty]
    ]
emptyBoard :: SudokuBoard
emptyBoard = [
    [emptyRegion, emptyRegion, emptyRegion],
    [emptyRegion, emptyRegion, emptyRegion],
    [emptyRegion, emptyRegion, emptyRegion]
    ]

main :: IO ()
main = do
    showBoard exBoard
    mapM_ putStrLn (map show generateBoard)

showBoard :: SudokuBoard -> IO()
showBoard b = mapM_ putStrLn strRegionRows
    where
        regionRows = concat $ map mergeRegionsInRow b
        strRegionRows = map show regionRows

mergeRegionsInRow :: [Region] -> [[Cell]]
mergeRegionsInRow (region:[]) = region
mergeRegionsInRow (top:regions) = [top!!0 ++ rest!!0, top!!1 ++ rest!!1, top!!2 ++ rest!!2]
    where
        rest = mergeRegionsInRow regions

generateBoard :: [[Cell]]
generateBoard = getRandomBoard
    where
        regionRows = concat $ map mergeRegionsInRow emptyBoard

getRandomBoard :: [[Cell]]
getRandomBoard = [one, two, three, four, five, six, seven, eight, nine]
    where
        (one, tmp1) = getRandomRow randomList 
        (two, tmp2) = getRandomRow tmp1 
        (three, tmp3) = getRandomRow tmp2 
        (four, tmp4) = getRandomRow tmp3
        (five, tmp5) = getRandomRow tmp4
        (six, tmp6) = getRandomRow tmp5
        (seven, tmp7) = getRandomRow tmp6
        (eight, tmp8) = getRandomRow tmp7
        (nine, tmp9) = getRandomRow tmp8

getRandomRow :: [Double] -> ([Cell], [Double])
getRandomRow xs = ([one, two, three, four, five, six, seven, eight, nine], tmp9)
    where
        (one, tmp1) = getRandomCell xs 
        (two, tmp2) = getRandomCell tmp1 
        (three, tmp3) = getRandomCell tmp2 
        (four, tmp4) = getRandomCell tmp3
        (five, tmp5) = getRandomCell tmp4
        (six, tmp6) = getRandomCell tmp5
        (seven, tmp7) = getRandomCell tmp6
        (eight, tmp8) = getRandomCell tmp7
        (nine, tmp9) = getRandomCell tmp8

getRandomCell :: [Double] -> (Cell, [Double])
getRandomCell (x:xs) = (Numeric (floor (x * 10)), xs) 