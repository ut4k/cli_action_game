import System.IO
-- import System.Console.ANSI --TODO cabal(stack) install ansi-terminal

data Cell = Wall | Road | Player deriving (Show)
type World = [Cell]

world1 = [
           Wall, Wall, Wall, Wall, Wall,
           Road, Road, Road, Road, Road,
           Wall, Wall, Wall, Wall, Wall
         ]

cell :: Cell -> String
cell Wall = "#"
cell Road = "."
cell Player = "@"

worldWidth :: Int
worldWidth = 5 

worldToCharList :: World -> [String]
worldToCharList w = map cell w

--edit every N-th element in list (bit tricky)
-- original : ["#"," "," ","#","#",..]
-- 1. zip list with its index [("#", 1), (" ", 2), (" ", 3)...]
-- 2. map anonymous function which takes (val, index) as a parameter
-- 3. use if condition to identify N-th element
-- TODO インデックス貼る以外の方法あるんじゃない?...
-- 実際はeditというより新しいリストを返してる、すべてイミュータブルだから。
parseWorld :: [String] -> String
parseWorld c = concat $ map (\(x, idx) -> if idx `mod` worldW == 0 then x++"\n" else x) indexed
    where indexed = zip c [1..]
          worldW = worldWidth

render :: World -> String
render w = parseWorld $ worldToCharList w

getCell :: World -> Int -> Cell
getCell w n = w!!n

--リストのインデックスで置き換えたら座標の意味がないやんけ!
updateWorld :: World -> Int -> Cell -> String
updateWorld w n c = render $ replaceNth n c w

--コピペ
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

main :: IO()
main = do
    -- clearScreen
    putStrLn $ parseWorld $ worldToCharList world1
    putStrLn $ updateWorld world1 5 Player
    putStrLn $ updateWorld world1 6 Player
    putStrLn $ updateWorld world1 7 Player
    putStrLn $ updateWorld world1 8 Player
    putStrLn $ updateWorld world1 9 Player
