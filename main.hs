data Cell = Wall Int Int
          | Road Int Int
          deriving (Show)

type World = [Cell]
type WorldWidth = Int
type Player = String
type Point = (Int, Int)

data Foo = Foo Int Int --test

player :: String -> String
player c = c

world1 = [
           (Wall 0 0), (Wall 0 1) , (Wall 0 2), (Wall 0 3), (Wall 0 4)
          ,(Road 1 0), (Road 1 1) , (Road 1 2), (Road 1 3), (Road 1 4)
          ,(Wall 2 0), (Wall 2 1) , (Wall 2 2), (Wall 2 3), (Wall 2 4)
         ]

cell :: Cell -> String
cell (Wall x y) = "#"
cell (Road x y) = "."

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
parseWorld c = concat $ map (\(x, idx) -> if idx `mod` 5 == 0 then x++"\n" else x) indexed
    where indexed = zip c [1..]

render :: World -> String
render w = parseWorld $ worldToCharList w

getCell :: World -> Int -> Cell
getCell w n = w!!n

updateWorld :: World -> a -> String
updateWorld w a = undefined
        --TODO search Cells in the World and replace it, then return new world

main :: IO()
main = putStrLn $ parseWorld $ worldToCharList world1
    where me = player "@"
