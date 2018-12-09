import Data.Ord
import Data.List
import Control.Monad

--わたし
me :: String
me = "@"

--壁と道は0か1で判断、
--でも敵とかトゲの概念が出てきたらどうする?
--2,3,4と種別を増やす?...

type MapCell = Int

--マップ全体
type World = [[MapCell]]

--ステージ1とする
world1 :: World
world1 = [
          [1,1,1,1,1]
         ,[0,0,0,0,0]
         ,[1,1,1,1,1]
         ]

--まさに見た目通り
--壁壁壁壁壁
--道道道道道
--壁壁壁壁壁

--ワールドのリストの中で最長のリストの数 -> 最大画面幅
--ワールドのリストの数 -> 最大画面高さ
getScreenSize :: World -> (Int, Int)
getScreenSize a = (maxX, maxY)
    where maxX = length $ maximumBy (comparing length) a
          maxY = length a

convLine :: [MapCell] -> String
convLine a = concat $ map convCell a ++ ["\n"]
-- 最後の"\n"いらない

convCell :: MapCell -> String
convCell 0 = " "
convCell 1 = "#"
-- 01以外だとエクセプション

renderWorld :: World -> IO()
renderWorld a = putStrLn $ concatMap convLine a

--ゲームはじまり!
main = do
       renderWorld world1
