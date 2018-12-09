-- わたし
me :: String
me = "@"

-- 壁と道のデータタイプどう作る?
-- リストにしたとき壁かまたは道か、座標はどこか
-- の２種類の識別情報をもっていないといけないはず

-- 歩けるところ
type Road = String
-- 壁
type Wall = String
-- 座標 ベクターだかCoordだか
type Vector = (Int, Int)
-- 1マス
data MapCell = Road Vector | Wall Vector deriving (Show)
-- マップ全体
type World = [MapCell]

-- ステージ1とする
world1 :: World
world1 = [
          Wall(0, 0) ,Wall(0, 1) ,Wall(0, 2) ,Wall(0, 3) ,Wall(0, 4) ,Wall(0, 5)
         ,Road(1, 0) ,Road(1, 1) ,Road(1, 2) ,Road(1, 3) ,Road(1, 4) ,Road(1, 5)
         ,Wall(2, 0) ,Wall(2, 1) ,Wall(2, 2) ,Wall(2, 3) ,Wall(2, 4) ,Wall(2, 5)
        ]

-- いちいち値コンストラクタの引数1もマッチさせないといけない
-- ので (Road _)の _の部分が要るっぽい
setCell :: MapCell -> (String, Vector)
setCell (Road a) = (".", a)
setCell (Wall a) = ("#", a)

-- データタイプをみて" "か"#"にわざわざ変える・・・
-- 変換は無駄な処理だから最初からWallやRoadコンストラクタが#とか" "をもってればいんじゃね？
-- typeはシノニムを作るだけだから
-- type Wall = "#" とかはできないはず
flattenCells :: World -> [(String, Vector)]
flattenCells a = map setCell $ a

--ワールド設定から最大幅、最大高さを得る
-- getScreenSize :: World -> (Int,Int)
getScreenSize a = (4, 5)
    where xlist = [ x | x <- a]

-- ゲームはじまり!
main = do
   return $ flattenCells world1
