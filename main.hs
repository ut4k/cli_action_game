data Cell = Wall Int Int
          | Road Int Int

type World = [Cell]

world1 = [
          (Wall 0 0), (Wall 0 1) , (Wall 0 2), (Wall 0 3), (Wall 0 4), (Wall 0 5),
          (Road 1 0), (Road 1 1) , (Road 1 2), (Road 1 3), (Road 1 4), (Road 1 5),
          (Wall 2 0), (Wall 2 1) , (Wall 2 2), (Wall 2 3), (Wall 2 4), (Wall 2 5)
         ]

cell :: Cell -> String 
cell (Wall x y) = "#"
cell (Road x y) = " "

main :: IO()
main =
        print $ map cell world1
