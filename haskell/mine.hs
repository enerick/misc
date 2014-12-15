{-

 簡易マインスイーパ

 mainを呼ぶとゲームが始まる。座標は1-originで指定する。
 数字マスをOpenした時の動作はWindowsのマインスイーパーと同じ（だと思う）。

-}
module Mine where

import qualified Data.Map as M
import Control.Exception as E
import System.Random
import System.IO

-- データ型と表示関数
data Game = Play Int
          | Over
          | Clear
            deriving (Show, Eq)

data Cell = Safe
          | Mine
          | Near Int
            deriving (Show, Eq)

data State = Flag
           | Open
           | Close
             deriving (Show, Eq)

type Box = (Cell, State)
type Board = M.Map (Int, Int) Box

-- 各セルを文字列に
showCell :: Box -> String
showCell (_,Close) = "_"
showCell (_,Flag) = "F"
showCell (Safe, Open) = "."
showCell (Mine, Open) = "@"
showCell (Near x, Open) = show x

-- 盤面を文字列に
showBoard :: Board -> String
showBoard b = showBoard' locate (M.map showCell b)
  where
    locate = map (\x -> map (\y -> (x,y)) [1..5]) [1..5]
    showBoard' [] b' = ""
    showBoard' (x:xs) b' = foldr (\x y -> (b' M.! x)++" "++y) "" x ++ "\n" ++ showBoard' xs b'

-- 初期化
-- 場を作る
initBoard :: Int -> IO Board
initBoard num = setMine init num
  where
    locate = map (\x -> map (\y -> (x,y)) [1..5]) [1..5]
    init = M.fromList $ map (\x -> (x,safe)) $ concat locate
    safe = (Safe, Close)

-- 地雷設置
setMine :: Board -> Int -> IO Board
setMine b num = do
  list <- mines num
  return $ foldr (\k y -> M.update (\x -> Just (Mine, Close)) k y) b list 

-- ランダムな座標の組を返す
mines :: Int -> IO [(Int, Int)]
mines n = mines' n locate []
  where
    mines' 0 loc choz = return choz
    mines' n loc choz = do
      x <- rand
      let key = loc !! x
      mines' (n-1) (del key loc) (key:choz)
      where
        rand = getStdRandom (randomR (0, (length loc)-1)) :: IO Int
    locate = concat $ map (\x -> map (\y -> (x,y)) [1..5]) [1..5]
    
-- Data.List.deleteがなんか呼べなかったので書いた
del :: Eq a => a -> [a] -> [a]
del key list = del' list []
  where
    del' [] l = reverse l
    del' (x:xs) l =
      if x == key then del' xs l
      else del' xs (x:l)
    
-- アルゴリズム部分
-- 周囲8マスの地雷の数
check8 :: Board -> Int -> Int -> Int
check8 b line row = length $ M.toList filtered
  where
    plmi1 base x = base-1 <= x && x <= base+1
    filtered = M.filterWithKey
               (\(li,ro) (x,_) -> plmi1 line li && plmi1 row ro && x == Mine) b

-- 周囲8マスの旗の数
check8flag :: Board -> Int -> Int -> Int
check8flag b line row = length $ M.toList filtered
  where
    plmi1 base x = base-1 <= x && x <= base+1
    filtered = M.filterWithKey
               (\(li,ro) (_,x) -> plmi1 line li && plmi1 row ro && x == Flag) b

-- 開ける
open :: (Board, Game) -> Int -> Int -> (Board, Game)
open status@(b, st) line row =
  case b M.! (line,row) of
    (Near n, Open) -> if flags >= n then open8 status line row else status
      where flags = check8flag b line row
    (_, Open) -> status
    (_, Flag) -> status -- 開いているところと旗のマスは開けられない
    (Mine, _) -> (up (Mine, Open) (line, row) b, Over) --ゲーム終了
    (Safe, _) -> if near == 0
                 then open8 (up (Safe, Open) (line, row) b, st) line row
                 else (up (Near near, Open) (line, row) b, st) -- Nearを作る
      where near = check8 b line row
  where
    up box k b' = M.update (\x -> Just box) k b'

-- 周囲8マスを開ける（開けるアクション自体はopenに投げる）
open8 :: (Board, Game) -> Int -> Int -> (Board, Game)
open8 (b,st) line row = foldr (\(li, ro) b' -> open b' li ro) (b,st) ei
  where
    plmi1 base x = base-1 <= x && x <= base+1
    filtered = M.filterWithKey
               (\(li,ro) (_,x) -> plmi1 line li && plmi1 row ro && x == Close) b
    ei = M.keys filtered

-- 命令を実行する
action :: (Board, Game) -> Int -> Int -> Char -> (Board, Game)
action b line row 'o' = open b line row
action (b, Play n) line row 'f' =
  case b M.! (line, row) of
    (_, Close) -> (up Flag, Play $ n-1) --旗を立ててRestを1減らす
    (_, Flag) -> (up Close, Play $ n+1)
    (_, Open) -> (b, Play n) --旗は立てられない
  where
    up state = M.update (\(c,_) -> Just (c, state)) (line, row) b
  
-- main
main :: IO ()
main = do
  putStr "注意: 1-originです\n"
  b <- initBoard 5
  judge b (Play 5) --TODO 地雷の数
  
-- 判定と盤面の状態表示
judge :: Board -> Game -> IO ()
judge b Clear = do
  putStrLn "!!!!!!!CLEAR!!!!!!!!!\n"
  putStrLn $ showBoard $ openAll b
judge b Over = do
  putStrLn "*******Game Over******"
  putStr $ showBoard $ openAll b
judge b st@(Play n) = do
  putStrLn $ "Rest : " ++ show n
  putStrLn $ showBoard b
  safegame b st

-- 最後に全部オープンする
openAll :: Board -> Board
openAll b = M.map (\(c, st) -> (c, Open)) b

-- クリア判定
judgeClear :: Board -> Bool
judgeClear b = if remain == M.empty then True else False
  where remain = M.filter (\(c,s) -> (c /= Mine && s == Close) || (c /= Mine && s == Flag)) b
    
-- コマンド入力 (ここで盤面の状態が変わる）
game :: Board -> Game -> IO ()
game b st = do
  putStr "input Line (-) : "
  hFlush stdout
  line <- getLine
  putStr "input Row  (|) : "
  hFlush stdout
  row <- getLine
  putStr "input Command (o:open, f:flag, n:nothing, q:quit) : "
  hFlush stdout
  op <- getChar
  getLine
  (case op of
      'q' -> return ()
      'n' -> do 
        judge b st
      _ -> do
        let (newb,g) = action (b,st) (read line :: Int) (read row :: Int) op
        if judgeClear newb
          then judge newb Clear
          else judge newb g)

-- 例外を拾う為のラッパ
safegame :: Board -> Game -> IO ()
safegame b st = game b st `E.catch` retry b st

retry :: Board -> Game -> SomeException -> IO ()
retry b st _ = do
  putStr "Error. retry!\n"
  judge b st
