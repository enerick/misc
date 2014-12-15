module EightQueen where
import Data.List

type Board = [Int]

isEightQueen :: Board -> Bool
isEightQueen [] = True
isEightQueen (q:qs) = isPuttable qs (q+1) (q-1) && isEightQueen qs

isPuttable :: Board -> Int -> Int -> Bool
isPuttable [] _ _ = True
isPuttable (q:qs) u l = q /= u && q /= l && isPuttable qs (u+1) (l-1)

genQueen :: [Board]
genQueen = filter isEightQueen (permutations [0..7])
