module Matrix where

import Array
import Array (Array, getOrFail, set)
import Utils (arrayZipWith)
import Utils
import List
import Error (raise)
import Debug (log)

-- Width, height and content
data Matrix a = Matrix Int Int (Array a)

repeat : Int -> Int -> a -> Matrix a
repeat w h v =
  if w <= 0 || h <= 0
  then raise <| "Invalid dimensions " ++ show (w, h)
  else Matrix w h <| Array.repeat (w * h) v

dimensions : Matrix a -> (Int, Int)
dimensions (Matrix w h _) = (w, h)

inner : Matrix a -> Array a
inner (Matrix _ _ a) = a

width : Matrix a -> Int
width (Matrix w _ _) = w

height : Matrix a -> Int
height (Matrix _ h _) = h

walk : (Int -> Int -> Matrix a -> Matrix a) -> Matrix a -> Matrix a
walk func matrix =
  let w = width matrix
      m = inner matrix
  in foldl (\i m-> func (i % w) (i // w) m)
           matrix
           [0..Array.length m - 1]

map : (a -> b) -> Matrix a -> Matrix b
map func matrix =
  let (w, h) = dimensions matrix
  in Matrix w h <| Array.map func (inner matrix)

set : a -> Int -> Int -> Matrix a -> Matrix a
set v = modify (\_ -> v)

get : Int -> Int -> Matrix a -> a
get x y matrix =
  let w = width matrix
  in getOrFail (x + y * w) (inner matrix) 

modify : (a -> a) -> Int -> Int -> Matrix a -> Matrix a
modify f x y matrix =
  let w = width matrix
  in Matrix w
            (height matrix)
            <| Utils.modify (x + y * w) f (inner matrix)

merge : (a -> a -> a) -> Matrix a -> Matrix a -> Matrix a
merge f a b =
  if (width a)  /= (width b)
  || (height a) /= (height b)
  then raise <| "Dimensions don't match: " ++ show (dimensions a)
                                           ++ " /= "
                                           ++ show (dimensions b)
  else Matrix (width a)
              (height b)
              <| arrayZipWith f (inner a) (inner b)

rows : Matrix a -> [Array a]
rows matrix =
  let w = width matrix
      m = inner matrix
  in List.map (\r -> Array.slice (r * w)
                                 ((r + 1) * w)
                                 m)
              [0..height matrix - 1] 
