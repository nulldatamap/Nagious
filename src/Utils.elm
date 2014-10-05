module Utils where
import Dict (Dict)
import Dict
import Array
import Array (Array, set, getOrFail)
import Error (raise)

unreachable = raise <| "Unreachable"

-- Keeps the elements from a that also are in b,
-- and adds the new elements from b into the mix.
pickyUnion : Dict comparable b -> Dict comparable b -> Dict comparable b
pickyUnion a b =
  Dict.union (Dict.intersect a b) b

-- zipWith for Arrays
arrayZipWith : (a -> b -> c) -> Array a -> Array b -> Array c
arrayZipWith f a b =
  let zw i = f (getOrFail i a) <| getOrFail i b
  in Array.map zw <| Array.initialize (min (Array.length a) (Array.length b)) identity

-- Modifies an element of an Array
modify : Int -> (a -> a) -> Array a -> Array a
modify i f a = set i (f <| getOrFail i a) a

-- Finds the first element that satisfies the given predicate
search : ( a -> Bool ) -> [a] -> Maybe a
search pred a =
  case a of
    [] -> Nothing
    (x::xs) -> if pred x
                  then Just x
                  else search pred xs
