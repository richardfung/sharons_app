module ListMap
( ListMap
, add
) where

import Data.Map as M

type ListMap k v = M.Map k [v]

add :: (Ord k) => k -> v -> ListMap k v -> ListMap k v
add k v lm =
    let prev = if M.member k lm then lm M.! k
               else []
    in M.insert k (v:prev) lm
