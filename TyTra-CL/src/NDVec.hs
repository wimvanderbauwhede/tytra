module NDVec where

import Prelude hiding (Foldable)

-- NDVec is a nested vector
data NDVec b = Vec [b] | MkNDVec [NDVec b] deriving (Ord,Eq,Show)

class Mappable v where
    ndmap :: (a -> b) -> (v a) -> (v b)

instance Mappable NDVec where
    ndmap f xs = case xs of
         MkNDVec ys -> MkNDVec (map (\y -> ndmap f y) ys)
         Vec ys -> Vec (map f ys)

class Foldable v where
    ndfoldl :: (a -> b -> a) -> a -> (v b) -> a

instance Foldable NDVec where
    ndfoldl f acc xs = case xs of
         MkNDVec ys -> (foldl (ndfoldl f) acc ys)
         Vec ys -> foldl f acc ys


-- Haskell tuples are fixed size so unless we use either dependent types or Template Haskell, we have to build them for tuples of fixed size
ndunzipt2 :: [Int] -> NDVec (a,b) -> (NDVec a,NDVec b)
ndunzipt2 ns v =
    let
        Vec vs' = ndmerge ns v
        (l,r) = unzip vs'
    in
        (ndsplit ns (Vec l), ndsplit ns (Vec r))

ndzipt2 :: [Int] ->  (NDVec a,NDVec b) -> NDVec (a,b)
ndzipt2 ns (l,r) =
    let
        Vec ls' = ndmerge ns l
        Vec rs' = ndmerge ns r
        vs = zip ls' rs'
    in
        ndsplit ns (Vec vs)


ndsplit :: [Int] -> NDVec a -> NDVec a
ndsplit ns v =
      let
        n:ns' = ns
        v' = split n v
      in
        if null ns' then v' else ndsplit ns' v'
--ndsplit [] v = v

split :: Int -> NDVec a -> NDVec a
split n v =
    let
        chunk n acctup elt  =
            let
                (tv,accv)=acctup
            in
                if (length tv) == n-1 then ([],accv++[tv++[elt]]) else (tv++[elt],accv)
    in
        case v of
            Vec v' -> MkNDVec $ map Vec (snd $ foldl (\acc elt -> (chunk n acc elt)) ([],[]) v')  -- this is a bare vector of something not NDVec
            MkNDVec v' -> MkNDVec $ map MkNDVec (snd $ foldl (\acc elt -> (chunk n acc elt)) ([],[]) v') -- this is a bare vector of NDVec

merge :: Int -> NDVec a -> NDVec a
merge n = ndmerge [n]

ndmerge :: [Int] -> NDVec a -> NDVec a
ndmerge ns v
    | null ns = v
    | otherwise = case v of -- Vec [b] | MkNDVec [NDVec b] => ys =
         MkNDVec ys -> case ys of
            Vec _:_ -> Vec (foldl (++) [] (map (\(Vec ws) -> ws) ys))
            MkNDVec _:_ -> ndmerge (tail ns) $ MkNDVec (foldl (++) [] (map (\(MkNDVec ws) -> ws) ys))
         Vec ys -> v
