module Main where

import NDVec

v = Vec [1 .. (2*4*2*2*3) ]
v2 = ndsplit [2,4,2,2] v
main = do
    putStrLn $ show  v
    putStrLn $ show $ split 2 v
    putStrLn $ show $ split 2 $ split 2 $ split 4 $ split 2 v
    putStrLn $ show $ ndsplit [2,4,2,2] v 
    putStrLn $ show $ ((split 2 $ split 2 $ split 4 $ split 2 v)==(ndsplit [2,4,2,2] v)) 
    putStrLn $ show $ ndmerge [2,4,2,2] $ ndsplit [2,4,2,2] v  -- ndmerge [2,4,2,2] ref
    putStrLn $ show $ ndzipt2 [2,4,2,2] (v2,v2)
    putStrLn $ show $ ((v2,v2) == ndunzipt2 [2,4,2,2] ( ndzipt2 [2,4,2,2] (v2,v2)))

ref = [ [ [ [1,2],[3,4],[1,2],[3,4] ], [ [1,2],[3,4],[1,2],[3,4] ] ], [ [ [1,2],[3,4],[1,2],[3,4] ], [ [1,2],[3,4],[1,2],[3,4] ] ], [ [ [1,2],[3,4],[1,2],[3,4] ], [ [1,2],[3,4],[1,2],[3,4] ] ] ] 

{-
MkNDVec [
    MkNDVec [
        MkNDVec [
            Vec [1,2],Vec [4,5],Vec [7,8],Vec [10,11]
        ],
        MkNDVec [
            Vec [16,17],Vec [19,20],Vec [22,23],Vec [25,26]
        ]
    ],
    MkNDVec [
        MkNDVec [Vec [46,47],Vec [49,50],Vec [52,53],Vec [55,56]
        ],
        MkNDVec [Vec [61,62],Vec [64,65],Vec [67,68],Vec [70,71]
        ]
        ]
        ]
-}

{-
[[[[[a]<2>]<4>]<2>]<2>]<3>    

[a]<2*4*2*2*3>
[[a]<2*4*2*2>]<3>
[[[a]<2*4*2>]<2>]<3>
[[[[a]<2*4>]<2>]<2>]<3>
[[[[[a]<2>]<4>]<2>]<2>]<3>

split k [a]<n> => [[a]<k>]<n/k>
ndsplit [2,4,2,2] v 
[a]<2*4*2*2*3> => split 2
[[a]<2>]<4*2*2*3>
[[[a]<2>]<4>]<2*2*3>
[[[[a]<2>]<4>]<2>]<2*3>
[ [[[[a]<2>]<4>]<2>]<2> ]<3>
-}
