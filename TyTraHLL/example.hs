-- example TyTra-HLL program
p_in :: Vec (im*jm*km) Float
f_sor :: Float -> Float
f_sor2 :: Float -> Float

p_out :: Vec (im*jm*km) Float
p_out = 
    let
        p_tmp :: Vec (im*jm*km) Float
        p_tmp = map f_sor p_in
    in 
        map f_sor2 p_tmp

