-- example TyTra-HLL program
p_in :: Vec (im*jm*km) Float
f_sor :: Float -> Float
f_sor2 :: Float -> Float -> Float
f_acc :: Float -> Float -> Float

p_out :: Vec (im*jm*km) Float
p_out = 
    let
        p_tmp :: Vec (im*jm*km) Float
        p_tmp = map f_sor p_in;
        s_tmp2 :: Float
        acc :: Float 
        acc = 0.0
        s_tmp2 = fold f_acc acc p_tmp
        p_tmp2 :: Vec (im*jm*km) Float
        p_tmp2 = map f_sor p_tmp
    in 
        map (f_sor2 s_tmp2) p_tmp

