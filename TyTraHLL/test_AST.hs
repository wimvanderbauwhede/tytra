module Main
where
import TyTraHLL.AST

{-
v_1 = map f_1 v_in
(v_1,l,v_1,r) = unzipt v_1
s_2,l = fold f_2,l s_0 v_1,l
v_2,r = map f_2,r v_1,r
v_out = map (f_3 s_2,l) v_2,r    
-}
p :: TyTraHLLProgram
p = Let [
        Assign 
            (Var "v_1" (Vec (Tuple [Prim,Prim]) 0)) 
            (Res 
                (PNDMap [] (Opaque "f1" [] Prim Prim (0,0))) 
                (Var "v0" (Vec Prim 0))),
        Assign 
            (Tuple [Var "v_1l" (Vec Prim 0),Var "v_1r" (Vec Prim 0)]) 
            (Res 
                (NDUnzipt [] (Vec (Tuple [Prim,Prim]) 0) 
                 (Var "v_1" (Vec (Tuple [Prim,Prim]) 0)))), 
        Assign 
            (Var "s_2l" Prim) 
            (Res 
                (PNDFold [] (OpaqueBin True "f_2l" [] (Var "s_0" Prim) Prim Prim (0,0))) 
                (Var "v_1l" (Vec Prim 0))),
        Assign 
            (Var "v_2r" (Vec Prim 0)) 
            (Res 
                (PNDMap [] (Opaque "f_2r" [] Prim Prim (0,0))) 
                (Var "v_1r" (Vec Prim 0))),
        Assign 
            (Var "v_out" (Vec Prim 0)) 
            (Res 
                (PNDMap [] (Opaque "f_3" [Var "s_2" Prim] Prim Prim (0,0))) 
                (Var "v_2r" (Vec Prim 0)))
    ] (Var "v_out" (Vec Prim 0))

sor :: TyTraHLLProgram
sor = Let [
        Assign 
            (Var "v_out" (Vec (Prim Float) (im*jm*km))) 
            (Res 
                (PNDMap [] (Opaque "f_sor" [] (Prim Float) (Prim Float) (c_sor,p_sor))) 
                (Var "v_in" (Vec (Prim Float) (im*jm*km))))
    ] (Var "v_out" (Vec (Prim Float) (im*jm*km)))


sor :: TyTraHLLProgram
sor = Let [
        Assign 
            (Var "v_in_s" (Vec (Prim Float) (im*jm*km))) 
            (Res 
                (NDSplit [])
                (Var "v_in" (Vec (Prim Float) (im*jm*km)))),
        Assign 
            (Var "v_out_m" (Vec (Prim Float) (im*jm*km))) 
            (Res 
                (NDMap [] Pipe (Opaque "f_sor" [] (Prim Float) (Prim Float) (c_sor,p_sor))) 
                (Var "v_in_s" (Vec (Prim Float) (im*jm*km)))),
        Assign 
            (Var "v_out" (Vec (Prim Float) (im*jm*km))) 
            (Res 
                (NDMerge []) 
                (Var "v_out_m" (Vec (Prim Float) (im*jm*km))))
    ] (Var "v_out" (Vec (Prim Float) (im*jm*km)))       
