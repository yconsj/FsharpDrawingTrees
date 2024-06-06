// Michael R. Hansen  01-06-2023
#r "nuget: FsCheck";;

open FsCheck

let rec sumA xs acc = match xs with
                      | []    -> 0
                      | x::xs -> sumA xs (x+acc);;   

let sumRefProp xs = List.sum xs = sumA xs 0;;

let _ = Check.Quick sumRefProp;;

let _ = Check.Verbose sumRefProp;; 

let commutative (x:float) (y:float) = x+y = y+x;;

let _ = Check.Verbose commutative;;

let assoc (x:float) (y:float) z = x+(y+z) = (x+y)+z;;

let _ = Check.Verbose assoc;;

(*
Falsifiable, after 9 tests (2 shrinks) (StdGen (409704088, 297044298)):
Original:
9.284046588
-7.80755089
-6.220687321
Shrunk:
9.0
-7.0
-6.220687321
*)

let withIn (tol:float)  x y = abs (x-y) <= tol 

let assocTol tol x y z = withIn tol (x+(y+z)) ((x+y)+z);; 

let _ = Check.Quick (assocTol 1.0E-10);;
(*
Falsifiable, after 1 test (2 shrinks) (StdGen (1721063886, 297044300)):
Original:
-infinity
-0.2509589142
4.940656458e-324
Shrunk:
-infinity
0.0
0.0
*)

let assocTolNF xn yn zn = 
   let x = NormalFloat.op_Explicit xn
   let y = NormalFloat.op_Explicit yn
   let z = NormalFloat.op_Explicit zn
   assocTol 1.0E-10 x y z;;

let _ = Check.Quick assocTolNF;;
// Ok, passed 100 tests.

