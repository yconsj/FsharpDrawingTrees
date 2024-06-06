// Michael R Hansen    06-06-2023
// 


#r "nuget: FsCheck";;

open FsCheck

Check.Quick (fun x -> 2.0 * x = x+x);;  

let myFloatGen = gen { let! f = Arb.generate<NormalFloat>
                       return NormalFloat.op_Explicit f}


let myFloatGen' = Gen.map NormalFloat.op_Explicit Arb.generate<NormalFloat>;;


type MyGenerators =
  static member float() =
      {new Arbitrary<float>() with
           override x.Generator  = myFloatGen
           override x.Shrinker f =  //Seq.empty -- no shrinker 
                                    seq [f / 2.0]
      };;

Arb.register<MyGenerators>();;

let prop1 x = 2.0*x = x + x + 0.0001;; 
Check.Quick prop1;;

