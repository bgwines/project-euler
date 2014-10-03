(* ::Package:: *)

(* ::Input:: *)
(*NumDivisors[x_]:=NumDivisors[x]=Length[Divisors[x]]*)


(* ::Input:: *)
(*SameNumDivisorsAsSucc[x_]:=NumDivisors[x]==NumDivisors[x+1]*)


(* ::InheritFromParent:: *)
(*Timing[Length[Select[Range[10^7-1],SameNumDivisorsAsSucc]]]*)
