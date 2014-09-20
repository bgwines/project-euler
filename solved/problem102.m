(* ::Package:: *)

(* ::Input:: *)
(*SameSide[p1_, p2_, a_, b_] :=Dot[Cross[Append[(b-a), 0],Append[(p1-a),0]],Cross[Append[(b-a),0],Append[(p2-a),0]]] >= 0*)


(* ::Input:: *)
(*PointInTriangle[p_, {a_, b_, c_}] :=SameSide[p, a, b, c]&& SameSide[p, b , a, c]&& SameSide[p, c, a, b]*)


(* ::Input:: *)
(*triangles = {{{-340,495},...
(**)


(* ::Input:: *)
(*Length[Select[PointInTriangle[{0,0}, #] &/@ triangles, Identity]]*)
