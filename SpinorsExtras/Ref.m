(* ::Package:: *)

BeginPackage["SpinorsExtras`Ref`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


SpRef::usage =
"\
SpRef[P] \
represents default reference vector associated with four-vector P.\

It's a lightlike vector with default numerical components: \
[+/- Sqrt[P1^2 + P2^2 + P3^2], -P1, -P2, -P3] for P = [P0, P1, P2, P3]."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]


(* ::Subsection:: *)
(*Spinor declarations*)


QuietSpinorPrint[
	DeclareSpinor[
		HoldPattern[SpRef[_]]
	];
]


(* ::Subsection:: *)
(*SpRef*)


SpRef[i_Integer] := SpRef[Sp[i]]


(* ::Subsection:: *)
(*DeclareSpinorMomentum[SpRef[...]]*)


(* Default reference vector. *)
DeclareSpinorMomentum[SpRef[P_]] ^:=
	If[MatchQ[Num4V[P], {_, _, _, _}],
		DeclareSpinorMomentum[
			SpRef[P],
			{
				If[TrueQ[Negative[Num4V[P][[1]]]], -1, 1] *
					Sqrt[Total[Num4V[P][[2;;4]]^2]]
				,
				-Num4V[P][[2]],
				-Num4V[P][[3]],
				-Num4V[P][[4]]
			}
		],
	(* else *)
		Message[DeclareSpinorMomentum::firstDeclare, SpRef[P], P];
		$Failed
	]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
