(* ::Package:: *)

BeginPackage["SpinorsExtras`SimpleTensor`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


SimpleTensorQ::usage =
"\
SimpleTensorQ[expr] \
returns True if expr represents simple tensor e.g. |a>[b|. Returns False \
otherwise."


SimpleTensorGetBLabel::usage =
"\
SimpleTensorGetBLabel[expr] \
returns label of B spinor from simple tensor expr."


SimpleTensorGetALabel::usage =
"\
SimpleTensorGetALabel[expr] \
returns label of A spinor from simple tensor expr."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


(* ::Subsection:: *)
(*SimpleTensorQ*)


SimpleTensorQ[expr_] := MatchQ[
	expr
	,
	_Sm | (_Sm __) | _SmBA | (_SmBA __) |
		HoldPattern @ Plus[
			(Sm[a_] | (Sm[a_] __) | SmBA[a_, _] | (SmBA[a_, _] __))..
		] |
		HoldPattern @ Plus[
			(Sm[a_] | (Sm[a_] __) | SmBA[_, a_] | (SmBA[_, a_] __))..
		]
]


(* ::Subsection:: *)
(*SimpleTensorGetBLabel*)


SimpleTensorGetBLabel[Sm[a_]] := a

SimpleTensorGetBLabel[SmBA[b_, _]] := b

SimpleTensorGetBLabel[
	HoldPattern @ Plus[
		(Sm[b_] | (Sm[b_] __) | SmBA[b_, _] | (SmBA[b_, _] __))..
	]
] := b

SimpleTensorGetBLabel[expr:(_Plus|_Times)] := SimpleTensorGetBLabel /@ expr

SimpleTensorGetBLabel[expr_] := expr


(* ::Subsection:: *)
(*SimpleTensorGetALabel*)


Options[SimpleTensorGetALabel] = {
	"GetCoefficient" -> False
}


SimpleTensorGetALabel[Sm[a_], OptionsPattern[]] := a

SimpleTensorGetALabel[Sm[a_] C__, OptionsPattern[]] :=
	If[OptionValue["GetCoefficient"],
		Times[a, C]
	(* else *),
		a
	]

SimpleTensorGetALabel[SmBA[_, a_], OptionsPattern[]] := a

SimpleTensorGetALabel[SmBA[_, a_] C__, OptionsPattern[]] :=
	If[OptionValue["GetCoefficient"],
		Times[a, C]
	(* else *),
		a
	]

SimpleTensorGetALabel[
	HoldPattern @ Plus[
		(Sm[a_] | (Sm[a_] __) | SmBA[_, a_] | (SmBA[_, a_] __))..
	]
	,
	OptionsPattern[]
] := a

SimpleTensorGetALabel[expr:_Plus, OptionsPattern[]] :=
	SimpleTensorGetALabel[#, "GetCoefficient" -> True]& /@ expr

SimpleTensorGetALabel[expr_, OptionsPattern[]] := expr


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
