(* ::Package:: *)

BeginPackage["SpinorsExtras`Decompose`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


DecomposeASpinor::usage =
"\
DecomposeASpinor[expr, a -> {b, c}] \
returns expr with all ocurences of spinor |a> decomposed in basis {|b>, |c>}:
|a> = <c|a>/<c|b> |b> + <b|a>/<b|c> |c>."


DecomposeBSpinor::usage =
"\
DecomposeBSpinor[expr, a -> {b, c}] \
returns expr with all ocurences of spinor |a] decomposed in basis {|b], |c]}:
|a] = [c|a]/[c|b] |b] + [b|a]/[b|c] |c]."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]
Needs["SpinorsExtras`Proportional`"]


SetAttributes[Factory, HoldAll]

Factory[
	tag_Symbol,
	propQFunction_Symbol,
	replaceFunction_Symbol,
	spProdType_Symbol
] := (
	tag[
		x_,
		_?SpinorQ -> {b_?SpinorQ, c_?SpinorQ}/; TrueQ[propQFunction[b, c]]
	] := (
		Message[tag::wrongBasis, b, c];
		x
	);

	tag[x_, a_?SpinorQ -> {b_?SpinorQ, c_?SpinorQ}] :=
		replaceFunction[
			x
			,
			a
			,
			spProdType[c, a]/spProdType[c, b] b +
				spProdType[b, a]/spProdType[b, c] c
		];
	
	tag[
		x_,
		a_?SpinorInterpretableQ ->
			{b_?SpinorInterpretableQ, c_?SpinorInterpretableQ}
	] /; MemberQ[{a, b, c}, _?(!SpinorQ[#]&)] :=
		tag[x, SpinorizeInteger[a] -> SpinorizeIntegerList[b, c]];
)


Factory[DecomposeBSpinor, BSpinorProportionalQ, BSpinorReplace, Spbb]

Factory[DecomposeASpinor, ASpinorProportionalQ, ASpinorReplace, Spaa]


(*
	Factory functions where needed only for creating definitions of other
	objects, they're not needed after package initialization.
*)
Remove[Factory]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
