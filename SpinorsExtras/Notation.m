(* ::Package:: *)

BeginPackage["SpinorsExtras`Notation`", {"SpinorsExtras`"}]


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"]


Needs["SpinorsExtras`Utilities`"]
Needs["SpinorsExtras`Ref`"]
Needs["SpinorsExtras`Massive`"]
Needs["SpinorsExtras`Composite`"]
Needs["SpinorsExtras`Pol`"]


(* ::Subsection:: *)
(*Helper functions*)


(* ::Subsubsection:: *)
(*MakeBoxesPlusMinusOne*)


(* For +/-1 print just +/- for other expressions make usual boxes. *)

MakeBoxesPlusMinusOne[+1, _] := "+"

MakeBoxesPlusMinusOne[-1, _] := "-"

MakeBoxesPlusMinusOne[PlusMinus[1], _] := "\[PlusMinus]"

MakeBoxesPlusMinusOne[MinusPlus[1], _] := "\[MinusPlus]"

MakeBoxesPlusMinusOne[x_, form_] := MakeBoxes[x, form]


(* ::Subsubsection:: *)
(*SetNotation*)


SetNotation[rules__RuleDelayed] := (
	(
		MakeBoxes[expr:#1, form:(StandardForm | TraditionalForm)] :=
			With[
				{boxes = #2[form]}
				,
				InterpretationBox[boxes, expr]
			]
	)& @@@ {rules}
)


(* ::Subsection:: *)
(*SpRef*)


(* "q_P" notation for reference vectors. *)
SetNotation[SpRef[x_] :> (SubscriptBox["q", MakeBoxes[x, #]]&)]


(* ::Subsection:: *)
(*SpAssoc*)


SetNotation[
	(*
		"P^\[Flat]" notation for associated vectors with implicit reference
		vectors.
	*)
	SpAssoc[P_?LVectorQ] :> (SuperscriptBox[MakeBoxes[P, #], "\[Flat]"]&)
	,
	(*
		"P^q" notation for associated vectors with explicit reference vectors.
	*)
	SpAssoc[P_?LVectorQ, Q_?LVectorQ] :>
		(SuperscriptBox[MakeBoxes[P, #], MakeBoxes[Q, #]]&)
]


(* ::Subsection:: *)
(*SpM*)


SetNotation[
	(* "_uv P" notation for massive spinors with implicit reference vectors. *)
	SpM[P_?LVectorQ, uv_?PlusMinusOneQ] :> (
		RowBox[{
			SubscriptBox["", MakeBoxesPlusMinusOne[uv, #]], 
			MakeBoxes[P, #]
		}]&
	)
	,
	(*
		"^ref_uv P" notation for massive spinors with explicit reference
		vectors.
	*)
	SpM[P_?LVectorQ, uv_?PlusMinusOneQ, ref_?SpinorQ] :> (
		RowBox[{
			SubsuperscriptBox[
				"",
				MakeBoxesPlusMinusOne[uv, #], 
				MakeBoxes[ref, #]
			]
			, 
			MakeBoxes[P, #]
		}]&
	)
]


(* ::Subsection:: *)
(*LvBA*)


(* "\[Eta]_b,a" notation for composite vectors. *)
SetNotation[
	LvBA[b_?AnySpinorQ, a_?AnySpinorQ] :> (
		RowBox[{
			"\[Eta]",
			RowBox[{"(", MakeBoxes[Row[{b, a}, ","], #], ")"}]}
		]&
	)
]


(* ::Subsection:: *)
(*PolVec*)


SetNotation[
	(*
		"\[Epsilon]^pol(P) notation for polarization vectors with implicit
		reference vectors."
	*)
	PolVec[P_?LVectorQ, pol_?PossiblePolQ] :> (
		RowBox[{
			SuperscriptBox["\[Epsilon]", MakeBoxesPlusMinusOne[pol, #]], 
			RowBox[{"(", MakeBoxes[P, #], ")"}]
		}]&
	)
	,
	(*
		"\[Epsilon]^pol(P,q) notation for polarization vectors with explicit
		reference vectors."
	*)
	PolVec[P_?LVectorQ, pol_?PossiblePolQ, ref_?SpinorQ] :> (
		RowBox[{
			SuperscriptBox["\[Epsilon]", MakeBoxesPlusMinusOne[pol, #]], 
			RowBox[{"(", MakeBoxes[Row[{P, ref}, ","], #], ")"}]
		}]&
	)
]


End[]


EndPackage[]
