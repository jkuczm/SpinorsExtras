(* ::Package:: *)

BeginPackage["SpinorsExtras`MassiveUtilities`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


ExpandMPToSpinors::usage =
"\
ExpandMPToSpinors[expr, a] \
returns expr with Minkowski products (MP) containing label a replaced by \
spinor products.\

ExpandMPToSpinors[expr, patt] \
returns expr with Minkowski products (MP) containing labels matching patt \
replaced by spinor products.\

ExpandMPToSpinors[expr] \
returns expr with all Minkowski products (MP) replaced by spinor products."


ExpandSToMPs::usage =
"\
ExpandSToMPs[expr] \
returns expr with invariants s[P, Q, ...] replaced by Minkowski products (MP)."


LightConeDecompose::usage =
"\
LightConeDecompose[expr, P] \
performs light cone decomposition of Lorentz vectors and massive spinors in \
expr with label P. Lorentz vectors are decomposed with default reference \
vector and massive spinors with their reference vectors.\

LightConeDecompose[expr, patt] \
performs light cone decomposition of those Lorentz vectors and massive \
spinors in expr which label matches pattern patt. Lorentz vectors are \
decomposed with default reference vector and massive spinors with their \
reference vectors.\

LightConeDecompose[expr, P -> q] \
with q being massless spinor. Decomposes Lorentz vectors and massive spinors, \
in expr, with label P. Lorentz vectors and spinors with implicit reference \
vectors are decomposed with reference vector q and massive spinors with \
explicit reference vector are decomposed with their reference vectors.\

LightConeDecompose[expr, P -> Q] \
with Q being massive LVector. Decomposition where Lorentz vectors and spinors \
with implicit reference vectors are decomposed with reference vector being \
vector associated with Q coming from simultaneous decomposition with P. \
Massive spinors with explicit reference vectors are decomposed with their \
reference vectors.\

LightConeDecompose[expr, patt -> x] \
decomposes all Lorentz vectors and massive spinors of which label matches \
pattern patt. Lorentz vectors and spinors with implicit reference vectors are \
decomposed with reference vector inferred from x and massive spinors with \
explicit reference vectors are decomposed with their reference vectors.\

LightConeDecompose[expr, patt :> replacement] \
Decomposition with reference vector inferred from replacement. patt can have \
named patterns inside and replacement can use variables matched by those \
named patterns.\

LightConeDecompose[expr, {p1 -> x1, p2, p3 -> x3, ...}] \
performs decompositions using all given patterns and rules.\

LightConeDecompose[expr, {{p11 -> x11, p12, ...}, {p21 -> x21, ...}, ...}] \
returns a List, each element of this list is a result of decomposition using \
subsequent list of patterns and rules.\

LightConeDecompose[expr] Decomposes Lorentz vectors and massive spinors with \
all labels. Lorentz vectors are decomposed with default reference vector and \
massive spinors with their reference vectors."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]
Needs["SpinorsExtras`Utilities`"]
Needs["SpinorsExtras`Ref`"]
Needs["SpinorsExtras`Massive`"]


(* ::Subsection:: *)
(*ExpandMPToSpinors*)


Options[ExpandMPToSpinors] = {
	"Massive" -> True,
	"OpenMassiveSpinorChains" -> False,
	"UOrVFunction" -> (1&)
}


ExpandMPToSpinors::massiveFalse =
"ExpandMPToSpinors called with massive LVector `1` as second argument, but \
option \"Massive\" is set to `2` so nothing is expanded in `3`."


ExpandMPToSpinors[expr_, opt:OptionsPattern[]] :=
	ExpandMPToSpinors[expr, _, opt]


ExpandMPToSpinors[expr_, P_?MassiveLVectorQ, opt:OptionsPattern[]] := (
	Message[
		ExpandMPToSpinors::massiveFalse,
		P,
		OptionValue["Massive"],
		HoldForm[ExpandMPToSpinors[expr, P, opt]]
	];
	expr
) /; ! TrueQ[OptionValue["Massive"]]


ExpandMPToSpinors[expr_, i_Integer, opt:OptionsPattern[]] :=
	ExpandMPToSpinors[expr, Sp[i], opt]


ExpandMPToSpinors[expr_, patt_, OptionsPattern[]] :=
	Module[
		{
			rules = {
				MP[a:patt?SpinorQ, b:patt?SpinorQ] :> 1/2 Spaa[a, b] Spbb[b, a]
				,
				MP[a:patt?SpinorQ, P_] :> 1/2 Spab[a, P, a]
			}
		}
		,
		If[OptionValue["Massive"],
			If[OptionValue["OpenMassiveSpinorChains"],
				rules =
					Insert[
						rules
						,
						MP[K:patt?LVectorQ, P:patt?LVectorQ] /; K =!= P :>
							Module[
								{
									sgnK = OptionValue["UOrVFunction"][K],
									sgnP = OptionValue["UOrVFunction"][P],
									spMK,
									spMP
								}
								,
								spMK = SpM[K, sgnK];
								spMP = SpM[P, sgnP];
								
								1/2 Spaa[spMK, spMP] Spbb[spMP, spMK]
								+ 1/2 Spab[spMK, spMP] Spab[spMP, spMK]
								- 1/4 Spab[spMK, spMK] Spab[spMP, spMP]
							]
						,
						2
					]
			];
			
			AppendTo[
				rules
				,
				MP[K:patt?LVectorQ, P_] /; K =!= P :>
					With[
						{spM = SpM[K, OptionValue["UOrVFunction"][K]]}
						,
						1/2 Spab[spM, P, spM]
					]
			];
		];
		
		expr /. rules
	]


(* ::Subsection:: *)
(*ExpandSTo (Private function)*)


ExpandSTo[x_, pairConversion_, massive_] :=
	Module[
		{result}
		,
		
		result = x //. HoldPattern[s[i__?ScaledSpinorQ]] :>
			Plus @@ (
				pairConversion @@@ Union[Sort /@ Permutations[{i}, {2}]]
			);
			
		If[massive,
			result = result //. HoldPattern[s[P__?ScaledLVectorQ]] :>
				Plus @@ (
					pairConversion @@@ Union[Sort /@ Permutations[{P}, {2}]]
				)
				+ Plus @@ (MP2 /@ {P})
		];
		
		Return[result]
	]


(* ::Subsection:: *)
(*ExpandSToMPs*)


Options[ExpandSToMPs] = {
	"Massive" -> True
}


ExpandSToMPs[x_, OptionsPattern[]] :=
	ExpandSTo[x, 2 MP[#1, #2]&, OptionValue["Massive"]]


(* ::Subsection:: *)
(*ExpandSToSpinors*)


SpabFromPair[x__?ScalarQ P_?LVectorQ, Q_?ScaledLVectorQ, uOrVFunction_] :=
	x SpabFromPair[P, Q, uOrVFunction]

SpabFromPair[P_?LVectorQ, x__?ScalarQ Q_?ScaledLVectorQ, uOrVFunction_] :=
	x SpabFromPair[P, Q, uOrVFunction]

SpabFromPair[a_?SpinorQ, b_?SpinorQ, _] := Spaa[a, b] Spbb[b, a]

SpabFromPair[a_?SpinorQ, P_?LVectorQ, _] := Spab[a, P, a]

SpabFromPair[P_?LVectorQ, a_?SpinorQ, _] := Spab[a, P, a]

SpabFromPair[P_?LVectorQ, Q_?LVectorQ, uOrVFunction_] :=
	Module[
		{spM = SpM[P, uOrVFunction[P]]}
		,
		Spab[spM, Q, spM]
	]


Unprotect[ExpandSToSpinors];


(* Remove original definition. *)
Quiet[
	ExpandSToSpinors[x_] =.;,
	{Unset::norep}
];


(* Add support for massive spinors. *)
Options[ExpandSToSpinors] = {
	"Massive" -> True,
	"UOrVFunction" -> (1&)
}


ExpandSToSpinors[x_, OptionsPattern[]] :=
	ExpandSTo[
		x,
		SpabFromPair[#1, #2, OptionValue["UOrVFunction"]]&,
		OptionValue["Massive"]
	]


Protect[ExpandSToSpinors];


(* ::Subsection:: *)
(*LightConeDecompose*)


MasslesRefFromRef[ref_?SpinorQ, _] := ref

(*
	For massive refenece vector,
	use associated vector coming from simultaneous decomposition with P.
*)
MasslesRefFromRef[ref_, P_] := SpAssoc[ref, P]


LightConeDecomposeLVectorExpr[P_, as_, ref_] :=
	as + MP2[P] / (2 MP[as, ref]) ref


LightConeDecomposeLVectorRule[((Rule | RuleDelayed)[patt_, givenRef_])] :=
	P:patt :> LightConeDecomposeLVectorExpr[
		P,
		SpAssoc[P, givenRef],
		MasslesRefFromRef[givenRef, P]
	]

LightConeDecomposeLVectorRule[patt_] :=
	P:patt :> LightConeDecomposeLVectorExpr[P, SpAssoc[P], SpRef[P]]


LightConeDecomposeSpinorExpr[P_, t_, as_, ref_, Spxx_] :=
	{as, t Sqrt[MP2[P]] / Spxx[as, ref] ref}


LightConeDecomposeSpinorRuleListForcedRef[
	((Rule | RuleDelayed)[patt_, givenRef_]),
	Spxx_
] := {
	SpM[P:patt, t_?PlusMinusOneQ, Repeated[_, {0, 1}]] :> 
		LightConeDecomposeSpinorExpr[
			P,
			t,
			SpAssoc[P, givenRef],
			MasslesRefFromRef[givenRef, P],
			Spxx
		]
}

LightConeDecomposeSpinorRuleListForcedRef[patt_, Spxx_] := {
	SpM[P:patt, t_?PlusMinusOneQ, Repeated[_, {0, 1}]] :> 
		LightConeDecomposeSpinorExpr[
			P,
			t,
			SpAssoc[P],
			SpRef[P],
			Spxx
		]
}


LightConeDecomposeSpinorRuleListOwnRef[
	((Rule | RuleDelayed)[patt_, givenRef_]),
	Spxx_
] := {
	SpM[P:patt, t_?PlusMinusOneQ] :> LightConeDecomposeSpinorExpr[
		P,
		t,
		SpAssoc[P, givenRef],
		MasslesRefFromRef[givenRef, P],
		Spxx
	]
	,
	SpM[P:patt, t_?PlusMinusOneQ, ref_] :>
		LightConeDecomposeSpinorExpr[P, t, SpAssoc[P, ref], ref, Spxx]
}

LightConeDecomposeSpinorRuleListOwnRef[patt_, Spxx_] := {
	SpM[P:patt, t_?PlusMinusOneQ] :>
		LightConeDecomposeSpinorExpr[P, t, SpAssoc[P], SpRef[P], Spxx]
	,
	SpM[P:patt, t_?PlusMinusOneQ, ref_] :>
		LightConeDecomposeSpinorExpr[P, t, SpAssoc[P, ref], ref, Spxx]
}


Options[LightConeDecompose] = {
	"ForceRefChange" -> False
}

LightConeDecompose[expr_, listOfLists:{_List..}, opt:OptionsPattern[]] :=
	LightConeDecompose[expr, #, opt]& /@ listOfLists;

LightConeDecompose[expr_, pattOrRuleList:{__}, OptionsPattern[]] :=
	Module[
		{
			result = expr,
			spinorRuleList =
				If[OptionValue["ForceRefChange"],
					LightConeDecomposeSpinorRuleListForcedRef
				(* else *),
					LightConeDecomposeSpinorRuleListOwnRef
				]
		}
		,
		
		result = ReplaceLVector[
			result,
			LightConeDecomposeLVectorRule /@ pattOrRuleList
		];
		result = ReplaceBSpinor[
			result,
			Flatten[spinorRuleList[#, Spaa]& /@ pattOrRuleList]
		];
		result = ReplaceASpinor[
			result,
			Flatten[spinorRuleList[#, Spbb]& /@ pattOrRuleList]
		];
		
		Return[result]
	]

LightConeDecompose[expr_, pattOrRule_, opt:OptionsPattern[]] :=
	LightConeDecompose[expr, {pattOrRule}, opt];

LightConeDecompose[expr_, opt:OptionsPattern[]] :=
	LightConeDecompose[expr, {_?LVectorQ}, opt];


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
