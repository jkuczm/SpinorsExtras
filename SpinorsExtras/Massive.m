(* ::Package:: *)

BeginPackage["SpinorsExtras`Massive`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


MassiveSpinorQ::usage =
"\
MassiveSpinorQ[x] \
returns True if x is interpretable as massive spinor label. Returns False \
otherwise."


AnySpinorQ::usage =
"\
AnySpinorQ[x] \
returns True if x is interpretable as massless or massive spinor label. \
Returns False otherwise."


SpM::usage =
"\
SpM[P, +1] \
represents u spinor associated with non-lightlike four-vector P and implicit \
reference spinor.\

SpM[P, -1] \
represents v spinor associated with non-lightlike four-vector P and implicit \
reference spinor.\

SpM[P, +1, ref] \
represents u spinor associated with non-lightlike four-vector P and reference \
spinor ref.\

SpM[P, -1, ref] \
represents v spinor associated with non-lightlike four-vector P and reference \
spinor ref."


SpAssoc::usage =
"\
SpAssoc[P] \
represents lightlike vector associated with four-vector P through light cone \
decomposition with default reference vector.\

SpAssoc[P, ref] \
represents lightlike vector associated with four-vector P through light cone \
decomposition with (massless or massive) reference vector ref."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]
Needs["SpinorsExtras`Utilities`"]
Needs["SpinorsExtras`Ref`"]


PrependTo[$ContextPath, "Spinors`Private`"]


(* ::Subsection:: *)
(*MassiveSpinorQ*)


MassiveSpinorQ[x_] :=
	MatchQ[
		x,
		HoldPattern @ SpM[
			_?LVectorQ,
			_?PlusMinusOneQ,
			Repeated[_?SpinorQ, {0, 1}]
		]
	]


(* ::Subsection:: *)
(*ScaledMassiveSpinorQ*)


ScaledQFactory["MassiveSpinor", _?MassiveSpinorQ]


(* ::Subsection:: *)
(*AnySpinorQ*)


AnySpinorQ[x_] := SpinorQ[x] || MassiveSpinorQ[x]


(* ::Subsection:: *)
(*ScaledAnySpinorQ*)


ScaledQFactory["AnySpinor", _?AnySpinorQ]


(* ::Subsection:: *)
(*SpAssoc*)


QuietSpinorPrint[
	DeclareSpinor[
		HoldPattern @ SpAssoc[_?LVectorQ, Repeated[_?LVectorQ, {0, 1}]]
	];
]


SpAssoc[a_?SpinorInterpretableQ, Repeated[_?LVectorInterpretableQ, {0, 1}]] :=
	SpinorizeInteger[a]


SpAssoc[P_?LVectorInterpretableQ, i_Integer] := SpAssoc[P, Sp[i]]


(* ::Subsection:: *)
(*SpM*)


SpM[
	i_?SpinorInterpretableQ,
	_?PlusMinusOneQ,
	Repeated[_?SpinorInterpretableQ, {0, 1}]
] :=
	SpinorizeInteger[i]


SpM[P_?LVectorQ, t_?PlusMinusOneQ, i_Integer] := SpM[P, t, Sp[i]]


(* ::Subsection:: *)
(*DeclareSpinorMomentum[SpAssoc[...]]*)


x4VAssociated[x4V_, P_, ref_] := x4VAssociated[x4V[P], x4V[ref]]

x4VAssociated[P:{_, _, _, _}, ref:{_, _, _, _}] :=
	P - MP2[P] / (2 MP[P, ref]) ref


DeclareSpinorMomentum[spAssoc:SpAssoc[P_?LVectorQ]] ^:=
	If[MatchQ[Num4V[P], {_, _, _, _}],
		If[!MatchQ[Num4V[SpRef[P]], {_, _, _, _}],
			DeclareSpinorMomentum[SpRef[P]];
		];
		DeclareSpinorMomentum[
			spAssoc,
			x4VAssociated[Num4V, P, SpRef[P]] // N
		]
	(* else *),
		Message[DeclareSpinorMomentum::firstDeclare, spAssoc, {P}];
		$Failed
	]


DeclareSpinorMomentum[spAssoc:SpAssoc[P_?LVectorQ, ref_?LVectorQ]] ^:= (
	With[
		{notDeclared = Select[{P, ref}, !MatchQ[Num4V[#], {_, _, _, _}]&]}
		,
		If[notDeclared =!= {},
			Message[DeclareSpinorMomentum::firstDeclare, spAssoc, notDeclared];
			Return[$Failed]
		]
	];
	
	If[SpinorQ[ref],
		Return[
			DeclareSpinorMomentum[spAssoc, x4VAssociated[Num4V, P, ref] // N]
		]
	];
	
	Module[
		{
			mP2 = MP2[Num4V[P]] // N,
			mRef2 = MP2[Num4V[ref]] // N,
			PRef = MP[Num4V[P], Num4V[ref]] // N,
			sqrtDelta
		}
		,
		(*
			Exp[I Pi Floor[1/2 - Arg[PRef]/Pi]] ensures correct sign for both
			real and complex PRef.
		*)
		sqrtDelta =
			Exp[I Pi Floor[1/2 - Arg[PRef]/Pi]] Sqrt[PRef^2 - mP2 mRef2];
		
		DeclareSpinorMomentum[
			spAssoc,
			((sqrtDelta + PRef) Num4V[P] - mP2 Num4V[ref]) / (2 sqrtDelta)
		]
	]
)


(* ::Subsection:: *)
(*Automatic simplifications of spinor chains*)


Unprotect[Spaa, Spbb, Spab, Spba]


(*
	Don't use syntax correction, treat A and B spinors as Dirac spinors and
	return zeros.
	
	Condition is used at the end of definition instead of place right after
	p___ to override coresponding definitions from Spinors`.
*)
Spaa[_?SpinorQ, p___, _?SpinorQ] := 0 /; OddQ[NbrDM[p]]
Spbb[_?SpinorQ, p___, _?SpinorQ] := 0 /; OddQ[NbrDM[p]]
Spab[_?SpinorQ, p___, _?SpinorQ] := 0 /; EvenQ[NbrDM[p]]
Spba[_?SpinorQ, p___, _?SpinorQ] := 0 /; EvenQ[NbrDM[p]]


(* Pull out coefficients *)
Spaa[coeff__?ScalarQ a_?AnySpinorQ, p__] := coeff Spaa[a, p]
Spbb[coeff__?ScalarQ a_?AnySpinorQ, p__] := coeff Spbb[a, p]
Spab[coeff__?ScalarQ a_?AnySpinorQ, p__] := coeff Spab[a, p]
Spba[coeff__?ScalarQ a_?AnySpinorQ, p__] := coeff Spba[a, p]

Spaa[p__, coeff__?ScalarQ a_?AnySpinorQ] := coeff Spaa[p, a]
Spbb[p__, coeff__?ScalarQ a_?AnySpinorQ] := coeff Spbb[p, a]
Spab[p__, coeff__?ScalarQ a_?AnySpinorQ] := coeff Spab[p, a]
Spba[p__, coeff__?ScalarQ a_?AnySpinorQ] := coeff Spba[p, a]


(*
	[x|SM1|...|SMn|y] = -[y|SMn|...|SM1|x]
	those products are antisymmetric, so we implement default ordering.
*)

Spaa[
	a_?AnySpinorQ,
	p___?SMatrixQ /; ! OrderedQ[{{p}, Reverse[{p}]}],
	a_?AnySpinorQ
] :=
	-Spaa[a, Sequence @@ Reverse[{p}], a]

Spaa[a_?AnySpinorQ, p___?SMatrixQ, b_?AnySpinorQ] /;
	! OrderedQ[{a, b}] && UnsameQ[a, b] :=
		-Spaa[b, Sequence @@ Reverse[{p}], a]


(*
	<x|SM1|...|SMn|y> = -<y|SMn|...|SM1|x>
	those products are antisymmetric, so we implement default ordering.
*)

Spbb[
	a_?AnySpinorQ,
	p___?SMatrixQ /; ! OrderedQ[{Reverse[{p}], {p}}],
	a_?AnySpinorQ
] :=
	-Spbb[a, Sequence @@ Reverse[{p}], a]

Spbb[a_?AnySpinorQ, p___?SMatrixQ, b_?AnySpinorQ] /;
	OrderedQ[{a, b}] && UnsameQ[a, b] :=
		-Spbb[b, Sequence @@ Reverse[{p}], a]


(*
	[x|SM1|...|SMn|y> = <y|SMn|...|SM1|x]
	those products are symmetric, so we implement default ordering.
*)
Spba[a_?AnySpinorQ, p___?SMatrixQ, b_?AnySpinorQ] :=
	Spab[b, Sequence @@ Reverse[{p}], a]


Protect[Spaa, Spbb, Spab, Spba]



(* Zero spinor prodcts of massive spinors. *)
SpM /: (Spaa | Spbb)[SpM[P_, _], SpM[P_, _]] = 0
SpM /: (Spaa | Spbb)[SpM[P_, _, q_?SpinorQ], SpM[P_, _, q_]] = 0

SpM /: (Spab | Spba)[SpM[_, _, q_], q_?SpinorQ] = 0
SpM /: (Spab | Spba)[q_?SpinorQ, SpM[_, _, q_]] = 0
SpM /:
	(Spab | Spba)[SpM[P_, _, SpAssoc[Q_, P_]], SpM[Q_, _, SpAssoc[P_, Q_]]] = 0


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
