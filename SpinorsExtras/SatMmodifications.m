(* ::Package:: *)

BeginPackage["SpinorsExtras`SatMmodifications`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


(* Redefine usage message of Spinors`SpOpen *)
SpOpen::usage =
"\
SpOpen[x] \
decomposes spinor products in x containing slashed matrices into products of \
smaller spinor products, by applying: k-slashed = |k>[k| + |k]<k|.\

SpOpen[x, a] \
does the same selectively, at the occurence of a-slashed.\

SpOpen[x, patt] \
does the same selectively, at the occurence of any massless Spinor that \
matches pattern patt."


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"]


Needs["SpinorsExtras`Package`"]
Needs["SpinorsExtras`Utilities`"]
Needs["SpinorsExtras`Massive`"]
Needs["SpinorsExtras`Composite`"]
Needs["SpinorsExtras`Proportional`"]


PrependTo[$ContextPath, "Spinors`Private`"] (* gg, OnShellQ, SpinProd *)


(* ::Subsection:: *)
(*Dot*)


Unprotect[Dot]


(* Add _SmBA2 and _CSmBA2 to patterns for linearity of Dot. *)
Dot[
	la1___
	,
	coeff__ * 
		la2:(
			(BSpHeadPattern | ASpHeadPattern)[_?SpinorQ] |
				_?SMatrixQ |_Dot | _Sm2 |_CSm2 |_SmBA2 |_CSmBA2 |_Sm4 |_SmBA4
		)
	,
	la3___
] :=
	coeff (la1.la2.la3)


(* Extend Dot simplifications to include e.g. ...CSmBA2[a, b].SmBA2[c, d]... *)
Dot[
	aa___,
	lat:(Lat[_?SpinorQ]|CSm2[_?SMatrixQ]|CSmBA2[_?SpinorQ, _?SpinorQ]),
	SmBA2[B_?SpinorQ, A_?SpinorQ],
	la:(La[_?SpinorQ]|CSm2[_?SMatrixQ]|CSmBA2[_?SpinorQ, _?SpinorQ]),
	bb___
] :=
	Dot[aa, lat, CLat[B]] Dot[CLa[A], la, bb]

Dot[
	aa___,
	la:(La[_?SpinorQ]|CSm2[_?SMatrixQ]|CSmBA2[_?SpinorQ, _?SpinorQ]),
	SmBA2[B_?SpinorQ,A_?SpinorQ],
	lat:(Lat[_?SpinorQ]|CSm2[_?SMatrixQ]|CSmBA2[_?SpinorQ, _?SpinorQ]),
	bb___
] :=
	Dot[aa, la, CLa[A]] Dot[CLat[B], lat, bb]

Dot[
	aa___,
	Cla:(CLa[_?SpinorQ]|Sm2[_?SMatrixQ]|SmBA2[_?SpinorQ, _?SpinorQ]),
	CSmBA2[B_?SpinorQ, A_?SpinorQ],
	Clat:(CLat[_?SpinorQ]|Sm2[_?SMatrixQ]|SmBA2[_?SpinorQ, _?SpinorQ]),
	bb___
] :=
	Dot[aa, Cla, La[A]] Dot[Lat[B], Clat, bb]

Dot[
	aa___,
	Clat:(CLat[_?SpinorQ]|Sm2[_?SMatrixQ]|SmBA2[_?SpinorQ, _?SpinorQ]),
	CSmBA2[B_?SpinorQ, A_?SpinorQ],
	Cla:(CLa[_?SpinorQ]|Sm2[_?SMatrixQ]|SmBA2[_?SpinorQ, _?SpinorQ]),
	bb___
] :=
	Dot[aa, Clat, Lat[B]] Dot[La[A], Cla, bb]


Protect[Dot]


(* ::Subsection:: *)
(*DeclareSpinorMomentum*)


Unprotect[DeclareSpinorMomentum]

(*
	Add changing of integer i to Sp[i] in DeclareSpinorMomentum with two
	spinors, it was missing and resulted in declaring "raw" integers as
	spinors.
*)
DeclareSpinorMomentum[
	i_Integer, la:{{_?NumericQ},{_?NumericQ}},lat:{{_?NumericQ,_?NumericQ}}
] := DeclareSpinorMomentum[Sp[i], la, lat]

Protect[DeclareSpinorMomentum]


(* ::Subsection:: *)
(*s*)


Unprotect[s]

(* Add handling of sums inside s *)
s[
	a___,
	HoldPattern[Plus[x:Repeated[_?ScaledLVectorQ, {2, Infinity}]]],
	b___
] :=
	s[a, x, b]

(* Add handling of multiplicated args *)
(* This simplification is to slow especially the ScalarQ test. *)
s[
	a___
	,
	repeated:Longest[
		Repeated[P_?LVectorQ | (__(*?ScalarQ*) P_?LVectorQ), {2, Infinity}]
	]
	,
	b___
] :=
	s[a, Total[GetScalarCoefficient /@ {repeated}] P, b]

(* Add handling of one arg *)
s[a_] := MP2[a]

(* Add numerics for s with elements multiplied by something. *)
N[
	HoldPattern[
		s[
			a:(
				(
					_?NumSpinorQ
					| _?NumVectorQ
					| Times[__, _?NumSpinorQ]
					| Times[__, _?NumVectorQ]
				)..
			)
		]
	]
	,
	p_:$MachinePrecision
] :=
	N[(Plus @@ (Num4V[#]& /@ {a})).gg.(Plus @@ (Num4V[#]& /@ {a})), p]

Protect[s]


(* ::Subsection:: *)
(*SpOpen*)


Unprotect[SpOpen]

(* Clear old definition. *)
Clear[SpOpen]

(*
	Add possibility of giving any pattern instead of just single Spinor.
	
	Add possibility of opening spinor chains with massive spinors on ends.
	Without it such spinor chains remain unchanged,
	which leads to infinite recursion
	e.g. when A(B)SpinorReplace or A(B)SpinorShift is used on such chain
	(due to first replacement rule in thouse functions
	that calls function on result of SpOpen).
*)

Options[SpOpen] = {
	"BothEndsMassive" -> False
}

SpOpen[expr_, opt:OptionsPattern[]] := SpOpen[expr, _, opt]

SpOpen[expr_, i_Integer, opt:OptionsPattern[]] := SpOpen[expr, Sp[i], opt]

SpOpen[expr_, patt_, OptionsPattern[]] := Module[
	{
		rules = {
			(spxy:SpinProd)[
				b_,
				before___,
				x:patt /; SMatrixQ[x] && OnShellQ[x],
				after___?SMatrixQ,
				a_?SpinorQ
			] :>
				Switch[Length[{after}],
					_?EvenQ, ChangeSecond[spxy],
					_?OddQ, spxy
				][b, before, x] FixSecond[spxy][x, after, a]
			,
			(spxy:SpinProd)[
				b_?SpinorQ,
					before___?SMatrixQ,
				x:patt /; SMatrixQ[x] && OnShellQ[x],
				after___,
				a_
			] :>
				Switch[Length[{before}],
					_?EvenQ, ChangeFirst[spxy],
					_?OddQ, spxy
				][x, after, a] FixFirst[spxy][b, before, x]
			,
			Dot[a___, Sm2[x:patt?OnShellQ], b___] :>
				Dot[a, CLat[x]] Dot[CLa[x], b],
			Dot[a___, CSm2[x:patt?OnShellQ], b___] :>
				Dot[a, La[x]] Dot[Lat[x], b]
		}
	}
	,
	
	If[OptionValue["BothEndsMassive"],
		AppendTo[
			rules
			,
			(spxy:SpinProd)[
				before__,
				x:patt /; SMatrixQ[x] && OnShellQ[x],
				after__
			] :>
				ChangeSecondToA[spxy][before, x] ChangeFirstToB[spxy][x, after]
				+
				ChangeSecondToB[spxy][before, x] ChangeFirstToA[spxy][x, after]
		];
	];
	
	expr //. rules //. {
		Dot[aa___, Sm4[x:patt?OnShellQ], USpa[a_?SpinorQ]] :>
			Dot[aa, USpb[x]] Dot[UbarSpa[x], USpa[a]]
		,
		Dot[aa___, Sm4[x:patt?OnShellQ], USpb[a_?SpinorQ]] :>
			Dot[aa, USpa[x]] Dot[UbarSpb[x], USpb[a]]
		, 
		Dot[UbarSpb[a_?SpinorQ], Sm4[x:patt?OnShellQ], b___] :>
			Dot[UbarSpb[a], USpb[x]] Dot[UbarSpa[x], b]
		, 
		Dot[UbarSpa[a_?SpinorQ], Sm4[x:patt?OnShellQ], b___] :>
			Dot[UbarSpa[a], USpa[x]] Dot[UbarSpb[x], b]
		, 
		Dot[UbarSpa[a_?SpinorQ], sm__Sm4, Sm4[x:patt?OnShellQ], b___] :>
			Switch[Length[{sm}],
				_?OddQ,
					Dot[UbarSpa[a], sm, USpb[x]] Dot[UbarSpa[x], b],
				_?EvenQ,
					Dot[UbarSpa[a], sm, USpa[x]] Dot[UbarSpb[x], b]
			]
		,
		Dot[UbarSpb[a_?SpinorQ], sm__Sm4, Sm4[x:patt?OnShellQ], b___] :>
			Switch[Length[{sm}],
				_?OddQ,
					Dot[UbarSpb[a], sm, USpa[x]] Dot[UbarSpb[x], b],
				_?EvenQ,
					Dot[UbarSpb[a], sm, USpb[x]] Dot[UbarSpa[x], b]
			]
	}
]

Protect[SpOpen]


(* ::Subsection:: *)
(*BSpinorReplace*)


Unprotect[BSpinorReplace]

BSpinorReplace[x_, ss_?SpinorQ, z_] :=
	SpOpen[x, ss, "BothEndsMassive" -> True] /. {
		Spbb[ss, ms__, ss] :> Spbb[z, ms, z],
		(spby:(Spbb|Spba))[ss, a__] :> spby[z, a],
		(spxb:(Spbb|Spab))[b__, ss] :> spxb[b, z]
		,
		(SpB:BSpHeadPattern)[ss] :> SpB[z]
		,
		s[b___, ss, a___] :> Plus @@ (Spba[z, #, ss]& /@ {b, a}) + s[a, b]
		,
		MP[ss, b_] | MP[b_, ss] :> 1/2 Spab[ss, b, z]
		,
		(sm:SmHeadPattern)[ss] :> ChangeSmToSmBA[sm][z, ss]
		,
		(sm:SmBAHeadPattern)[ss, a_] :> sm[z, a]
	}

Protect[BSpinorReplace]


(* ::Subsection:: *)
(*ASpinorReplace*)


Unprotect[ASpinorReplace]

ASpinorReplace[x_, ss_?SpinorQ, z_] :=
	SpOpen[x, ss, "BothEndsMassive" -> True] /. {
		Spaa[ss, ms__, ss] :> Spaa[z, ms, z],
		(spay:(Spaa|Spab))[ss, a__] :> spay[z, a],
		(spxa:(Spaa|Spba))[b__, ss] :> spxa[b, z]
		,
		(SpA:ASpHeadPattern)[ss] :> SpA[z]
		,
		s[b___, ss, a___] :> Plus @@ (Spab[z, #, ss]& /@ {b, a}) + s[a, b]
		,
		MP[ss, b_] | MP[b_, ss] :> 1/2 Spab[z, b, ss]
		,
		(sm:SmHeadPattern)[ss] :> ChangeSmToSmBA[sm][ss, z]
		,
		(sm:SmBAHeadPattern)[b_, ss] :> sm[b, z]
	}

Protect[ASpinorReplace]


(* ::Subsection:: *)
(*BSpinorShift*)


(*Unprotect[BSpinorShift]

(* Add possibility of shifting massive spinors. *)
BSpinorShift[x_, P_?MassiveLVectorQ, z_] := x /. {
	(spxy:SpinProd)[b__, P, a__] :>
		BSpinorShift[SpOpen[spxy[b, P, a], P], P, z],
	Spbb[P, a__, P] :> Spbb[P + z, a, P + z],
	(spby:(Spbb|Spba))[P, a__] :> spby[P + z, a],
	(spxb:(Spbb|Spab))[b__, P] :> spxb[b, P + z],
	
	Lat[P] :> Lat[P] + Lat[z],
	CLat[P] :> CLat[P] + CLat[z],
	USpb[P] :> USpb[P] + USpb[z],
	UbarSpb[P] :> UbarSpb[P] + UbarSpb[z],
	
	MP2[P] :> MP2[P] + 1/2 Spab[P, z] Spab[P, P] + 1/4 Spab[P, z]^2,
	MP[P, b_]|MP[b_, P] :> MP[P, b] + 1/2 Spba[z, b, P],
	s[b___, P, a___] :>
		s[b, P, a]
		+ Plus @@ (Spba[z, #, P]& /@ {b, a})
		+ 1/2 Spab[P, z] Spab[P, P]
		+ 1/4 Spab[P, z]^2,
	
	SmBA[P, a_] :>  SmBA[P, a] + SmBA[z, a],
	SmBA2[P, a_] :>  SmBA2[P, a] + SmBA2[z, a],
	CSmBA2[P, a_] :>  CSmBA2[P, a] + CSmBA2[z, a],
	SmBA4[P, a_] :>  SmBA4[P, a] + SmBA4[z, a],
	
	Sm[P] :> Sm[P] + SmBA[z, P] - 1/2 Spba[z, P],
	(* TODO: Change Spba[P, z] to equivalent in 2-, 4-dim representation,
		when it's implemented. *)
	Sm2[P] -> Sm2[P] + CLat[z].CLa[P] - 1/2 Spba[z, P],
	CSm2[P] -> CSm2[P] + La[P].Lat[z] - 1/2 Spba[z, P],
	Sm4[P] -> Sm4[P] + SmBA4[z, P] - 1/2 Spba[z, P]
}

Protect[BSpinorShift]


(* ::Subsection:: *)
(*ASpinorShift*)


Unprotect[ASpinorShift]

(* Add possibility of shifting massive spinors. *)
ASpinorShift[x_, P_?MassiveLVectorQ, z_] := x /. {
	(spxy:SpinProd)[b__, P, a__] :>
		ASpinorShift[SpOpen[spxy[b, P, a], P], P, z],
	Spaa[P, a__, P] :> Spaa[P + z, a, P + z],
	(spay:(Spaa|Spab))[P, a__] :> spay[P + z, a],
	(spxa:(Spaa|Spba))[b__, P] :> spxa[b, P + z],
	
	La[P] :> La[P] + La[z],
	CLa[P] :> CLa[P] + CLa[z],
	USpa[P] :> USpa[P] + USpa[z],
	UbarSpa[P] :> UbarSpa[P] + UbarSpa[z],
	
	MP2[P] :> MP2[P] + 1/2 Spab[z, P] Spab[P, P] + 1/4 Spab[z, P]^2,
	MP[P, b_]|MP[b_, P] :> MP[b, P] + 1/2 Spab[z, b, P],
	s[b___, P, a___] :>
		s[b, P, a]
		+ Plus @@ (Spab[z, #, P]& /@ {b, a})
		+ 1/2 Spab[z, P] Spab[P, P]
		+ 1/4 Spab[z, P]^2,
	
	SmBA[b_, P] :> SmBA[b, P] + SmBA[b, z],
	SmBA2[b_, P] :> SmBA2[b, P] + SmBA2[b, z],
	CSmBA2[b_, P] :> CSmBA2[b, P] + CSmBA2[b, z],
	SmBA4[b_, P] :> SmBA4[b, P] + SmBA4[b, z],
	
	Sm[P] -> Sm[P] + SmBA[P, z] - 1/2 Spba[P, z],
	(* TODO: Change Spba[P, z] to equivalent in 2-, 4-dim representation,
		when it's implemented. *)
	Sm2[P] -> Sm2[P] + CLat[P].CLa[z] - 1/2 Spba[P, z],
	CSm2[P] -> CSm2[P] + La[z].Lat[P] - 1/2 Spba[P, z],
	Sm4[P] -> Sm4[P] + SmBA4[P, z] - 1/2 Spba[P, z]
}

Protect[ASpinorShift]*)


(* ::Subsection:: *)
(*ShiftBA*)


Unprotect[ShiftBA]

(* Add possibility of shifts for massive spinors. *)

(* TODO: Use separate function for "default" massive shift? *)
(*ShiftBA[b_?MassiveLVectorQ, a_?SpinorInterpretableQ, z_] :=
	ShiftBA[{b, a}, {a, SpAssoc[b, a]}, z]
	
ShiftBA[b_?SpinorInterpretableQ, a_?MassiveLVectorQ, z_] :=
	ShiftBA[{b, SpAssoc[a, b]}, {a, b}, z]
	
ShiftBA[b_?MassiveLVectorQ, a_?MassiveLVectorQ, z_] :=
	ShiftBA[{b, SpAssoc[a, b]}, {a, SpAssoc[b, a]}, z]

ShiftBA[{b_?SpinorInterpretableQ, assocA_}, a_?LVectorQ, z_] :=
	ShiftBA[{b, assocA}, {a, b}, z]
	
ShiftBA[{b_?MassiveLVectorQ, assocA_}, a_?LVectorQ, z_] :=
	ShiftBA[{b, assocA}, {a, SpAssoc[b, a]}, z]

ShiftBA[b_?LVectorQ, {a_?SpinorInterpretableQ, assocB_}, z_] :=
	ShiftBA[{b, a}, {a, assocB}, z]
ShiftBA[b_?LVectorQ, {a_?MassiveLVectorQ, assocB_}, z_] :=
	ShiftBA[{b, SpAssoc[a, b]}, {a, assocB}, z]

ShiftBA[
	{b_, assocA_?SpinorInterpretableQ},
	{a_, assocB_?SpinorInterpretableQ},
	z_
][x_] :=
	Module[
		{zz}
		,
		ASpinorShift[
			BSpinorShift[
				x,
				Replace[b, i_Integer :> Sp[i]],
				-zz Replace[assocA, i_Integer :> Sp[i]]
			],
			Replace[a, i_Integer :> Sp[i]],
			zz Replace[assocB, i_Integer :> Sp[i]]
		] /. zz -> z
	]*)

ShiftBA[b_?MassiveLVectorQ, a_?MassiveLVectorQ, z_][x_] :=
	Module[
		{
			result = x,
			eta = LvBA[SpAssoc[a, b], SpAssoc[b, a]]
		}
		,
		result = ReplaceLVector[result, {b -> b - z eta, a -> a + z eta}];
		
		result = ReplaceBSpinor[result, spM:SpM[b, ___] :> spM - z eta];
		result = ReplaceASpinor[result, spM:SpM[a, ___] :> spM + z eta];
		
		result
	]

Protect[ShiftBA]


(* ::Subsection:: *)
(*Num4V*)


Unprotect[Num4V]

(*
	Clear old simplifications.
	
	If someone loads package second time, below definitions will already be
	unset. Theres nothing wrong with it, no need to warn users, so we switch
	off Unset::norep message.
*)
Quiet[
	Num4V[coeff__?(FreeQ[#,_?LVectorQ]&) v_?LVectorQ] =.;
	Num4V[coeff__?(FreeQ[#,_?LVectorQ]&) p_Plus] =.;
	,
	{Unset::norep}
]

(*
	Add possibility of coefficients containing LVectors e.g. MP[P, Q] is valid
	coefficient but was excluded by original implementation.
*)
Num4V[coeff__?ScalarQ v_?LVectorQ] := coeff Num4V[v]
Num4V[coeff__?ScalarQ p_Plus] := coeff Num4V[p]

Protect[Num4V]


(* ::Subsection:: *)
(*Spxy*)


Unprotect[Spaa, Spbb, Spab, Spba]


(*ToBeExpandedExtern :=
	_Plus | coeff__?(Not[MatchQ[#, _?LVectorQ]]&) _?LVectorQ

SpinorExpand[(sp:SpinProdIntern)[p2__, coeff__ p1_?LVectorQ]] :=
	coeff SpinorExpand[sp[p1, p2]]
SpinorExpand[(sp:SpinProdIntern)[coeff__ p1_?LVectorQ, p2__]] :=
	coeff SpinorExpand[sp[p1, p2]]

Spaa[p:ToBeExpandedExtern, a___] :=
	(SpinorExpand[Sptemp[p, a]] /. Sptemp -> Spaa) /; $SpinorAutoExpand
Spab[p:ToBeExpandedExtern, a___] :=
	(SpinorExpand[Sptemp[p, a]] /. Sptemp -> Spab) /; $SpinorAutoExpand
Spba[p:ToBeExpandedExtern, a___] :=
	(SpinorExpand[Sptemp[p, a]] /. Sptemp -> Spba) /; $SpinorAutoExpand
Spbb[p:ToBeExpandedExtern, a___] :=
	(SpinorExpand[Sptemp[p, a]] /. Sptemp -> Spbb) /; $SpinorAutoExpand
	
Spaa[b___, p:ToBeExpandedExtern] :=
	(SpinorExpand[Sptemp[b, p]] /. Sptemp -> Spaa) /; $SpinorAutoExpand
Spab[b___, p:ToBeExpandedExtern] :=
	(SpinorExpand[Sptemp[b, p]] /. Sptemp -> Spab) /; $SpinorAutoExpand
Spba[b___, p:ToBeExpandedExtern] :=
	(SpinorExpand[Sptemp[b, p]] /. Sptemp -> Spba) /; $SpinorAutoExpand
Spbb[b___, p:ToBeExpandedExtern] :=
	(SpinorExpand[Sptemp[b, p]] /. Sptemp -> Spbb) /; $SpinorAutoExpand*)


(*
	Unset simplifications for SmBA attached to Spxy functions.
	First in each pair had a bug there's a_ instead of a_?SpinorQ.
*)
Spaa[a_, aa___?SMatrixQ, SmBA[B_?SpinorQ, A_?SpinorQ], bb__] =.
Spaa[aa__, SmBA[B_?SpinorQ, A_?SpinorQ], bb___, b_?SpinorQ] =.

Spbb[a_, aa___?SMatrixQ, SmBA[B_?SpinorQ, A_?SpinorQ], bb__] =.
Spbb[aa__, SmBA[B_?SpinorQ, A_?SpinorQ], bb___, b_?SpinorQ] =.

Spab[a_, aa___?SMatrixQ, SmBA[B_?SpinorQ, A_?SpinorQ], bb__] =.
Spab[aa__, SmBA[B_?SpinorQ, A_?SpinorQ], bb___, b_?SpinorQ] =.

Spba[a_, aa___?SMatrixQ, SmBA[B_?SpinorQ, A_?SpinorQ], bb__] =.
Spba[aa__, SmBA[B_?SpinorQ, A_?SpinorQ], bb___, b_?SpinorQ] =.


Protect[Spaa, Spbb, Spab, Spba]


(* ::Subsection:: *)
(*SmBA*)


Unprotect[SmBA]


(*
	Set simplifications for SmBA attached to SmBA.
	Don't include massive spinors inside SmBA, for massive b and a:
	|b]<a| + |a>[b| is not a slashed matrix (it's not chiral swapping).
*)
SmBA /:
	(sp:SpinProd)[
		before__,
		SmBA[b_?SpinorQ, a_?SpinorQ],
		after__
	] :=
		ChangeSecondToA[sp][before, a] ChangeFirstToB[sp][b, after] +
		ChangeSecondToB[sp][before, b] ChangeFirstToA[sp][a, after]


Protect[SmBA]


(* ::Subsection:: *)
(*Proportionality*)


Unprotect[Spaa, Spbb, Spab, Spba]


Spaa[args__] /; MemberQ[Partition[{args}, 2, 1], _?LVectorProportionalQ] = 0
Spbb[args__] /; MemberQ[Partition[{args}, 2, 1], _?LVectorProportionalQ] = 0
Spab[args__] /; MemberQ[Partition[{args}, 2, 1], _?LVectorProportionalQ] = 0
Spba[args__] /; MemberQ[Partition[{args}, 2, 1], _?LVectorProportionalQ] = 0


Spbb[a_, b_, ___] /; BSpinorProportionalQ[a, b] = 0
Spbb[___, a_, b_] /; BSpinorProportionalQ[a, b] = 0
Spba[a_, b_, ___] /; BSpinorProportionalQ[a, b] = 0
Spab[___, a_, b_] /; BSpinorProportionalQ[a, b] = 0


Spaa[a_, b_, ___] /; ASpinorProportionalQ[a, b] = 0
Spaa[___, a_, b_] /; ASpinorProportionalQ[a, b] = 0
Spab[a_, b_, ___] /; ASpinorProportionalQ[a, b] = 0
Spba[___, a_, b_] /; ASpinorProportionalQ[a, b] = 0


Protect[Spaa, Spbb, Spab, Spba]


End[]


EndPackage[]

