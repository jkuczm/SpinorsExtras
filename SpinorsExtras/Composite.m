(* ::Package:: *)

BeginPackage["SpinorsExtras`Composite`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


LvBA::usage =
"\
LvBA[b, a] \
represents vector composed of two independent massless or massive spinors \
labeled by b and a: 1/2 [b|\*SuperscriptBox[\[Gamma], \[Mu]]|a>.

If at least one of spinors is massless than LvBA[b, a] is also massless."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]
Needs["SpinorsExtras`Utilities`"]
Needs["SpinorsExtras`Massive`"]
Needs["SpinorsExtras`MassiveUtilities`"] (* LightConeDecompose *)


PrependTo[$ContextPath, "Spinors`Private`"] (* SpinProd *)
PrependTo[$ContextPath, "SpinorsExtras`Massive`Private`"]
(* ScaledAnySpinorQ *)


(* ::Subsection:: *)
(*LvBA*)


QuietSpinorPrint[
	DeclareSpinor[
		HoldPattern[LvBA[_?ScaledSpinorQ, _?ScaledAnySpinorQ]],
		HoldPattern[LvBA[_?ScaledAnySpinorQ, _?ScaledSpinorQ]]
	];
	
	DeclareLVector[
		HoldPattern[LvBA[_?ScaledAnySpinorQ, _?ScaledAnySpinorQ]]
	];
]


LvBA[i_Integer, j_Integer] := LvBA[Sp[i], Sp[j]]
LvBA[i_Integer, a_?ScaledAnySpinorQ] := LvBA[Sp[i], a]
LvBA[b_?ScaledAnySpinorQ, i_Integer] := LvBA[b, Sp[i]]

LvBA[x_?SpinorQ | coeffB__ x_?SpinorQ, x_ | coeffA__ x_] := coeffB coeffA x
LvBA[
	(x:SpM[P_?LVectorQ, _?PlusMinusOneQ, Repeated[_?SpinorQ, {0, 1}]]) |
	(coeffB__ x:SpM[P_?LVectorQ, _?PlusMinusOneQ, Repeated[_?SpinorQ, {0, 1}]])
	,
	x_ | coeffA__ x_
] :=
	coeffB coeffA P

LvBA[
	b_?SpinorQ | coeffB__ b_?SpinorQ
	,
	SpM[P_?LVectorQ, _?PlusMinusOneQ, q:Repeated[_?SpinorQ, {0, 1}]] |
	coeffA__ SpM[P_?LVectorQ, _?PlusMinusOneQ, q:Repeated[_?SpinorQ, {0, 1}]]
] :=
	LvBA[coeffB b, coeffA SpAssoc[P, q]]

LvBA[
	SpM[P_?LVectorQ, _?PlusMinusOneQ, q:Repeated[_?SpinorQ, {0, 1}]] |
	coeffB__ SpM[P_?LVectorQ, _?PlusMinusOneQ, q:Repeated[_?SpinorQ, {0, 1}]]
	,
	a_?SpinorQ | coeffA__ a_?SpinorQ
] :=
	LvBA[coeffB SpAssoc[P, q], coeffA a]


LvBA[LvBA[b_?ScaledSpinorQ, _?ScaledSpinorQ], y_] := LvBA[b, y]

LvBA[x_, LvBA[_?ScaledSpinorQ, a_?ScaledSpinorQ]] := LvBA[x, a]


(* ::Subsection:: *)
(*DeclareLVectorMomentum[LvBA[...]]*)


DeclareLVectorMomentum[
	lvBA:LvBA[
		spB:(
			b_?SpinorQ |
			SpM[P_?LVectorQ, _?PlusMinusOneQ, qB:Repeated[_?SpinorQ, {0, 1}]]
		)
		,
		spA:(
			a_?SpinorQ |
			SpM[Q_?LVectorQ, _?PlusMinusOneQ, qA:Repeated[_?SpinorQ, {0, 1}]]
		)
	]
] ^:= (
	With[
		{
			notDeclared =
				DeleteCases[{b, P, qB, a, Q, qA}, _?NumSpinorQ | _?NumVectorQ]
		}
		,
		If[notDeclared =!= {},
			Message[DeclareLVectorMomentum::firstDeclare, lvBA, notDeclared];
			Return[$Failed]
		]
	];
	
	(* If associated vectors don't have numerical values declare them. *)
	DeclareSpinorMomentum /@
		DeleteCases[
			{SpAssoc[P, qB], SpAssoc[Q, qA]},
			SpAssoc[] | _?NumSpinorQ
		];

	DeclareLVectorMomentum[
		lvBA
		,
		(Spba[spB, #, spA] / 2)& /@ {Gamma0, Gamma1, Gamma2, Gamma3}
			// LightConeDecompose // N
	]
)


(* ::Subsection:: *)
(*DeclareSpinorMomentum[LvBA[...]]*)


DeclareSpinorMomentum[lvBA:LvBA[b_?SpinorQ, a_?SpinorQ]] ^:= (
	With[
		{notDeclared = DeleteCases[{b, a}, _?NumSpinorQ]}
		,
		If[notDeclared =!= {},
			Message[DeclareSpinorMomentum::firstDeclare, lvBA, notDeclared];
			Return[$Failed]
		]
	];

	DeclareSpinorMomentum[lvBA, La[a] // N, Lat[b] // N]
)


(* ::Subsection:: *)
(*Automatic replacement of LvBA with proper spinors*)


LvBA /:
	(spxy:SpinProd)[
		before__,
		LvBA[b_?ScaledSpinorQ, a_?ScaledSpinorQ],
		after__
	] :=
		ChangeSecondToA[spxy][before, a] ChangeFirstToB[spxy][b, after] +
		ChangeSecondToB[spxy][before, b] ChangeFirstToA[spxy][a, after]


LvBA /: (spby:(Spbb | Spba))[LvBA[b_?ScaledSpinorQ, _?ScaledSpinorQ], y__] :=
	spby[b, y]
		
LvBA /: (spxb:(Spbb | Spab))[x__, LvBA[b_?ScaledSpinorQ, _?ScaledSpinorQ]] :=
	spxb[x, b]

LvBA /: (spay:(Spaa | Spab))[LvBA[_?ScaledSpinorQ, a_?ScaledSpinorQ], y__] :=
	spay[a, y]

LvBA /: (spxa:(Spaa | Spba))[x__, LvBA[_?ScaledSpinorQ, a_?ScaledSpinorQ]] :=
	spxa[x, a]


LvBA /: (SpB:BSpHeadPattern)[LvBA[b_?ScaledSpinorQ, _?ScaledSpinorQ]] := SpB[b]
LvBA /: (SpA:ASpHeadPattern)[LvBA[_?ScaledSpinorQ, a_?ScaledSpinorQ]] := SpA[a]

LvBA /: s[x___, LvBA[b_?ScaledAnySpinorQ, a_?ScaledAnySpinorQ], y___] :=
	Total[Spab[a, #, b]& /@ {x, y}] + s[y, x]


LvBA /: MP[LvBA[b_?ScaledAnySpinorQ, a_?ScaledAnySpinorQ], x_] :=
	1/2 Spab[a, x, b]


LvBA /: (sm:SmHeadPattern)[LvBA[b_?ScaledSpinorQ, a_?ScaledSpinorQ]] :=
	ChangeSmToSmBA[sm][b, a]


LvBA /: (sm:SmBAHeadPattern)[LvBA[b_?ScaledSpinorQ, _?ScaledSpinorQ], y_] :=
	sm[b, y]
	
LvBA /: (sm:SmBAHeadPattern)[x_, LvBA[_?ScaledSpinorQ, a_?ScaledSpinorQ]] :=
	sm[x, a]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
