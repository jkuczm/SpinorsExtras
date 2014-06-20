(* ::Package:: *)

BeginPackage["SpinorsExtras`Utilities`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


DeclarePlusMinusOne::usage =
"\
DeclarePlusMinusOne[x] \
declares x as symbolic representation of +1 or -1.\

DeclarePlusMinusOne[x, y, ...] \
declares x, y, ... as symbolic representations of +1 or -1."


UndeclarePlusMinusOne::usage =
"\
UndeclarePlusMinusOne[x] \
removes x from list of symbolic representations of +1 or -1.\

UndeclarePlusMinusOne[x, y, ...] \
removes x, y, ... from list of symbolic representations of +1 or -1."


PlusMinusOneQ::usage =
"\
PlusMinusOneQ[x] \
returns True if x represents +1 or -1.\

It is used internally to check whether something represents possible sign of \
spin projection."


MassiveLVectorQ::usage =
"\
MassiveLVectorQ[x] \
returns True if x is interpretable as LVector but not as a massless Spinor. \
Returns False otherwise."


ReplaceLVector::usage =
"\
ReplaceLVector[expr, P -> Q] \
returns expr with Lorentz vector P replaced by Q.\

ReplaceLVector[expr, patt -> Q] \
returns expr with all Lorentz vectors that match pattern patt replaced by Q.\

ReplaceLVector[expr, patt :> replacement] \
returns expr with all Lorentz vectors that match pattern patt replaced by \
replacement. patt can have named patterns inside and replacement can use \
variables matched by those named patterns.\

ReplaceLVector[expr, {P -> Q, patt :> replacement, ...}] \
returns expr with replacement using all given rules.\

ReplaceLVector[expr, {{P1 -> Q1, ...}, {P2 -> Q2, ...}, ...] \
returns a List, each element of this list is a result of replacement using \
subsequent list of rules.

ReplaceLVector behaves as built-in ReplaceAll function but replaces only \
expressions that are found in places where they can be interpreted as objects \
proportional to four-vector. So e.g. label inside spinor chain (representing \
slashed matrix) can be replaced, but label at the end of spinor chain \
(representing spinor - an object that scales as square root of four-vector) \
will never be replaced."


ReplaceBSpinor::usage =
"\
ReplaceBSpinor[expr, x -> r] \
returns expr with massless or massive B spinors, labeled by x, replaced by r, \
r can be linear combination of massless or massive spinors.\

ReplaceBSpinor[expr, x -> {b, a}] \
replaces massless or massive B spinor x at beginning of spinor chain with \
difference of B spinor b and A spinor a. B spinor x at end of spinor chain \
will be replaced by sum of B spinor b and A spinor a. b can be linear \
combinations of labels they will be interpreted as linear combinations of B \
spinors, analogously a can be linear combinations of labels and they will be \
interpreted as linear combinations of A spinors.\

ReplaceBSpinor[expr, patt -> r] \
returns expr with all massless or massive B spinors that match pattern patt \
replaced by r.\

ReplaceBSpinor[expr, patt -> {b, a}] \
returns expr with all massless or massive B spinors, at beginning of spinor \
chain, that match pattern patt replaced by difference of B spinor b and A \
spinor a. B spinors at end of spinor chain, that match pattern patt, will be \
replaced by sum of B spinor b and A spinor a.\

ReplaceBSpinor[expr, patt :> replacement] \
returns expr with all massless or massive B spinors that match pattern patt \
replaced by replacement. patt can have named patterns inside and replacement \
can use variables matched by those named patterns.\

ReplaceBSpinor[expr, patt :> {replacementB, replacementA}] \
replaces B spinors, that match pattern patt, with difference of B spinor \
replacementB and A spinor replacementA. B spinors at end of spinor chain, \
that match pattern patt, will be replaced by sum of B spinor replacementB and \
A spinor replacementA. replacementB and replacementA can use variables \
matched by named patterns in patt.\

ReplaceBSpinor[expr, {x -> {b, a}, patt :> replacement, ...}] \
returns expr with replacement using all given rules.\

ReplaceBSpinor[expr, {{x1 -> r1, ...}, {x2 -> {b2, a2}, ...}, ...] \
returns a List, each element of this list is a result of replacement using \
subsequent list of rules.

ReplaceBSpinor behaves as built-in ReplaceAll function but replaces only \
expressions that are found in places where they can be interpreted as \
massless or massive B spinors."


ReplaceASpinor::usage =
"\
ReplaceASpinor[expr, x -> r] \
returns expr with massless or massive A spinors, labeled by x, replaced by r, \
r can be linear combination of massless or massive spinors.\

ReplaceASpinor[expr, x -> {a, b}] \
replaces massless or massive A spinor x at beginning of spinor chain with \
difference of A spinor a and B spinor b. A spinor x at end of spinor chain \
will be replaced by sum of A spinor a and B spinor b. b can be linear \
combinations of labels they will be interpreted as linear combinations of \
B spinors, analogously a can be linear combinations of labels and they will \
be interpreted as linear combinations of A spinors.\

ReplaceASpinor[expr, patt -> r] \
returns expr with all massless or massive A spinors that match pattern patt \
replaced by r.\

ReplaceASpinor[expr, patt -> {a, b}] \
returns expr with all massless or massive A spinors, at beginning of spinor \
chain, that match pattern patt replaced by difference of A spinor a and B \
spinor b. A spinors at end of spinor chain, that match pattern patt, will be \
replaced by sum of A spinor a and B spinor b.\

ReplaceASpinor[expr, patt :> replacement] \
returns expr with all massless or massive A spinors that match pattern patt \
replaced by replacement. patt can have named patterns inside and replacement \
can use variables matched by those named patterns.\

ReplaceASpinor[expr, patt :> {replacementA, replacementB}] \
replaces A spinors, that match pattern patt, with difference of A spinor \
replacementA and B spinor replacementB. A spinors at end of spinor chain, \
that match pattern patt, will be replaced by sum of A spinor replacementA and \
B spinor replacementB. replacementB and replacementA can use variables \
matched by named patterns in patt.\

ReplaceASpinor[expr, {x -> {a, b}, patt :> replacement, ...}] \
returns expr with replacement using all given rules.\

ReplaceASpinor[expr, {{x1 -> r1, ...}, {x2 -> {a2, b2}, ...}, ...] \
returns a List, each element of this list is a result of replacement using \
subsequent list of rules.

ReplaceASpinor behaves as built-in ReplaceAll function but replaces only \
expressions that are found in places where they can be interpreted as \
massless or massive A spinors."


ReplaceSpinor::usage =
"\
ReplaceSpinor[expr, x -> r] \
returns expr with massless or massive spinors x replaced by r, r can be linear \
combination of massless or massive spinors.\

ReplaceSpinor[expr, x -> {b, a}] \
replaces B spinor x with b and A spinor x with a, a and b can be linear \
combinations of massless or massive spinors.\

ReplaceSpinor[expr, patt -> r] \
returns expr with all massless or massive spinors that match pattern patt \
replaced by r.\

ReplaceSpinor[expr, patt -> {b, a}] \
returns expr with all massless or massive B spinors that match pattern patt \
replaced by b and all A spinors that match patt with a.\

ReplaceSpinor[expr, patt :> replacement] \
returns expr with all massless or massive spinors that match pattern patt \
replaced by replacement. patt can have named patterns inside and replacement \
can use variables matched by those named patterns.\

ReplaceSpinor[expr, patt :> {replacementB, replacementA}] \
replaces B spinors with replacementB and A spinors with replacementA. \
replacementB and replacementA can use variables matched by named patterns in \
patt.\

ReplaceSpinor[expr, {x -> {b, a}, patt :> replacement, ...}] \
returns expr with replacement using all given rules.\

ReplaceSpinor[expr, {{x1 -> r1, ...}, {x2 -> {b2, a2}, ...}, ...] \
returns a List, each element of this list is a result of replacement using \
subsequent list of rules.

ReplaceSpinor behaves as built-in ReplaceAll function but replaces only \
expressions that are found in places where they can be interpreted as objects \
that scale as square root of four-vector."


ExternalMomentaPartitions::usage =
"\
ExternalMomentaPartitions[{P1, P2, P3, ...}, {Pi, Pj}] \
returns list of possible partitions of given momenta P1, P2, P3, ... into two \
lists such that each list contains Pi or Pj."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]


PrependTo[$ContextPath, "Spinors`Private`"]


(* ::Subsection:: *)
(*PlusMinusOne*)


DeclareUndeclareQFactory[
	"PlusMinusOne",
	{+1, -1, PlusMinus[1], MinusPlus[1]},
	"symbolic representations of +1 or -1"
]


(* ::Subsection:: *)
(*MassiveLVectorQ*)


MassiveLVectorQ[x_] := LVectorQ[x] && !SpinorQ[x]


(* ::Subsection:: *)
(*OneRuleListOfListsFactory*)


OneRuleListOfListsFactory[replaceFunction_Symbol] := (
	replaceFunction[expr_, rule:(_Rule | _RuleDelayed)] :=
		replaceFunction[expr, {rule}];
	
	replaceFunction[expr_, rules:{{(_Rule | _RuleDelayed)..}..}] :=
		replaceFunction[expr, #]& /@ rules
)


(* ::Subsection:: *)
(*ReplaceLVector*)


ReplaceLVector[expr_, rules:{(_Rule | _RuleDelayed)..}] :=
	Module[
		{
			spinorizedRules = Map[SpinorizeInteger, rules, {2}],
			tmpWrapper
		}
		,
		(*
			Replaced expression can occur multiple times inside Spxy, s, or MP,
			so PeplaceRepeated (//.) is used. To avoid circular replacemets,
			e.g. for P -> -P, this replacement is separated ito two steps:
			first all expressions to be replaced are wrapped with tmpWrapper,
			then all wrapped expressions are replaced using ReplaceAll (/.).
		*)
		expr //. Flatten[
			{
				(spxy:SpinProd)[
					a__,
					P:Except[_tmpWrapper, #1],
					b__
				] :>
					spxy[a, tmpWrapper[P], b]
						
				,
				s[a___, P:Except[_tmpWrapper, #1], b___] :>
					s[a, tmpWrapper[P], b]
				,
				MP[P:Except[_tmpWrapper, #1], a_] |
					MP[a_, P:Except[_tmpWrapper, #1]] :>
						MP[a, tmpWrapper[P]]
			}& @@@
				spinorizedRules
		] /. Flatten[
			{
				tmpWrapper[#1] :> #2,
				(sm:SmHeadPattern)[#1] :> sm[#2]
			}& @@@
				spinorizedRules
		]
	]
	
OneRuleListOfListsFactory[ReplaceLVector]


(* ::Subsection:: *)
(*ReplaceBSpinor*)


ReplaceBSpinor[expr_, rules:{(_Rule | _RuleDelayed)..}] :=
	Module[
		{
			spinorizedRules = Map[SpinorizeInteger, rules, {2}]
			,
			tmpWrapperB,
			tmpWrapperA,
			tmpWrappPatt
		}
		,
		tmpWrappPatt = _tmpWrapperB | _tmpWrapperA;
		
		expr //. Flatten[
			If[MatchQ[#2, {_, _}],
				{
					(spby:(Spbb|Spba))[x:Except[tmpWrappPatt, #1], d__] :>
						spby[tmpWrapperB[x], d]
						- ChangeFirst[spby][tmpWrapperA[x], d]
					,
					(spxb:(Spbb|Spab))[c__, x:Except[tmpWrappPatt, #1]] :>
						spxb[c, tmpWrapperB[x]]
						+ ChangeSecond[spxb][c, tmpWrapperA[x]]
				}
			(* else *),
				{
					(spby:(Spbb|Spba))[x:Except[tmpWrappPatt, #1], d__] :>
						spby[tmpWrapperB[x], d]
					,
					(spxb:(Spbb|Spab))[c__, x:Except[tmpWrappPatt, #1]] :>
						spxb[c, tmpWrapperB[x]]
				}
			]& @@@
				spinorizedRules
		] /. Flatten[
			If[MatchQ[#2, {_, _}],
				{
					tmpWrapperB[#1] :> First[#2],
					tmpWrapperA[#1] :> Last[#2],
					If[And @@ SpinorInterpretableQ /@ #2,
						SmBA[#1, d_] :>
							SmBA[First[#2], d] + Spaa[d, Last[#2]] ProjMinus
					(* else *),
						Unevaluated[Sequence[]]
					]
				}
			(* else *),
				{
					tmpWrapperB[#1] :> #2,
					SmBA[#1, d_] :> SmBA[#2, d]
				}
			]& @@@
				spinorizedRules
		]
	]
	
OneRuleListOfListsFactory[ReplaceBSpinor]


(* ::Subsection:: *)
(*ReplaceASpinor*)


ReplaceASpinor[expr_, rules:{(_Rule | _RuleDelayed)..}] :=
	Module[
		{
			spinorizedRules = Map[SpinorizeInteger, rules, {2}]
			,
			tmpWrapperB,
			tmpWrapperA,
			tmpWrappPatt
		}
		,
		tmpWrappPatt = _tmpWrapperB | _tmpWrapperA;
		
		expr //. Flatten[
			If[MatchQ[#2, {_, _}],
				{
					(spay:(Spaa|Spab))[x:Except[tmpWrappPatt, #1], d__] :>
						spay[tmpWrapperA[x], d]
						- ChangeFirst[spay][tmpWrapperB[x], d]
					,
					(spxa:(Spaa|Spba))[c__, x:Except[tmpWrappPatt, #1]] :>
						spxa[c, tmpWrapperA[x]]
						+ ChangeSecond[spxa][c, tmpWrapperB[x]]
				}
			(* else *),
				{
					(spay:(Spaa|Spab))[x:Except[tmpWrappPatt, #1], d__] :>
						spay[tmpWrapperA[x], d]
					,
					(spxa:(Spaa|Spba))[c__, x:Except[tmpWrappPatt, #1]] :>
						spxa[c, tmpWrapperA[x]]
				}
			]& @@@
				spinorizedRules
		] /. Flatten[
			If[MatchQ[#2, {_, _}],
				{
					tmpWrapperA[#1] :> First[#2],
					tmpWrapperB[#1] :> Last[#2],
					If[And @@ SpinorInterpretableQ /@ #2,
						SmBA[c_, #1] :>
							SmBA[c, First[#2]] + Spbb[c, Last[#2]] ProjPlus
					(* else *),
						Unevaluated[Sequence[]]
					]
				}
			(* else *),
				{
					tmpWrapperA[#1] :> #2,
					SmBA[c_, #1] :> SmBA[c, #2]
				}
			]& @@@
				spinorizedRules
		]
	]
	
OneRuleListOfListsFactory[ReplaceASpinor]


(* ::Subsection:: *)
(*ReplaceSpinor*)


DoubleNonPair[arg:{_, _}] := arg

DoubleNonPair[arg_] := {arg, arg}


ReplaceSpinor[expr_, rules:{(_Rule | _RuleDelayed)..}] :=
	Module[
		{
			spinorizedRules =
				MapAt[DoubleNonPair, #, {2}]& /@
					Map[SpinorizeInteger, rules, {2}]
			,
			tmpWrapperB,
			tmpWrapperA
		}
		,
		expr //. Flatten[
			{
				(spby:(Spbb|Spba))[x:Except[_tmpWrapperB, #1], d__] :>
					spby[tmpWrapperB[x], d]
				,
				(spxb:(Spbb|Spab))[c__, x:Except[_tmpWrapperB, #1]] :>
					spxb[c, tmpWrapperB[x]]
				,
				(smBA:SmBAHeadPattern)[
					x:Except[_tmpWrapperB, #1],
					d_
				] :>
					smBA[tmpWrapperB[x], d]
				,
				(spay:(Spaa|Spab))[x:Except[_tmpWrapperA, #1], d__] :>
					spay[tmpWrapperA[x], d]
				,
				(spxa:(Spaa|Spba))[c__, x:Except[_tmpWrapperA, #1]] :>
					spxa[c, tmpWrapperA[x]]
				,
				(smBA:SmBAHeadPattern)[
					c_,
					x:Except[_tmpWrapperA, #1]
				] :>
					smBA[c, tmpWrapperA[x]]
			}& @@@
				spinorizedRules
		] /. Flatten[
			{
				tmpWrapperB[#1] :> First[#2],
				tmpWrapperA[#1] :> Last[#2],
				(spB:BSpHeadPattern)[#1] :> spB[First[#2]],
				(spA:ASpHeadPattern)[#1] :> spA[Last[#2]]
			}& @@@
				spinorizedRules
		]
	]
	
OneRuleListOfListsFactory[ReplaceSpinor]


(* ::Subsection:: *)
(*ExternalMomentaPartitions*)


ExternalMomentaPartitions::momMissing = "`1` momenta are not present in `2`."

ExternalMomentaPartitions[mom_List, singledMom:{p1_, p2_}] := Module[
	{notPresent = Complement[singledMom, mom]}
	,
	If[Length[notPresent] >= 1,
		Message[ExternalMomentaPartitions::momMissing, notPresent, mom];
		Return[$Failed]
	];
	
	{#, Complement[mom, #]} & /@ Select[
		Subsets[mom, {2, Length[mom] - 2}],
		MemberQ[#, p1] && !MemberQ[#, p2]&
	]
]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
