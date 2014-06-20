(* ::Package:: *)

BeginPackage["SpinorsExtras`Pol`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


DeclarePossiblePol::usage =
"\
DeclarePossiblePol[x] \
declares x as symbolic representation of posible vector boson polarizations.\

DeclarePossiblePol[x, y, ...] \
declares x, y, ... as symbolic representations of posible vector boson \
polarizations."


UndeclarePossiblePol::usage =
"\
UndeclarePossiblePol[x] \
removes x from list of symbolic representations of posible vector boson \
polarizations.\

UndeclarePossiblePol[x, y, ...] \
removes x, y, ... from list of symbolic representations of posible vector \
boson polarizations."


PossiblePolQ::usage =
"\
PossiblePolQ[x] \
returns True if x represents possible vector boson polarization."


PolVec::usage =
"\
PolVec[P, pol] \
represents polarization vector of vector boson with momentum P, polarization \
pol and implicit reference vector.\

PolVec[P, pol, ref] \
represents polarization vector of vector boson with momentum P, polarization \
pol and reference vector ref.

Transverse polarization vectors: PolVec[P, +/-1, ...] are lightlike \
four-vectors."


ExpandPolVec::usage =
"\
ExpandPolVec[expr, PolVec[P, pol]] \
returns expr with occurences of polarization vectors PolVec[P, pol] replaced \
by proper, for given polarization pol, combinations of four-vectors, slashed \
matrices and spinors associated with momentum P and default reference vector.\

ExpandPolVec[expr, PolVec[P, pol, q]] \
decomposes occurences of PolVec[P, pol, q], in expr, using reference vector q.\

ExpandPolVec[expr, patt] \
decomposes those occurences of polarization vectors, in expr, which match \
pattern patt.\

ExpandPolVec[expr] \
decomposes all polarization vectors in given expression expr."


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
Needs["SpinorsExtras`Composite`"]
Needs["SpinorsExtras`Phase`"]


(* ::Subsection:: *)
(*PossiblePol*)


DeclareUndeclareQFactory[
	"PossiblePol",
	(* Everything interpretable as +/-1 is also possible polarization. *)
	{_?PlusMinusOneQ, 0, "S"},
	"posible vector boson polarizations"
]


(* ::Subsection:: *)
(*PolVec*)


PolVec::forbidden0Pol =
"In `1` first argument `2` is massles so `3` polarization does not exist."


QuietSpinorPrint[
	DeclareSpinor[
		HoldPattern[
			PolVec[_?LVectorQ, _?PlusMinusOneQ, Repeated[_?SpinorQ, {0, 1}]]
		]
	];
	
	DeclareLVector[
		HoldPattern[
			PolVec[_?LVectorQ, _?PossiblePolQ, Repeated[_?SpinorQ, {0, 1}]]
		]
	];
]


PolVec[
	i_Integer,
	pol_?PossiblePolQ,
	ref:Repeated[_?SpinorInterpretableQ, {0, 1}]
] :=
	PolVec[Sp[i], pol, ref]
	
PolVec[P_?LVectorInterpretableQ, pol_?PossiblePolQ, i_Integer] :=
	PolVec[P, pol, Sp[i]]
	
PolVec[a_?SpinorQ, pol:(0 | "S"), ref:Repeated[_?SpinorQ, {0, 1}]] := (
	Message[PolVec::forbidden0Pol, HoldForm[PolVec[a, pol, ref]], a, pol];
	$Failed
)


(* ::Subsection:: *)
(*ExpandPolVec*)


Options[ExpandPolVec] = {"PhaseConvention" -> Automatic}


ExpandPolVec[expr_, opt:OptionsPattern[]] := ExpandPolVec[expr, _, opt]

ExpandPolVec[expr_, patt_, OptionsPattern[]] :=
	With[
		{
			matchPattQ = MatchQ[#, patt]&,
			phaseConvention = OptionValue["PhaseConvention"]
		}
		,
		
		ReplaceLVector[
			expr /. {
				pv:PolVec[P_?LVectorQ, -1, refVec_?SpinorQ]?matchPattQ :>
					LvBA[
						Phase[phaseConvention, pv] *
							Sqrt[2] SpAssoc[P, refVec] /
							Spaa[refVec, SpAssoc[P, refVec]]
						,
						refVec
					]
				,
				pv:PolVec[P_?LVectorQ, -1]?matchPattQ :>
					LvBA[
						Phase[phaseConvention, pv] *
							Sqrt[2] SpAssoc[P] / Spaa[SpRef[P], SpAssoc[P]]
						,
						SpRef[P]
					]
				,
				pv:PolVec[P_?LVectorQ, 1, refVec_?SpinorQ]?matchPattQ :>
					LvBA[
						refVec
						,
						Phase[phaseConvention, pv] *
							Sqrt[2] SpAssoc[P, refVec] /
							Spbb[refVec, SpAssoc[P, refVec]]
					]
				,
				pv:PolVec[P_?LVectorQ, 1]?matchPattQ :>
					LvBA[
						SpRef[P]
						,
						Phase[phaseConvention, pv] *
							Sqrt[2] SpAssoc[P] / Spbb[SpRef[P], SpAssoc[P]]
					]
			}
			,
			{
				pv:PolVec[P_?LVectorQ, 0, refVec_?SpinorQ]?matchPattQ :>
					Phase[phaseConvention, pv] *
						(
							P / Sqrt[Abs[MP2[P]]] -
							MP2[P] refVec / (Sqrt[Abs[MP2[P]]] MP[P, refVec])
						)
				,
				pv:PolVec[P_?LVectorQ, 0]?matchPattQ :>
					Phase[phaseConvention, pv] *
						(
							P / Sqrt[Abs[MP2[P]]] -
							MP2[P] SpRef[P] / (Sqrt[Abs[MP2[P]]] MP[P, SpRef[P]])
						)
				,
				pv:PolVec[P_?LVectorQ, "S", _?SpinorQ]?matchPattQ :>
					Phase[phaseConvention, pv] P / Sqrt[Abs[MP2[P]]]
				,
				pv:PolVec[P_?LVectorQ, "S"]?matchPattQ :>
					Phase[phaseConvention, pv] P / Sqrt[Abs[MP2[P]]]
			}
		]
	]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
