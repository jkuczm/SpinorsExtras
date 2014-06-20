(* ::Package:: *)

BeginPackage["SpinorsExtras`Numerics`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


DeclareSpinorRandomMomentum::usage =
"\
DeclareSpinorRandomMomentum[a, maxP] \
declares random four-vector with three-vector components between \
-maxP and +maxP, and with positive energy, to be the four-vector associated \
with a spinor.\

DeclareSpinorRandomMomentum[a] \
uses 1 as default value for maxP."


GenComplexMomenta::usage =
"\
GenComplexMomenta[{s1, s2, s3}] \
generates random complex four momenta for the spinors s1, s2, s3 so that they \
sum to zero."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


(* ::Subsection:: *)
(*DeclareSpinorRandomMomentum*)


Options[DeclareSpinorRandomMomentum] = {"Complex" -> False}


DeclareSpinorRandomMomentum[a_, maxP:(_?NumberQ):1, OptionsPattern[]] :=
	Module[
		{
			threeMom =
				If[OptionValue["Complex"],
					RandomComplex[{-maxP, maxP}(1 + I), 3]
				(* else *),
					RandomReal[{-maxP, maxP}, 3]
				]
		}
		,
		DeclareSpinorMomentum[a, Prepend[#, Sqrt[Total[#^2]]]& @ threeMom]
	]


(* ::Subsection:: *)
(*GenComplexMomenta*)


Options[GenComplexMomenta] = {"Range" -> {1 + I, -1 - I}}


GenComplexMomenta[spinors:{_, _, _}, OptionsPattern[]] :=
	Module[
		{
			commonEpsLa = List /@ RandomComplex[OptionValue["Range"], 2],
			mostSpinors = Most[spinors]
		}
		,
		DeclareSpinorMomentum[
			#,
			PfromBiSpinor[commonEpsLa.{RandomComplex[OptionValue["Range"], 2]}]
		]& /@
			mostSpinors;
		
		DeclareSpinorMomentum[Last[spinors], - Total[Num4V /@ mostSpinors]];
	]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
