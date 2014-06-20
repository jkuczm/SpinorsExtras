(* ::Package:: *)

BeginPackage["SpinorsExtras`Phase`"]


(* ::Section:: *)
(*Usage messages*)


Phase::usage =
"\
Phase[phaseConvention, expr] \
represents additional phase of expression represented by expr. \
Custom phase conventions for expr can be defined by setting: \
Phase[\"MyConventionName\", expr] := ..."


AppendPhase::usage =
"\
AppendPhase[expr, patt] \
multiplies each subexpression of expr, that match pattern patt, with a Phase."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


(* ::Subsection:: *)
(*Phase*)


Phase[Automatic, _] = 1


(* ::Subsection:: *)
(*AppendPhase*)


Options[AppendPhase] = {
	"PhaseConvention" -> Automatic
}


AppendPhase[expr_, pattern_, OptionsPattern[]] :=
	expr /. x:pattern :> Phase[OptionValue["PhaseConvention"], x] x


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
