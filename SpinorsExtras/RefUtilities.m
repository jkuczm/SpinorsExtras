(* ::Package:: *)

BeginPackage["SpinorsExtras`RefUtilities`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


ExplicitRef::usage =
"\
ExplicitRef[expr] \
returns expr where all occurrences of SpAssoc, SpM and PolVec with implicit \
reference vectors have added explicit default reference vectors.\

ExplicitRef[expr, P] \
adds reference vectors only to SpAssoc, SpM and PolVec with label P.

ExplicitRef[expr, patt] \
adds reference vectors only to SpAssoc, SpM and PolVec with labels matching \
patt."


ImplicitRef::usage =
"\
ImplicitRef[expr] \
returns expr where all occurrences of SpAssoc, SpM and PolVec with explicit \
default reference vectors have those vectors removed.\

ImplicitRef[expr, P] \
removes reference vectors only from SpAssoc, SpM and PolVec with label P.\

ImplicitRef[expr, patt] \
removes reference vectors only from SpAssoc, SpM and PolVec with labels \
matching patt.\

With option \"HideNonDefault\" set to True removes all explicit reference \
vectors including non-default ones."


RefInvariantQ::usage =
"\
RefInvariantQ[expr, ref -> {a, b, ...}] \
returns result of comparison between numerical values of expr with ref \
replaced by a, b, ... (those spinors should have declared numerical values).\

RefInvariantQ[expr, patt -> {a, b, ...}] \
replaces all occurrences of labels that match patt.\

RefInvariantQ[expr, patt :> {a, b, ...}] \
replaces all occurrences of labels that match patt. patt can contain named \
sub-patterns that can be used on RHS of :>.\

RefInvariantQ[expr, ref] \
returns comparison between numerical values of expr with ref replaced by two \
different random spinors.\

RefInvariantQ[expr, patt] \
replaces all occurrences of labels that match patt.\

RefInvariantQ[expr, {ref1, ref2 -> {a2, b2, ...}, ...}] \
tests invariance with respect to reference vectors ref1, ref2, ...\

RefInvariantQ[expr, {{ref11, ...}, {refPatt21 -> {a21, b21, ...}, ...}, ...}] \
returns a List, each element of this list is a result of invariance test of \
expr using subsequent list of patterns and rules.\

RefInvariantQ[expr] \
tests invariance with respect to all explicit reference vectors found in expr."


RefSimplify::usage =
"\
RefSimplify[expr, ref -> {a, b, ...}] \
returns simplest expression from list of results of replacing, in expr, \
spinor ref with a, b, ...\

RefSimplify[expr, ref -> patt] \
uses, for replacement, spinors found in expr that match pattern patt.\

RefSimplify[expr, refPatt -> ...] \
replaces all occurrences of labels that match refPatt.\

RefSimplify[expr, refPatt :> ...] \
replaces all occurrences of labels that match refPatt. refPatt can contain \
named sub-patterns that can be used on RHS of :>.\

RefSimplify[expr, ref] \
uses all spinors found in expr except ref.\

RefSimplify[expr, refPatt] \
replaces all occurrences of labels that match refPatt. For replacement uses \
all spinors found in expr except those that match refPatt.\

RefSimplify[expr, {ref1, ref2 -> patt2, refPatt3 -> {a3, b3, ...}, ...}] \
simplifies expr with respect to reference vectors matching: \
ref1, ref2, refPatt3, ...\

RefSimplify[\
expr, \
{{ref11, ref12 -> patt12, ...}, {refPatt21 -> {a21, b21, ...}, ...}, ...}\
] \
returns a List, each element of this list is a result of simplification of \
expr using subsequent list of patterns and rules.\

RefSimplify[expr] \
simplifies expr with respect to all explicit reference vectors found in expr."


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
Needs["SpinorsExtras`Pol`"]
Needs["SpinorsExtras`Numerics`"]


(* ::Subsection:: *)
(*ExplicitRef*)


(*
	We use pattern "_" as default value in optional argument labelPattern,
	that's intentional, so we switch off warrning.
*)
Quiet[
	ExplicitRef[expr_, labelPattern_:_] := expr //. {
		(h:(SpM|PolVec))[P:labelPattern, y_] :> h[P, y, SpRef[P]],
		SpAssoc[P:labelPattern] :> SpAssoc[P, SpRef[P]]
	}
	,
	{Optional::opdef}
]


(* ::Subsection:: *)
(*ImplicitRef*)


Options[ImplicitRef] = {"HideNonDefault" -> False}


Quiet[
	ImplicitRef[expr_, labelPattern:Except[_?OptionQ]:_, OptionsPattern[]] :=
		expr //.
			If[OptionValue["HideNonDefault"],
				{
					(h:(SpM|PolVec))[P:labelPattern, y_, _] :> h[P, y],
					SpAssoc[P:labelPattern, _] :> SpAssoc[P]
				}
			(* else *),
				{
					(h:(SpM|PolVec))[P:labelPattern, y_, SpRef[P_]] :> h[P, y],
					SpAssoc[P:labelPattern, SpRef[P_]] :> SpAssoc[P]
				}
			]
	,
	{Optional::opdef}
]


(* ::Subsection:: *)
(*RefInvariantQ*)


RefInvariantQ::NotANumber =
"Numeric values of given expression are: `1` from which at least one is not a \
number. Have you set numerical values for all Spinors, LVectors and SMatrices \
appearing in given expression?"

RefInvariantQ::RefAbsent =
"Reference vector `1` is not present in given expression."

RefInvariantQ::TooFewRefs =
"At least 2 possible reference vectors are needed, `1` given: `2`."

RefInvariantQ::NonInvariantForRef =
"Given expression is not invariant with respect to reference vectors: `1`."


Options[RefInvariantQ] = {
	"Accuracy" -> Automatic,
	"PostReplace" -> Identity,
	"Comparison" -> Equal,
	"Verbose" -> False
}


RefInvariantQ[
	expr_,
	rule:((Rule | RuleDelayed)[ref:Except[_String], replacementRefs:{__}]),
	OptionsPattern[]
] :=
	Module[
		{
			uniqueRefs =
				DeleteDuplicates[SpinorizeIntegerList[replacementRefs]],
			results,
			uniqueRefsNo
		}
		,
		uniqueRefsNo = Length[uniqueRefs];
		
		If[uniqueRefsNo < 2,
			Message[RefInvariantQ::TooFewRefs, uniqueRefsNo, uniqueRefs];
			Return[$Failed]
		];
		
		(* Warn if given reference label is not present in given expression. *)
		If[FreeQ[expr, ref],
			Message[RefInvariantQ::RefAbsent, ref];
		];
		
		results = expr /. (List /@ Distribute[rule, List]) // N;
		
		(* Warn if not all results are numbers or lists of numbers. *)
		If[Not[And @@ NumberQ /@ Flatten[results, Infinity, List]],
			Message[RefInvariantQ::NotANumber, results];
		];
		
		If[OptionValue["Accuracy"] =!= Automatic,
			results = SetAccuracy[results, OptionValue["Accuracy"]]
		];
		
		results = OptionValue["PostReplace"] /@ results;
		
		If[OptionValue["Verbose"],
			Print @ TableForm[
				results,
				TableHeadings -> {(ref -> #)& /@ uniqueRefs, None}
			];
		];
		
		OptionValue["Comparison"] @@ results
	]

RefInvariantQ[
	expr_,
	ref:Except[_List | _Rule | _RuleDelayed],
	opts:OptionsPattern[]
] :=
	Module[
		{
			tmpRef1, tmpRef2,
			replacementRefs,
			result
		}
		,
		replacementRefs = {tmpRef1, tmpRef2};
		
		QuietSpinorPrint[
			DeclareSpinorRandomMomentum /@ replacementRefs;
			
			(*
				Declare momenta for all associated vectors with given ref as
				reference vector replaced by temporary refs.
			*)
			DeclareSpinorMomentum /@
				Flatten[
					Cases[expr, SpAssoc[_, ref], Infinity] /.
						({ref -> #}& /@ replacementRefs)
				]
		];
		
		result = RefInvariantQ[expr, ref -> replacementRefs, opts];
		
		QuietSpinorPrint[UndeclareSpinor /@ replacementRefs];
		
		result
	]

RefInvariantQ[expr_, refs:{Except[_List]..}, opts:OptionsPattern[]] :=
	Module[
		{
			results,
			result
		}
		,
		results = RefInvariantQ[expr, #, opts] & /@ refs;
		result = And @@ results;
		
		If[! result,
			Message[
				RefInvariantQ::NonInvariantForRef,
				Pick[refs, Not /@ results]
			];
		];
		
		result
	]

RefInvariantQ[expr_, refs:{{Except[_List]..}..}, opts:OptionsPattern[]] :=
	RefInvariantQ[expr, #, opts]& /@ refs

RefInvariantQ[expr_, opts:OptionsPattern[]] :=
	With[
		{refs = DeleteDuplicates[Cases[expr, SpRef[_], {0, Infinity}]]}
		,
		If[refs === {},
			If[OptionValue["Verbose"],
				Print["No explicit reference vectors found."];
			];
			
			Return[True]
		];
		
		RefInvariantQ[expr, refs, opts]
	]


(* ::Subsection:: *)
(*AppendIndependentSpinors*)


AppendIndependentSpinors[spinors_List] :=
	With[
		{uniqueSpinors = DeleteDuplicates[spinors]}
		,
		DeleteDuplicates @ Join[
			uniqueSpinors,
			LvBA @@@ Tuples[uniqueSpinors, 2]
		]
	]


(* ::Subsection:: *)
(*QuietInfinities*)


SetAttributes[QuietInfinities, HoldFirst]


QuietInfinities[body_] := Quiet[body, {Power::infy, Infinity::indet}]


(* ::Subsection:: *)
(*InfinityFreeQ*)


InfinityFreeQ[expr_] := FreeQ[expr, _DirectedInfinity | Indeterminate]


(* ::Subsection:: *)
(*RefSimplify*)


Options[RefSimplify] = {
	"PostReplace" -> Simplify,
	"SimplicityMeasure" -> LeafCount,
	"IndependentSpinors" -> True,
	"ReplacementPattern" -> _?SpinorQ,
	"ExcludeReplacedRef" -> True,
	"ExcludeInfinities" -> True,
	"QuietInfinityWarnings" -> True,
	"Verbose" -> False,
	"PrintFunction" ->
		Function[
			{rule, simplicity, result, accepted},
			With[
				{
					rejected =
						If[accepted,
							""
						(* else *),
							"Rejected: "
						]
				}
				,
				Print[rejected, simplicity, ": ", rule, ": ", result]
			]
		]
}


RefSimplify[
	expr_,
	replacement:Except[_List | (_String -> _)],
	opts:OptionsPattern[]
] :=
	RefSimplify[expr, {replacement}, opts]

RefSimplify[
	expr_,
	replacementList:{Except[_List | (_String -> _)]..},
	OptionsPattern[]
] :=
	Module[
		{
			spinorizedReplList =
				Replace[
					replacementList,
					{
						rule:(_Rule | _RuleDelayed) :>
							(SpinorizeInteger /@ rule)
						,
						nonRule_ :> SpinorizeInteger[nonRule]
					},
					1
				]
			,
			toReplace,
			notFound,
			replacementRules,
			possibleRefs
			,
			prepareReplacement,
			acceptFunction,
			replaceFunction,
			handleOneRuleList
			,
			resultSimplicity = Infinity,
			result = expr
		}
		,
		toReplace =
			Replace[
				spinorizedReplList,
				r:(_Rule | _RuleDelayed) :> First[r],
				1
			];
		notFound = FreeQ[expr, #]& /@ toReplace;
		
		If[OptionValue["Verbose"] && (Or @@ notFound),
			Print[
				Pick[toReplace, notFound],
				" not found in given expression."];
		];
		
		If[And @@ notFound,
			(*
				None of given reference vectors was found in expr,
				so return it unchanged.
			*)
			Return[expr]
		];
		
		prepareReplacement =
			If[OptionValue["IndependentSpinors"],
				AppendIndependentSpinors
			(* else *),
				DeleteDuplicates
			];
		
		
		(* Exclude elements not present in expression. *)
		replacementRules = Pick[replacementList, notFound, False];
		
		
		(*
			If element of replacement list is not a rule treat it as ref that
			should be replaced by all possible spinors.
		*)
		replacementRules =
			Replace[
				replacementRules,
				patt:Except[_Rule | _RuleDelayed] :> (patt -> _),
				1
			];
		
		(*
			If among replacement rules there are ones with non-list RHS,
			extract all spinors from given expression and treat non-list RHS
			as patterns and replace them with matched spinors extracted from
			given expression.
		*)
		If[MemberQ[Last /@ replacementRules, Except[_List]],
			possibleRefs =
				DeleteDuplicates @ SpinorizeIntegerList @ Cases[
					expr,
					OptionValue["ReplacementPattern"],
					{0, Infinity}
				];
				
			If[OptionValue["ExcludeReplacedRef"],
				possibleRefs =
					DeleteCases[possibleRefs, Alternatives @@ toReplace];
			];
				
			possibleRefs = prepareReplacement[possibleRefs];
		];
		
		(*
			Spinorize integers given as possible replacement spinors.
			Use all possible composite vectors composed from given spinors.
		*)
		replacementRules =
			Map[
				ReplacePart[
					#
					,
					2 ->
						With[
							{rhs = #[[2]]}
							,
							If[MatchQ[rhs, _List],
								prepareReplacement[SpinorizeIntegerList[rhs]]
							(* else *),
								Cases[possibleRefs, rhs]
							]
						]
				]&
				,
				replacementRules
			];
		
		(* Change q -> {a, b, ...} to {q -> a, q -> b, ...}. *)
		replacementRules = Distribute[#, List] & /@ replacementRules;
		
		(* Use all combinations of replacement rules. *)
		replacementRules = Tuples[replacementRules];
		
		
		acceptFunction =
			If[OptionValue["ExcludeInfinities"],
				InfinityFreeQ
			(* else *),
				True&
			];
		
		With[
			{
				postReplace = OptionValue["PostReplace"],
				simplicityMeasure = OptionValue["SimplicityMeasure"]
			}
			,
			replaceFunction =
				Module[
					{
						newExpr = postReplace[expr /. #],
						simplicity,
						accepted
					}
					,
					accepted = acceptFunction[newExpr];
					simplicity = simplicityMeasure[newExpr];
					
					If[accepted && simplicity < resultSimplicity,
						resultSimplicity = simplicity;
						result = newExpr
					];
					
					{simplicity, newExpr, accepted}
				]&;
		];
			
		handleOneRuleList =
			If[OptionValue["Verbose"],
				With[
					{printFunction = OptionValue["PrintFunction"]}
					,
					With[
						{res = replaceFunction[#]}
						,
						printFunction[#, Sequence @@ res];
						
						res
					]&
				]
			(* else *),
				replaceFunction
			];
			
		If[OptionValue["QuietInfinityWarnings"],
			QuietInfinities
		(* else *),
			Identity
		][Scan[handleOneRuleList, replacementRules]];
		
		result
	]

RefSimplify[
	expr_,
	listOfReplacementLists:{{Except[_List | (_String -> _)]..}..},
	opts:OptionsPattern[]
] :=
	RefSimplify[expr, #, opts]& /@ listOfReplacementLists

RefSimplify[expr_, opts:OptionsPattern[]] :=
	With[
		{refs = DeleteDuplicates[Cases[expr, SpRef[_], {0, Infinity}]]}
		,
		If[refs === {},
			If[OptionValue["Verbose"],
				Print["No explicit reference vectors found."];
			];
			
			Return[expr]
		];
		
		RefSimplify[expr, refs, opts]
	]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
