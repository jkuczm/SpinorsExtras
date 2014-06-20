(* ::Package:: *)

BeginPackage["SpinorsExtras`Tests`TestEnvironment`", {"Spinors`"}]


(* ::Section:: *)
(*Public*)


BeginSpinorsTestEnvironment::usage =
"\
BeginSpinorsTestEnvironment[], \
BeginTestEnvironment[\"name`\"] or \
BeginTestEnvironment[\"name`\", {\"need1\", \"need2\", ...}] \
Begins test environment and performs various setup tasks. \
Saves values of Spinors`Private` symbols with info about declared Spinors, \
LVectors, and SMatrices: SpinorList, LVectorList and SMatrixList. \
Returns result of evaluation of BeginTestEnvironment with same arguments."


(*
	Use separate function to declare symbols as Spinors etc. so that symbols 
	passed to this function can be defined *inside* test environment.
*)
SetUpSpinorsTestEnvironment::usage =
"\
SetUpSpinorsTestEnvironment[] \
Declares symbols given in options as Spinors, LVectors, and SMatrices and \
assigns random numerical values to those symbols."


EndSpinorsTestEnvironment::usage =
"\
EndSpinorsTestEnvironment[] \
Ends test environment and performs various teardown tasks. \
Restores values of Spinors`Private` symbols with info about declared Spinors, \
LVectors, and SMatrices: SpinorList, LVectorList and SMatrixList saved by \
call to BeginSpinorsTestEnvironment. \
Returns result of evaluation of EndTestEnvironment."


(* Unprotect all public symbols in this package. *)
Unprotect["`*"];


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Imports*)


Needs["MUnitExtras`TestEnvironment`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables, ProtectSyntax *)
Needs["OptionsUtilities`"] (* DelegateOptions *)


Needs["SpinorsExtras`Package`"]
Needs["SpinorsExtras`Utilities`"] (* DeclarePlusMinusOne *)
Needs["SpinorsExtras`Numerics`"] (* DeclareSpinorRandomMomentum *)
Needs["SpinorsExtras`Ref`"] (* DeclareSpinorMomentum for SpRef *)
Needs["SpinorsExtras`Massive`"] (* DeclareSpinorMomentum for SpAssoc *)


(* ::Subsection:: *)
(*Private symbols*)


$SavedLists = {}


(* ::Subsection:: *)
(*BeginSpinorsTestEnvironment*)


Options[BeginSpinorsTestEnvironment] = Options[BeginTestEnvironment]

SetOptions[
	BeginSpinorsTestEnvironment
	,
	"BasicNeeds" -> 
		Join[
			OptionValue[BeginTestEnvironment, "BasicNeeds"],
			{
				(*
					S@M defines some symbols in Global` context
					(for compatibility with version 5 and 6 of Mathematica?).
				*)
				"Global`"
				,
				"SpinorsExtras`Tests`TestCases`",
				"SpinorsExtras`Tests`TestEnvironment`"
				,
				"Spinors`"
			}
		]
]


BeginSpinorsTestEnvironment[
	contextArg:(_String | Automatic):Automatic,
	needsArg:{___String}:{},
	opts:OptionsPattern[]
] :=
	With[
		{
			result =
				BeginTestEnvironment[
					contextArg,
					needsArg,
					DelegateOptions[
						opts,
						BeginSpinorsTestEnvironment,
						BeginTestEnvironment
					]
				]
		}
		,
		If[result === $Failed,
			Return[$Failed]
		];
		
		AppendTo[
			$SavedLists,
			First[result] -> {
				Spinors`Private`SpinorList,
				Spinors`Private`LVectorList,
				Spinors`Private`SMatrixList
			}
		];
		
		result
	]


(* ::Subsection:: *)
(*SetUpSpinorsTestEnvironment*)


Options[SetUpSpinorsTestEnvironment] = {
	"Spinors" -> {},
	"IntegerSpinors" -> {},
	"LVectors" -> {},
	"SMatrices" -> {}
	,
	"RandomMomentaSpinors" -> {},
	"RandomMomentaLVectors" -> {},
	"SpRefMomentaLVectors" -> {},
	"SpAssocMomentaLVectors" -> {},
	"SpAssocMomentaLVectorRefPairs" -> {},
	"PlusMinusOnes" -> {}
	,
	"PrintDeclarationInfo" -> False
}


SetUpSpinorsTestEnvironment[OptionsPattern[]] :=
	With[
		{
			wrapper =
				If[OptionValue["PrintDeclarationInfo"],
					Identity
				(* else *),
					QuietSpinorPrint
				]
			,
			spinors = OptionValue["Spinors"],
			lvectors = OptionValue["LVectors"],
			randomMomentaSpinors = OptionValue["RandomMomentaSpinors"]
		}
		,
		wrapper[
			DeclareSpinor @@ spinors;
			DeclareLVector @@ lvectors;
			DeclareSMatrix @@ OptionValue["SMatrices"];
			
			DeclareSpinorRandomMomentum[#, 1000]& /@
				If[randomMomentaSpinors === All,
					Join[spinors, OptionValue["IntegerSpinors"]]
				(* else *),
					randomMomentaSpinors
				];
			
			
			Module[
				{randomLV, spRefLV, spAssocLV}
				,
				
				{randomLV, spRefLV, spAssocLV} =
					Replace[
						OptionValue[{
							"RandomMomentaLVectors",
							"SpRefMomentaLVectors",
							"SpAssocMomentaLVectors"
						}]
						,
						All -> lvectors,
						1
					];
			
				DeclareLVectorMomentum[#, RandomReal[{-1000, 1000}, 4]]& /@
					randomLV;
				
				DeclareSpinorMomentum /@ Join[
					SpRef /@ spRefLV,
					SpAssoc /@ spAssocLV,
					SpAssoc @@@ OptionValue["SpAssocMomentaLVectorRefPairs"]
				]
			];
			
			DeclarePlusMinusOne @@ OptionValue["PlusMinusOnes"];
		];
	]


(* ::Subsection:: *)
(*EndSpinorsTestEnvironment*)


EndSpinorsTestEnvironment::noTestEnv = "No spinors test environment to end."


Options[EndSpinorsTestEnvironment] = Options[EndTestEnvironment]


EndSpinorsTestEnvironment[OptionsPattern[]] := (
	If[Length[$SavedLists] < 1,
		Message[EndSpinorsTestEnvironment::noTestEnv];
		Return[$Failed]
	];
	
	With[
		{savedLists = Last[Last[$SavedLists]]}
		,
		Spinors`Private`SpinorList = savedLists[[1]];
		Spinors`Private`LVectorList = savedLists[[2]];
		Spinors`Private`SMatrixList = savedLists[[3]];
	];
	
	EndTestEnvironment[]
)


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


(*
	Protect all public symbols in this package and their syntax,
	except variables with names starting with $.
*)
ProtectContextNonVariables["ProtectFunction" -> ProtectSyntax];
ProtectContextNonVariables[];


EndPackage[]
