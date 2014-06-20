(* ::Package:: *)

BeginPackage["SpinorsExtras`Proportional`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


DeclareBSpinorProportional::usage =
"\
DeclareBSpinorProportional[x, y, ...] \
sets B spinors |x], |y], ... to be treated as proportional."

DeclareASpinorProportional::usage =
"\
DeclareASpinorProportional[x, y, ...] \
sets A spinors |x>, |y>, ... to be treated as proportional."

DeclareLVectorProportional::usage =
"\
DeclareLVectorProportional[x, y, ...] \
sets B and A spinors labeled by x, y, ... to be treated as proportional, \
which means that corresponding four-vectors are proportional."


BSpinorProportionalQ::usage =
"\
BSpinorProportionalQ[x, y, ...] or BSpinorProportionalQ[{x, y, ...}] \
returns True if |x], |y], ... B spinors are proportional. Returns False \
otherwise."

ASpinorProportionalQ::usage =
"\
ASpinorProportionalQ[x, y, ...] or ASpinorProportionalQ[{x, y, ...}] \
returns True if |x>, |y>, ... A spinors are proportional. Returns False \
otherwise."

LVectorProportionalQ::usage =
"\
LVectorProportionalQ[x, y, ...] or LVectorProportionalQ[{x, y, ...}] \
returns True if x, y, ... label four-vectors that are proportional. Returns \
False otherwise. Four-vectors are proportional iff B spinors associated with \
them are proportional to each other and so are A spinors."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


Needs["SpinorsExtras`Package`"]


(* ::Subsection:: *)
(*Declare...Proportional*)


SetAttributes[DeclareSpinorProportionalInternal, HoldFirst]

DeclareSpinorProportionalInternal[propList_, spArgs_] :=
	Module[
		{
			intersected = {spArgs},
			notIntersected = {}
		}
		,
		
		If[Length[Intersection[spArgs, #]] > 0,
			AppendTo[intersected, #];
		(* else *),
			AppendTo[notIntersected, #];
		]& /@ propList;
		
		intersected = Union @@ intersected;
		propList = Union[Append[notIntersected, intersected]];
		
		intersected
	]


DeclareLVectorProportionalInternal[_, spArgs_] := (
	DeclareSpinorProportionalInternal[$ProportionalBSpinorList, spArgs];
	DeclareSpinorProportionalInternal[$ProportionalASpinorList, spArgs];
	
	spArgs
)


$ProportionalBSpinorList = {}

$ProportionalASpinorList = {}


SetAttributes[Factory, HoldAll]

Factory[tag_Symbol, internalFunction_Symbol, propList_Symbol, type_String] := (
	tag[args___?SpinorInterpretableQ] :=
		Module[
			{
				spArgs =
					Union @ Flatten[
						Alternatives @@ SpinorizeIntegerList[args]
					]
				,
				allProportional
			}
			,
			If[Length[spArgs] < 2,
				Message[tag::notEnoughSpinors, tag, List @@ spArgs, 2];
				Return[$Failed]
			];
			
			allProportional = internalFunction[propList, spArgs];
			
			Spinors`Private`PRINT[
				type <> ": " <> ToString[List @@ allProportional] <>
					" are now considered proportional."
			];
		];

	tag[args__] := (
		Message[
			tag::nonSpinor
			,
			Position[
				{args},
				_?(!SpinorInterpretableQ[#]&),
				{1},
				Heads -> False
			]
			,
			HoldForm[tag[args]]
		];
		$Failed
	);
)

Factory[
	DeclareBSpinorProportional,
	DeclareSpinorProportionalInternal,
	$ProportionalBSpinorList,
	"B spinors"
]
Factory[
	DeclareASpinorProportional,
	DeclareSpinorProportionalInternal,
	$ProportionalASpinorList,
	"A spinors"
]
Factory[
	DeclareLVectorProportional,
	DeclareLVectorProportionalInternal,
	Null,
	"LVectors"
]


(* ::Subsection:: *)
(*...ProportionalQ*)


Factory[tag_Symbol] := (
	(* sp pattern name is used because we want to macth only same arguments. *)
	tag[l_List] := tag @@ l;
	
	tag[Repeated[sp_?SpinorInterpretableQ, {2, Infinity}]] := True;
	
	tag[
		args:Repeated[_?SpinorInterpretableQ, {2, Infinity}] /;
			MemberQ[{args}, _?(!SpinorQ[#]&)]
	] :=
		tag @@ SpinorizeIntegerList[args]
)

Factory[BSpinorProportionalQ]
Factory[ASpinorProportionalQ]

Factory[LVectorProportionalQ]


Factory[tag_Symbol, propList_] := (
	tag[args:Repeated[_?SpinorQ, {2, Infinity}]] :=
		Select[propList, MatchQ[{args}, {#..}]&, 1] =!= {}
)

Factory[BSpinorProportionalQ, $ProportionalBSpinorList]
Factory[ASpinorProportionalQ, $ProportionalASpinorList]

LVectorProportionalQ[args:Repeated[_?SpinorQ, {2, Infinity}]] :=
	BSpinorProportionalQ[args] && ASpinorProportionalQ[args]


(*
	Factory functions where needed only for creating definitions of other
	objects, they're not needed after package initialization.
*)
Remove[Factory]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
