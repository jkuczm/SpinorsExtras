(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Numerics`"}];
(*
	Don't set up spinors since we're testing DeclareSpinorRandomMomentum which
	is used in set up function
*)


SetOptions[TestZero,
	EquivalenceFunction -> Equal,
	InputWrapper-> (SetAccuracy[#, 8]&)
]


(* ::Section:: *)
(*Tests*)


TestNull[
	DeclareSpinorRandomMomentum[testSpinor, 100],
	TestID -> "real: DeclareSpinorRandomMomentum evaluation"
];
TestMatch[
	Num4V[testSpinor],
	{Repeated[_Real, {4}]},
	TestID -> "real: is four mom set"
];
TestZero[
	MP2[Num4V[testSpinor]],
	TestID -> "real: masslessness"
];


TestNull[
	DeclareSpinorRandomMomentum[testSpinor, 100, "Complex" -> True],
	TestID -> "complex: DeclareSpinorRandomMomentum evaluation"
];
TestMatch[
	Num4V[testSpinor],
	{Repeated[_Complex, {4}]},
	TestID -> "complex: is four mom set"
];
TestZero[
	MP2[Num4V[testSpinor]],
	TestID -> "complex: masslessness"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
