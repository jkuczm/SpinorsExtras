(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Ref`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P}
];


SetOptions[
	{
		Test, TestUnchanged,
		TestCaseSpinorQLVectorQTrue, TestCaseSpinorQLVectorQFalse
	}
	,
	ApplyToInput -> SpRef
];


SetOptions[TestCaseSparse,
	AllTestsArgs ->
		List /@ Join[
			Subsets[{x, P, a, 3}, 1] (* no args, 1 arg *),
			{{a, b}} (* 2 args *)
		]
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Spinorizing Integers *)


TestCaseSparse[
	{
		{{3}, SpRef[Sp[3]], TestID -> "Spinorizing Integers: Integer"}
	}
	,
	TestID -> "Spinorizing integers",
	Test -> Test,
	TestDefault -> TestUnchanged
];


(* ::Subsection:: *)
(*SpinorQ, LVectorQ*)


TestCaseSpinorQLVectorQFalse[
	SpRef,
	TestID -> "SpinorQ, LVectorQ: Symbol evaluation"
];


TestCaseSparse[
	{
		{{}, TestID -> "SpinorQ, LVectorQ: no args"},
		{{a, b}, TestID -> "SpinorQ, LVectorQ: 2 args"}
	}
	,
	TestID -> "SpinorQ, LVectorQ",
	Test -> TestCaseSpinorQLVectorQFalse,
	TestDefault -> TestCaseSpinorQLVectorQTrue
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
