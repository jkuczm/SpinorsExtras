(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Massive`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c, d},
	"LVectors" -> {P, Q, R}
];


SetOptions[
	{
		Test, TestUnchanged,
		TestCaseSpinorQLVectorQTrue, TestCaseSpinorQLVectorQFalse
	}
	,
	ApplyToInput -> SpAssoc
];


arg1List = {x, P, a, 3};

SetOptions[TestCaseSparse,
	AllTestsArgs ->
		List /@ Join[
			Subsets[arg1List, 1] (* no args, 1 arg *),
			Tuples[{arg1List, {y, Q, b, {b, c}, 4}}] (* 2 args *),
			{{a, b, c}} (* 3 args *)
		]
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Simplifications for Spinor first argument, spinorizing Integers *)


TestCaseSparse[
	{
		{{a}, a, TestID -> "Simplifications: Spinor"},
		{{3}, Sp[3], TestID -> "Simplifications: Integer"}
		,
		{{P, 4}, SpAssoc[P, Sp[4]],
			TestID -> "Simplifications: Spinor, LVector"}
		,
		{{a, Q}, a, TestID -> "Simplifications: Spinor, LVector"},
		{{a, b}, a, TestID -> "Simplifications: Spinor, Spinor"},
		{{a, 4}, a, TestID -> "Simplifications: Spinor, Integer"}
		,
		{{3, Q}, Sp[3], TestID -> "Simplifications: Integer, LVector"},
		{{3, b}, Sp[3], TestID -> "Simplifications: Integer, Spinor"},
		{{3, 4}, Sp[3], TestID -> "Simplifications: Integer, Integer"}
	}
	,
	TestID -> "Simplifications",
	Test -> Test,
	TestDefault -> TestUnchanged
];


(* ::Subsection:: *)
(*SpinorQ, LVectorQ*)


TestCaseSpinorQLVectorQFalse[
	SpAssoc,
	TestID -> "SpinorQ, LVectorQ: Symbol evaluation"
];


TestCaseSparse[
	{
		{{P}, TestID -> "SpinorQ, LVectorQ: LVector"},
		{{a}, TestID -> "SpinorQ, LVectorQ: Spinor"},
		{{3}, TestID -> "SpinorQ, LVectorQ: Integer"}
		,
		{{P, Q}, TestID -> "SpinorQ, LVectorQ: LVector, LVector"},
		{{P, b}, TestID -> "SpinorQ, LVectorQ: LVector, Spinor"},
		{{P, 4}, TestID -> "SpinorQ, LVectorQ: LVector, Integer"}
		,
		{{a, Q}, TestID -> "SpinorQ, LVectorQ: Spinor, LVector"},
		{{a, b}, TestID -> "SpinorQ, LVectorQ: Spinor, Spinor"},
		{{a, 4}, TestID -> "SpinorQ, LVectorQ: Spinor, Integer"}
		,
		{{3, Q}, TestID -> "SpinorQ, LVectorQ: Integer, LVector"},
		{{3, b}, TestID -> "SpinorQ, LVectorQ: Integer, Spinor"},
		{{3, 4}, TestID -> "SpinorQ, LVectorQ: Integer, Integer"}
	}
	,
	TestID -> "SpinorQ, LVectorQ",
	Test -> TestCaseSpinorQLVectorQTrue,
	TestDefault -> TestCaseSpinorQLVectorQFalse
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
