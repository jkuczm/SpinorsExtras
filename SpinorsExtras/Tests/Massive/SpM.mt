(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Massive`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c, d},
	"LVectors" -> {P, Q, R},
	"PlusMinusOnes" -> {pm}
];


SetOptions[
	{
		Test, TestUnchanged,
		TestCaseSpinorQLVectorQTrue, TestCaseSpinorQLVectorQFalse
	}
	,
	ApplyToInput -> SpM
];

arg1List = {x, P, a, 1};
arg2List = {y, Q, b, 4, 1, -1, pm};

SetOptions[TestCaseSparse,
	AllTestsArgs ->
		List /@ Join[
			Subsets[arg1List, 1] (* no args, 1 arg *),
			Tuples[{arg1List, arg2List}] (* 2 args *),
			Tuples[{arg1List, arg2List, {z, R, c, 1}}] (* 3 args *),
			{{a, 1, c, d}} (* 4 args *)
		]
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Simplifications and spinorizing Integers *)


TestCaseSparse[
	{
		{{a, 1}, a, TestID -> "Simplifications: Spinor, +1"},
		{{a, -1}, a, TestID -> "Simplifications: Spinor, -1"},
		{{a, pm}, a, TestID -> "Simplifications: Spinor, sym (+/-1)"}
		,
		{{1, 1}, Sp[1], TestID -> "Simplifications: Integer, +1"},
		{{1, -1}, Sp[1], TestID -> "Simplifications: Integer, -1"},
		{{1, pm}, Sp[1], TestID -> "Simplifications: SInteger sym (+/-1)"}
		,
		{{P, 1, 1}, SpM[P, 1, Sp[1]],
			TestID -> "Simplifications: Spinor, +1, Spinor"},
		{{P, -1, 1}, SpM[P, -1, Sp[1]],
			TestID -> "Simplifications: Spinor, -1, Spinor"},
		{{P, pm, 1}, SpM[P, pm, Sp[1]],
			TestID -> "Simplifications: Spinor, sym (+/-1), Spinor"}
		,
		{{a, 1, c}, a, TestID -> "Simplifications: Spinor, +1, Spinor"},
		{{a, -1, c}, a, TestID -> "Simplifications: Spinor, -1, Spinor"},
		{{a, pm, c}, a,
			TestID -> "Simplifications: Spinor, sym (+/-1), Spinor"}
		,
		{{a, 1, 1}, a, TestID -> "Simplifications: Spinor, +1, Integer"},
		{{a, -1, 1}, a, TestID -> "Simplifications: Spinor, -1, Integer"},
		{{a, pm, 1}, a,
			TestID -> "Simplifications: Spinor, sym (+/-1), Integer"}
		,
		{{1, 1, c}, Sp[1], TestID -> "Simplifications: Integer, +1, Spinor"},
		{{1, -1, c}, Sp[1], TestID -> "Simplifications: Integer, -1, Spinor"},
		{{1, pm, c}, Sp[1],
			TestID -> "Simplifications: SInteger sym (+/-1), Spinor"}
		,
		{{1, 1, 1}, Sp[1], TestID -> "Simplifications: Integer, +1, Integer"},
		{{1, -1, 1}, Sp[1], TestID -> "Simplifications: Integer, -1, Integer"},
		{{1, pm, 1}, Sp[1],
			TestID -> "Simplifications: SInteger sym (+/-1), Integer"}
	}
	,
	TestID -> "Simplifications",
	Test -> Test,
	TestDefault -> TestUnchanged
];


(* ::Subsection:: *)
(*SpinorQ, LVectorQ*)


TestCaseSpinorQLVectorQFalse[
	SpM,
	TestID -> "SpinorQ, LVectorQ: Symbol evaluation"
];


TestCaseSparse[
	{
		{{a, 1}, TestID -> "SpinorQ, LVectorQ: Spinor, +1"},
		{{a, -1}, TestID -> "SpinorQ, LVectorQ: Spinor, -1"},
		{{a, pm}, TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1)"}
		,
		{{1, 1}, TestID -> "SpinorQ, LVectorQ: Integer, +1"},
		{{1, -1}, TestID -> "SpinorQ, LVectorQ: Integer, -1"},
		{{1, pm}, TestID -> "SpinorQ, LVectorQ: SInteger sym (+/-1)"}
		,
		{{a, 1, c}, TestID -> "SpinorQ, LVectorQ: Spinor, +1, Spinor"},
		{{a, -1, c}, TestID -> "SpinorQ, LVectorQ: Spinor, -1, Spinor"},
		{{a, pm, c}, TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1), Spinor"}
		,
		{{a, 1, 1}, TestID -> "SpinorQ, LVectorQ: Spinor, +1, Integer"},
		{{a, -1, 1}, TestID -> "SpinorQ, LVectorQ: Spinor, -1, Integer"},
		{{a, pm, 1},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1), Integer"}
		,
		{{1, 1, c}, TestID -> "SpinorQ, LVectorQ: Integer, +1, Spinor"},
		{{1, -1, c}, TestID -> "SpinorQ, LVectorQ: Integer, -1, Spinor"},
		{{1, pm, c},
			TestID -> "SpinorQ, LVectorQ: SInteger sym (+/-1), Spinor"}
		,
		{{1, 1, 1}, TestID -> "SpinorQ, LVectorQ: Integer, +1, Integer"},
		{{1, -1, 1}, TestID -> "SpinorQ, LVectorQ: Integer, -1, Integer"},
		{{1, pm, 1},
			TestID -> "SpinorQ, LVectorQ: SInteger sym (+/-1), Integer"}
	}
	,
	TestID -> "SpinorQ, LVectorQ",
	Test -> TestCaseSpinorQLVectorQTrue,
	TestDefault -> TestCaseSpinorQLVectorQFalse
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
