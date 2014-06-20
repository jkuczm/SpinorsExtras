(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Pol`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c},
	"LVectors" -> {P, Q, R},
	"PlusMinusOnes" -> {pm}
];

DeclarePossiblePol[pp]


SetOptions[
	{
		Test, TestUnchanged,
		TestCaseSpinorQLVectorQ, TestCaseSpinorQLVectorQFalse
	}
	,
	ApplyToInput -> PolVec
];

arg1List = {x, P, a, 1};
arg2List = {y, Q, b, 4, 1, -1, pm, 0, "S", pp};

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
(*Spinorizing Integers, warrnings*)


TestCaseSparse[
	{
		{
			{a, 0},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, 0]], a, 0
			]},
			TestID -> "Spinorizing Integers: Spinor, 0"
		},
		{
			{a, "S"},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, "S"]], a, "S"
			]},
			TestID -> "Spinorizing Integers: Spinor, \"S\""
		}
		,
		{
			{1, +1},
			PolVec[Sp[1], +1],
			TestID -> "Spinorizing Integers: Integer, +1"
		},
		{
			{1, -1},
			PolVec[Sp[1], -1],
			TestID -> "Spinorizing Integers: Integer, -1"
		},
		{
			{1, pm},
			PolVec[Sp[1], pm],
			TestID -> "Spinorizing Integers: Integer, sym (+/-1)"
		},
		{
			{1, 0},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], 0]], Sp[1], 0
			]},
			TestID -> "Spinorizing Integers: Integer, 0"
		},
		{
			{1, "S"},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], "S"]], Sp[1], "S"
			]},
			TestID -> "Spinorizing Integers: Integer, \"S\""
		},
		{
			{1, pp},
			PolVec[Sp[1], pp],
			TestID -> "Spinorizing Integers: Integer, sym (pol)"
		}
		,
		{
			{P, +1, 1},
			PolVec[P, +1, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, +1, Integer"
		},
		{
			{P, -1, 1},
			PolVec[P, -1, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, -1, Integer"
		},
		{
			{P, pm, 1},
			PolVec[P, pm, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, sym (+/-1), Integer"
		},
		{
			{P, 0, 1},
			PolVec[P, 0, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, 0, Integer"
		},
		{
			{P, "S", 1},
			PolVec[P, "S", Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, \"S\", Integer"
		}
		,
		{
			{P, pp, 1},
			PolVec[P, pp, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, sym (pol), Integer"
		}
		,
		{
			{a, 0, c},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, 0, c]], a, 0
			]},
			TestID -> "Spinorizing Integers: Spinor, 0, Spinor"
		},
		{
			{a, "S", c},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, "S", c]], a, "S"
			]},
			TestID -> "Spinorizing Integers: Spinor, \"S\", Spinor"
		}
		,
		{
			{a, +1, 1},
			PolVec[a, +1, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, +1, Integer"
		},
		{
			{a, -1, 1},
			PolVec[a, -1, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, -1, Integer"
		},
		{
			{a, pm, 1},
			PolVec[a, pm, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, sym (+/-1), Integer"
		},
		{
			{a, 0, 1},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, 0, Sp[1]]], a, 0
			]},
			TestID -> "Spinorizing Integers: Spinor, 0, Integer"
		},
		{
			{a, "S", 1},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, "S", Sp[1]]], a, "S"
			]},
			TestID -> "Spinorizing Integers: Spinor, \"S\", Integer"
		}
		,
		{
			{a, pp, 1},
			PolVec[a, pp, Sp[1]],
			TestID -> "Spinorizing Integers: Spinor, sym (pol), Integer"
		}
		,
		{
			{1, +1, c},
			PolVec[Sp[1], +1, c],
			TestID -> "Spinorizing Integers: Integer, +1, Spinor"
		},
		{
			{1, -1, c},
			PolVec[Sp[1], -1, c],
			TestID -> "Spinorizing Integers: Integer, -1, Spinor"
		},
		{
			{1, pm, c},
			PolVec[Sp[1], pm, c],
			TestID -> "Spinorizing Integers: Integer, sym (+/-1), Spinor"
		},
		{
			{1, 0, c},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], 0, c]], Sp[1], 0
			]},
			TestID -> "Spinorizing Integers: Integer, 0, Spinor"
		},
		{
			{1, "S", c},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], "S", c]],
				Sp[1], "S"
			]},
			TestID -> "Spinorizing Integers: Integer, \"S\", Spinor"
		},
		{
			{1, pp, c},
			PolVec[Sp[1], pp, c],
			TestID -> "Spinorizing Integers: Integer, sym (pol), Spinor"
		}
		,
		{
			{1, +1, 1},
			PolVec[Sp[1], +1, Sp[1]],
			TestID -> "Spinorizing Integers: Integer, +1, Integer"
		},
		{
			{1, -1, 1},
			PolVec[Sp[1], -1, Sp[1]],
			TestID -> "Spinorizing Integers: Integer, -1, Integer"
		},
		{
			{1, pm, 1},
			PolVec[Sp[1], pm, Sp[1]],
			TestID -> "Spinorizing Integers: Integer, sym (+/-1), Integer"
		},
		{
			{1, 0, 1},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], 0, Sp[1]]],
				Sp[1], 0
			]},
			TestID -> "Spinorizing Integers: Integer, 0, Integer"
		},
		{
			{1, "S", 1},
			$Failed,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], "S", Sp[1]]],
				Sp[1], "S"
			]},
			TestID -> "Spinorizing Integers: Integer, \"S\", Integer"
		},
		{
			{1, pp, 1},
			PolVec[Sp[1], pp, Sp[1]],
			TestID -> "Spinorizing Integers: Integer, sym (pol), Integer"
		}
	}
	,
	TestID -> "Spinorizing Integers",
	Test -> Test,
	TestDefault -> TestUnchanged
];


(* ::Subsection:: *)
(*SpinorQ, LVectorQ*)


TestCaseSpinorQLVectorQFalse[
	PolVec,
	TestID -> "SpinorQ, LVectorQ: Symbol evaluation"
];


TestCaseSparse[
	{
		{
			{P, +1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, +1"
		},
		{
			{P, -1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, -1"
		},
		{
			{P, pm},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1)"
		},
		{
			{P, 0},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, 0"
		},
		{
			{P, "S"},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, \"S\""
		},
		{
			{P, pp},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (pol)"
		}
		,
		{
			{a, +1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, +1"
		},
		{
			{a, -1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, -1"
		},
		{
			{a, pm},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1)"
		},
		{
			{a, 0},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, 0]], a, 0
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, 0"
		},
		{
			{a, "S"},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, "S"]], a, "S"
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, \"S\""
		},
		{
			{a, pp},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (pol)"
		}
		,
		{
			{1, +1},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, +1"
		},
		{
			{1, -1},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, -1"
		},
		{
			{1, pm},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, sym (+/-1)"
		},
		{
			{1, 0},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], 0]], Sp[1], 0
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, 0"
		},
		{
			{1, "S"},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], "S"]], Sp[1], "S"
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, \"S\""
		},
		{
			{1, pp},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Integer, sym (pol)"
		}
		,
		{
			{P, +1, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, +1, Spinor"
		},
		{
			{P, -1, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, -1, Spinor"
		},
		{
			{P, pm, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1), Spinor"
		},
		{
			{P, 0, c},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, 0, Spinor"
		},
		{
			{P, "S", c},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, \"S\", Spinor"
		},
		{
			{P, pp, c},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (pol), Spinor"
		}
		,
		{
			{P, +1, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, +1, Integer"
		},
		{
			{P, -1, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, -1, Integer"
		},
		{
			{P, pm, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1), Integer"
		},
		{
			{P, 0, 1},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, 0, Integer"
		},
		{
			{P, "S", 1},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, \"S\", Integer"
		},
		{
			{P, pp, 1},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (pol), Integer"
		}
		,
		{
			{a, +1, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, +1, Spinor"
		},
		{
			{a, -1, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, -1, Spinor"
		},
		{
			{a, pm, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1), Spinor"
		},
		{
			{a, 0, c},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, 0, c]], a, 0
			]},
			TestID -> "SpinorQ, LVectorQ: Spinor, 0, Spinor"
		},
		{
			{a, "S", c},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, "S", c]], a, "S"
			]},
			TestID -> "SpinorQ, LVectorQ: Spinor, \"S\", Spinor"
		},
		{
			{a, pp, c},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (pol), Spinor"
		}
		,
		{
			{a, +1, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, +1, Integer"
		},
		{
			{a, -1, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, -1, Integer"
		},
		{
			{a, pm, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (+/-1), Integer"
		},
		{
			{a, 0, 1},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, 0, Sp[1]]], a, 0
			]},
			TestID -> "SpinorQ, LVectorQ: Spinor, 0, Integer"
		},
		{
			{a, "S", 1},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[a, "S", Sp[1]]], a, "S"
			]},
			TestID -> "SpinorQ, LVectorQ: Spinor, \"S\", Integer"
		},
		{
			{a, pp, 1},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Spinor, sym (pol), Integer"
		}
		,
		{
			{1, +1, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, +1, Spinor"
		},
		{
			{1, -1, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, -1, Spinor"
		},
		{
			{1, pm, c},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, sym (+/-1), Spinor"
		},
		{
			{1, 0, c},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], 0, c]], Sp[1], 0
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, 0, Spinor"
		},
		{
			{1, "S", c},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], "S", c]],
				Sp[1], "S"
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, \"S\", Spinor"
		},
		{
			{1, pp, c},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Integer, sym (pol), Spinor"
		}
		,
		{
			{1, +1, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, +1, Integer"
		},
		{
			{1, -1, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, -1, Integer"
		},
		{
			{1, pm, 1},
			True,
			TestID -> "SpinorQ, LVectorQ: Integer, sym (+/-1), Integer"
		},
		{
			{1, 0, 1},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], 0, Sp[1]]],
				Sp[1], 0
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, 0, Integer"
		},
		{
			{1, "S", 1},
			False,
			{HoldForm @ Message[
				PolVec::forbidden0Pol, HoldForm[PolVec[Sp[1], "S", Sp[1]]],
				Sp[1], "S"
			]},
			TestID -> "SpinorQ, LVectorQ: Integer, \"S\", Integer"
		},
		{
			{1, pp, 1},
			{False, True},
			TestID -> "SpinorQ, LVectorQ: Integer, sym (pol), Integer"
		}
	}
	,
	TestID -> "SpinorQ, LVectorQ",
	Test -> TestCaseSpinorQLVectorQ,
	TestDefault -> TestCaseSpinorQLVectorQFalse
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
