(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Massive`",
	"MUnitExtras`Package`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {sp1, sp2, ref1, ref2},
	"LVectors" -> {P, Q},
	"SMatrices" -> {sMatr[_Integer]}
];


(* Automatic simplifications for massive <P|...], ... products. *)
AssignTestFeatures[TestCaseSpabSpbaOrders]

TestCaseSpabSpbaOrders[
	testFunction_,
	label1_,
	label2_,
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts}
		,
		testFunction[
			Spab[label1, label2],
			TestFailureMessage -> "Spab"
		];
		testFunction[
			Spab[label2, label1],
			TestFailureMessage -> "Spab reversed"
		];
		
		testFunction[
			Spba[label1, label2],
			TestFailureMessage -> "Spba"
		];
		testFunction[
			Spba[label2, label1],
			TestFailureMessage -> "Spba reversed"
		];
		,
		"CommonOptionsFor" -> {testFunction}
	];


AssignTestFeatures[TestCaseTripleSpabSpbaOrders]

TestCaseTripleSpabSpbaOrders[
	{testFunction1_, testFunction2_, testFunction3_},
	testSp_,
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts}
		,
		TestCaseSpabSpbaOrders[
			testFunction1,
			SpM[P, uv1, SpAssoc[Q, P]],
			testSp,
			TestFailureMessage -> "P, ref1=Q^P"
		];
		TestCaseSpabSpbaOrders[
			testFunction2,
			SpM[P, uv1, ref1],
			testSp,
			TestFailureMessage -> "P, arbitrary ref1"
		];
		TestCaseSpabSpbaOrders[
			testFunction3,
			SpM[P, uv1],
			testSp,
			TestFailureMessage -> "P, implicit ref1"
		];
		,
		"CommonOptionsFor" -> {testFunction1, testFunction2, testFunction3}
	];


(* ::Section:: *)
(*Tests*)

TestCaseTripleSpabSpbaOrders[
	{TestZero, TestNonZero, TestNonZero},
	SpM[Q, uv2, SpAssoc[P, Q]],
	TestID -> "Q, ref=P^Q"
];
TestCaseTripleSpabSpbaOrders[
	{TestNonZero, TestNonZero, TestNonZero},
	SpM[Q, uv2, ref1],
	TestID -> "Q, ref=P^Q"
];
TestCaseTripleSpabSpbaOrders[
	{TestZero, TestNonZero, TestNonZero},
	SpAssoc[Q, P],
	TestID -> "Q^P"
];
TestCaseTripleSpabSpbaOrders[
	{TestNonZero, TestNonZero, TestNonZero},
	SpAssoc[P, Q],
	TestID -> "P^Q"
];
TestCaseTripleSpabSpbaOrders[
	{TestNonZero, TestZero, TestNonZero},
	ref1,
	TestID -> "ref1"
];
TestCaseTripleSpabSpbaOrders[
	{TestNonZero, TestNonZero, TestNonZero},
	ref2,
	TestID -> "ref2"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
