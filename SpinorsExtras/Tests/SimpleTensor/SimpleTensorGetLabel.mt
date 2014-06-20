(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`SimpleTensor`",
	"MUnitExtras`Package`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"RandomMomentaSpinors" -> All
];


AssignTestFeatures[TestCase, Test];

TestCase[expr_, expectedB_, expectedA_, opts:OptionsPattern[]] :=
	TestCaseEnvironment[
		{opts, Options[TestCase]}
		,
		Module[
			{bLabel, aLabel}
			,
			
			(* compare extracted label of B spinor with expected *)
			Test[
				bLabel = SimpleTensorGetBLabel[expr],
				expectedB,
				TestFailureMessage -> "bLabel same as expected"
			];
			
			(* compare extracted label of A spinor with expected *)
			Test[
				aLabel = SimpleTensorGetALabel[expr],
				expectedA,
				TestFailureMessage -> "aLabel same as expected"
			];
			
			(* compare symbolicaly reconstructed tensor with original tensor *)
			Test[
				SmBA[bLabel, aLabel],
				expr,
				TestFailureMessage -> "reconstructed equal to expr symbolically"
			];
		]
		,
		"CommonOptionsFor" -> {Test}
	];


(* ::Section:: *)
(*Tests*)


TestCase[
	Sm[a],
	a,
	a,
	TestID -> "single Sm"
];
TestCase[
	SmBA[b, a],
	b,
	a,
	TestID -> "single SmBA"
];
TestCase[
	5 Sm[a],
	5 a,
	a,
	TestID -> "multiplied Sm"
];
TestCase[
	X SmBA[b, a],
	X b,
	a,
	TestID -> "multiplied SmBA"
];
TestCase[
	Sm[a] + SmBA[a, b],
	a,
	a + b,
	TestID -> "Sm and SmBA same B"
];
TestCase[
	Sm[a] + SmBA[b, a],
	a + b,
	a,
	TestID -> "Sm and SmBA same A"
];
TestCase[
	Sm[a] 3.2 + SmBA[a, b],
	a,
	a 3.2 + b,
	TestID -> "Sm multiplied and SmBA same B"
];
TestCase[
	8 Sm[a] + SmBA[b, a],
	8a + b,
	a,
	TestID -> "Sm multiplied and SmBA same A"
];
TestCase[
	Sm[a] + SmBA[a, b] 3.2,
	a,
	a + b 3.2,
	TestID -> "Sm and SmBA multiplied same B"
];
TestCase[
	Sm[a] + 8 SmBA[b, a],
	a + 8b,
	a,
	TestID -> "Sm and SmBA multiplied same A"
];
TestCase[
	9 Sm[a] + SmBA[a, b] 3.2,
	a,
	9a + b 3.2,
	TestID -> "Sm multiplied and SmBA multiplied same B"
];
TestCase[
	5.4 Sm[a] + 8 SmBA[b, a],
	5.4a + 8 b,
	a,
	TestID -> "Sm multiplied and SmBA multiplied same A"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
