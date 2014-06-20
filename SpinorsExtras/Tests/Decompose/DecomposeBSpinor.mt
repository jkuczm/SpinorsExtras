(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Decompose`",
	"SpinorsExtras`Proportional`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c, d, e},
	"IntegerSpinors" -> Range[4],
	"RandomMomentaSpinors" -> All
];


(* ::Section:: *)
(*Tests*)


SetOptions[{TestCaseSymbolicNumeric, TestSubexpression},
	ApplyToInput -> DecomposeBSpinor
];

TestSubexpression[
	{Spaa[a, b], b -> {c, d}},
	TestID -> "Spaa"
];
TestCaseSymbolicNumeric[
	{Spbb[a, b], b -> {c, d}},
	Spbb[d, b]/Spbb[d, c] Spbb[a, c] + Spbb[c, b]/Spbb[c, d] Spbb[a, d],
	TestID -> "Spbb"
];
TestCaseSymbolicNumeric[
	{Spbb[1, 2], 2 -> {3, 4}},
	Spbb[4, 2]/Spbb[4, 3] Spbb[1, 3] + Spbb[3, 2]/Spbb[3, 4] Spbb[1, 4],
	TestID -> "Spbb integer args"
];

TestSubexpression[
	{Spbb[a, b], b -> {c, c}},
	{HoldForm[Message[DecomposeBSpinor::wrongBasis, c, c]]},
	TestID -> "wrong basis: same spinors"
];

Block[
	{SpinorsExtras`Proportional`Private`$ProportionalBSpinorList = {}}
	,
	DeclareBSpinorProportional[d, e];
	TestSubexpression[
		{Spbb[a, b], b -> {d, e}},
		{HoldForm[Message[DecomposeBSpinor::wrongBasis, d, e]]},
		TestID -> "wrong basis: proportional spinors"
	];
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
