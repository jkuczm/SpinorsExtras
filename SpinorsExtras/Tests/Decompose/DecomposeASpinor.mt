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


SetOptions[{TestCaseSymbolicNumeric, TestSubexpression}, {
	ApplyToInput -> DecomposeASpinor
}];

TestSubexpression[
	{Spbb[a, b], b -> {c, d}},
	TestID -> "Spbb"
];
TestCaseSymbolicNumeric[
	{Spaa[a, b], b -> {c, d}},
	Spaa[d, b]/Spaa[d, c] Spaa[a, c] + Spaa[c, b]/Spaa[c, d] Spaa[a, d],
	TestID -> "Spaa"
];
TestCaseSymbolicNumeric[
	{Spaa[1, 2], 2 -> {3, 4}},
	Spaa[4, 2]/Spaa[4, 3] Spaa[1, 3] + Spaa[3, 2]/Spaa[3, 4] Spaa[1, 4],
	TestID -> "Spaa integer args"
];

TestSubexpression[
	{Spaa[a, b], b -> {c, c}},
	{HoldForm[Message[DecomposeASpinor::wrongBasis, c, c]]},
	TestID -> "wrong basis: same spinors"
];

Block[
	{SpinorsExtras`Proportional`Private`$ProportionalASpinorList = {}}
	,
	DeclareASpinorProportional[d, e];
	TestSubexpression[
		{Spaa[a, b], b -> {d, e}},
		{HoldForm[Message[DecomposeASpinor::wrongBasis, d, e]]},
		TestID -> "wrong basis: proportional spinors"
	];
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
