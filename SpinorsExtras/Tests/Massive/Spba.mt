(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Massive`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P, Q},
	"SMatrices" -> {SmA, SmB, SmC},
	"PlusMinusOnes" -> {uv1, uv2}
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*0 smatrices*)


TestZero[
	Spba[a, a],
	TestID -> "0 smatrices: 2 massless same"
];
TestZero[
	Spba[a, b],
	TestID -> "0 smatrices: 2 massless different: ordered"
];
TestZero[
	Spba[b, a],
	TestID -> "0 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spba[x1 a, x2 b],
	TestID -> "0 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spba[a, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], a]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spba[SpM[P, uv1], a],
	HoldPattern[Spab[a, SpM[P, uv1]]],
	TestID -> "0 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spba[x1 a, x2 SpM[P, uv1]],
	x1 x2 Spba[a, SpM[P, uv1]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestMatch[
	Spba[SpM[P, uv1], SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SpM[P, uv1]]],
	TestID -> "0 smatrices: 2 massive same"
];
TestMatch[
	Spba[SpM[P, uv1], SpM[Q, uv2]],
	HoldPattern[Spab[SpM[Q, uv2], SpM[P, uv1]]],
	TestID -> "0 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spba[SpM[Q, uv2], SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SpM[Q, uv2]]],
	TestID -> "0 smatrices: 2 massive different: reversed order"
];
Test[
	Spba[x1 SpM[P, uv1], x2 SpM[Q, uv2]],
	x1 x2 Spba[SpM[P, uv1], SpM[Q, uv2]],
	TestID -> "0 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*1 smatrix*)


TestMatch[
	Spba[a, SmA, a],
	HoldPattern[Spab[a, SmA, a]],
	TestID -> "1 smatrices: 2 massless same"
];
TestMatch[
	Spba[a, SmA, b],
	HoldPattern[Spab[b, SmA, a]],
	TestID -> "1 smatrices: 2 massless different: ordered"
];
TestMatch[
	Spba[b, SmA, a],
	HoldPattern[Spab[a, SmA, b]],
	TestID -> "1 smatrices: 2 massless different: reversed order"
];
Test[
	Spba[x1 a, x2 SmA, x3 b],
	x1 x2 x3 Spba[a, SmA, b],
	TestID -> "1 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spba[a, SmA, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmA, a]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spba[SpM[P, uv1], SmA, a],
	HoldPattern[Spab[a, SmA, SpM[P, uv1]]],
	TestID -> "1 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spba[x1 a, x2 SmA, x3 SpM[P, uv1]],
	x1 x2 x3 Spba[a, SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestMatch[
	Spba[SpM[P, uv1], SmA, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmA, SpM[P, uv1]]],
	TestID -> "1 smatrices: 2 massive same"
];
TestMatch[
	Spba[SpM[P, uv1], SmA, SpM[Q, uv2]],
	HoldPattern[Spab[SpM[Q, uv2], SmA, SpM[P, uv1]]],
	TestID -> "1 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spba[SpM[Q, uv2], SmA, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmA, SpM[Q, uv2]]],
	TestID -> "1 smatrices: 2 massive different: reversed order"
];
Test[
	Spba[x1 SpM[P, uv1], x2 SmA, x3 SpM[Q, uv2]],
	x1 x2 x3 Spba[SpM[P, uv1], SmA, SpM[Q, uv2]],
	TestID -> "1 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*2 smatrices*)


TestZero[
	Spba[a, SmA, SmB, a],
	TestID -> "2 smatrices: 2 massless same: ordered"
];
TestZero[
	Spba[a, SmB, SmA, a],
	TestID -> "2 smatrices: 2 massless same: reversed order"
];
TestZero[
	Spba[a, SmA, SmB, b],
	TestID -> "2 smatrices: 2 massless different: ordered"
];
TestZero[
	Spba[b, SmA, SmB, a],
	TestID -> "2 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spba[x1 a, x2 SmA, x3 SmB, x4 b],
	TestID -> "2 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spba[a, SmA, SmB, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmB, SmA, a]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spba[SpM[P, uv1], SmA, SmB, a],
	HoldPattern[Spab[a, SmB, SmA, SpM[P, uv1]]],
	TestID -> "2 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spba[x1 a, x2 SmA, x3 SmB, x4 SpM[P, uv1]],
	x1 x2 x3 x4 Spba[a, SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestMatch[
	Spba[SpM[P, uv1], SmA, SmB, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmB, SmA, SpM[P, uv1]]],
	TestID -> "2 smatrices: 2 massive same: ordered"
];
TestMatch[
	Spba[SpM[P, uv1], SmB, SmA, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmA, SmB, SpM[P, uv1]]],
	TestID -> "2 smatrices: 2 massive same: reversed order"
];
TestMatch[
	Spba[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	HoldPattern[Spab[SpM[Q, uv2], SmB, SmA, SpM[P, uv1]]],
	TestID -> "2 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spba[SpM[Q, uv2], SmA, SmB, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmB, SmA, SpM[Q, uv2]]],
	TestID -> "2 smatrices: 2 massive different: reversed order"
];
Test[
	Spba[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SpM[Q, uv2]],
	x1 x2 x3 x4 Spba[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	TestID -> "2 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*3 smatrices*)


TestMatch[
	Spba[a, SmA, SmB, SmC, a],
	HoldPattern[Spab[a, SmC, SmB, SmA, a]],
	TestID -> "3 smatrices: 2 massless same: ordered"
];
TestMatch[
	Spba[a, SmC, SmB, SmA, a],
	HoldPattern[Spab[a, SmA, SmB, SmC, a]],
	TestID -> "3 smatrices: 2 massless same: reversed order"
];
TestMatch[
	Spba[a, SmA, SmB, SmC, b],
	HoldPattern[Spab[b, SmC, SmB, SmA, a]],
	TestID -> "3 smatrices: 2 massless different: ordered"
];
TestMatch[
	Spba[b, SmA, SmB, SmC, a],
	HoldPattern[Spab[a, SmC, SmB, SmA, b]],
	TestID -> "3 smatrices: 2 massless different: reversed order"
];
Test[
	Spba[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 b],
	x1 x2 x3 x4 x5 Spba[a, SmA, SmB, SmC, b],
	TestID -> "3 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spba[a, SmA, SmB, SmC, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmC, SmB, SmA, a]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spba[SpM[P, uv1], SmA, SmB, SmC, a],
	HoldPattern[Spab[a, SmC, SmB, SmA, SpM[P, uv1]]],
	TestID -> "3 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spba[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 SpM[P, uv1]],
	x1 x2 x3 x4 x5 Spba[a, SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestMatch[
	Spba[SpM[P, uv1], SmA, SmB, SmC, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmC, SmB, SmA, SpM[P, uv1]]],
	TestID -> "3 smatrices: 2 massive same: ordered"
];
TestMatch[
	Spba[SpM[P, uv1], SmC, SmB, SmA, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmA, SmB, SmC, SpM[P, uv1]]],
	TestID -> "3 smatrices: 2 massive same: reversed order"
];
TestMatch[
	Spba[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	HoldPattern[Spab[SpM[Q, uv2], SmC, SmB, SmA, SpM[P, uv1]]],
	TestID -> "3 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spba[SpM[Q, uv2], SmA, SmB, SmC, SpM[P, uv1]],
	HoldPattern[Spab[SpM[P, uv1], SmC, SmB, SmA, SpM[Q, uv2]]],
	TestID -> "3 smatrices: 2 massive different: reversed order"
];
Test[
	Spba[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SmC, x5 SpM[Q, uv2]],
	x1 x2 x3 x4 x5 Spba[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	TestID -> "3 smatrices: 2 massive different: ordered: scaled"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
