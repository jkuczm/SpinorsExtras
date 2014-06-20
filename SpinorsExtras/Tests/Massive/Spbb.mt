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
	Spbb[a, a],
	TestID -> "0 smatrices: 2 massless same"
];
TestMatch[
	Spbb[a, b],
	HoldPattern[-Spbb[b, a]],
	TestID -> "0 smatrices: 2 massless different: ordered"
];
TestUnchanged[
	Spbb[b, a],
	TestID -> "0 smatrices: 2 massless different: reversed order"
];
Test[
	Spbb[x1 a, x2 b],
	x1 x2 Spbb[a, b],
	TestID -> "0 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spbb[a, SpM[P, uv1]],
	HoldPattern[-Spbb[SpM[P, uv1], a]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spbb[SpM[P, uv1], a],
	TestID -> "0 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spbb[x1 a, x2 SpM[P, uv1]],
	x1 x2 Spbb[a, SpM[P, uv1]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestZero[
	Spbb[SpM[P, uv1], SpM[P, uv1]],
	TestID -> "0 smatrices: 2 massive same"
];
TestMatch[
	Spbb[SpM[P, uv1], SpM[Q, uv2]],
	HoldPattern[-Spbb[SpM[Q, uv2], SpM[P, uv1]]],
	TestID -> "0 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spbb[SpM[Q, uv2], SpM[P, uv1]],
	TestID -> "0 smatrices: 2 massive different: reversed order"
];
Test[
	Spbb[x1 SpM[P, uv1], x2 SpM[Q, uv2]],
	x1 x2 Spbb[SpM[P, uv1], SpM[Q, uv2]],
	TestID -> "0 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*1 smatrix*)


TestZero[
	Spbb[a, SmA, a],
	TestID -> "1 smatrices: 2 massless same"
];
TestZero[
	Spbb[a, SmA, b],
	TestID -> "1 smatrices: 2 massless different: ordered"
];
TestZero[
	Spbb[b, SmA, a],
	TestID -> "1 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spbb[x1 a, x2 SmA, x3 b],
	TestID -> "1 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spbb[a, SmA, SpM[P, uv1]],
	HoldPattern[-Spbb[SpM[P, uv1], SmA, a]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spbb[SpM[P, uv1], SmA, a],
	TestID -> "1 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spbb[x1 a, x2 SmA, x3 SpM[P, uv1]],
	x1 x2 x3 Spbb[a, SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spbb[SpM[P, uv1], SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 2 massive same"
];
TestMatch[
	Spbb[SpM[P, uv1], SmA, SpM[Q, uv2]],
	HoldPattern[-Spbb[SpM[Q, uv2], SmA, SpM[P, uv1]]],
	TestID -> "1 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spbb[SpM[Q, uv2], SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 2 massive different: reversed order"
];
Test[
	Spbb[x1 SpM[P, uv1], x2 SmA, x3 SpM[Q, uv2]],
	x1 x2 x3 Spbb[SpM[P, uv1], SmA, SpM[Q, uv2]],
	TestID -> "1 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*2 smatrices*)


TestMatch[
	Spbb[a, SmA, SmB, a],
	HoldPattern[-Spbb[a, SmB, SmA, a]],
	TestID -> "2 smatrices: 2 massless same: ordered"
];
TestUnchanged[
	Spbb[a, SmB, SmA, a],
	TestID -> "2 smatrices: 2 massless same: reversed order"
];
TestMatch[
	Spbb[a, SmA, SmB, b],
	HoldPattern[-Spbb[b, SmB, SmA, a]],
	TestID -> "2 smatrices: 2 massless different: ordered"
];
TestUnchanged[
	Spbb[b, SmA, SmB, a],
	TestID -> "2 smatrices: 2 massless different: reversed order"
];
Test[
	Spbb[x1 a, x2 SmA, x3 SmB, x4 b],
	x1 x2 x3 x4 Spbb[a, SmA, SmB, b],
	TestID -> "2 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spbb[a, SmA, SmB, SpM[P, uv1]],
	HoldPattern[-Spbb[SpM[P, uv1], SmB, SmA, a]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spbb[SpM[P, uv1], SmA, SmB, a],
	TestID -> "2 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spbb[x1 a, x2 SmA, x3 SmB, x4 SpM[P, uv1]],
	x1 x2 x3 x4 Spbb[a, SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestMatch[
	Spbb[SpM[P, uv1], SmA, SmB, SpM[P, uv1]],
	HoldPattern[-Spbb[SpM[P, uv1], SmB, SmA, SpM[P, uv1]]],
	TestID -> "2 smatrices: 2 massive same: ordered"
];
TestUnchanged[
	Spbb[SpM[P, uv1], SmB, SmA, SpM[P, uv1]],
	TestID -> "2 smatrices: 2 massive same: reversed order"
];
TestMatch[
	Spbb[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	HoldPattern[-Spbb[SpM[Q, uv2], SmB, SmA, SpM[P, uv1]]],
	TestID -> "2 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spbb[SpM[Q, uv2], SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 2 massive different: reversed order"
];
Test[
	Spbb[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SpM[Q, uv2]],
	x1 x2 x3 x4 Spbb[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	TestID -> "2 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*3 smatrices*)


TestZero[
	Spbb[a, SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 2 massless same: ordered"
];
TestZero[
	Spbb[a, SmC, SmB, SmA, a],
	TestID -> "3 smatrices: 2 massless same: reversed order"
];
TestZero[
	Spbb[a, SmA, SmB, SmC, b],
	TestID -> "3 smatrices: 2 massless different: ordered"
];
TestZero[
	Spbb[b, SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spbb[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 b],
	TestID -> "3 smatrices: 2 massless different: ordered: scaled"
];

TestMatch[
	Spbb[a, SmA, SmB, SmC, SpM[P, uv1]],
	HoldPattern[-Spbb[SpM[P, uv1], SmC, SmB, SmA, a]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spbb[SpM[P, uv1], SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spbb[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 SpM[P, uv1]],
	x1 x2 x3 x4 x5 Spbb[a, SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestMatch[
	Spbb[SpM[P, uv1], SmA, SmB, SmC, SpM[P, uv1]],
	HoldPattern[-Spbb[SpM[P, uv1], SmC, SmB, SmA, SpM[P, uv1]]],
	TestID -> "3 smatrices: 2 massive same: ordered"
];
TestUnchanged[
	Spbb[SpM[P, uv1], SmC, SmB, SmA, SpM[P, uv1]],
	TestID -> "3 smatrices: 2 massive same: reversed order"
];
TestMatch[
	Spbb[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	HoldPattern[-Spbb[SpM[Q, uv2], SmC, SmB, SmA, SpM[P, uv1]]],
	TestID -> "3 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spbb[SpM[Q, uv2], SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 2 massive different: reversed order"
];
Test[
	Spbb[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SmC, x5 SpM[Q, uv2]],
	x1 x2 x3 x4 x5 Spbb[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	TestID -> "3 smatrices: 2 massive different: ordered: scaled"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
