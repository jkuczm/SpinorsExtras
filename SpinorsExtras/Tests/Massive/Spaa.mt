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
	Spaa[a, a],
	TestID -> "0 smatrices: 2 massless same"
];
TestUnchanged[
	Spaa[a, b],
	TestID -> "0 smatrices: 2 massless different: ordered"
];
TestMatch[
	Spaa[b, a],
	HoldPattern[-Spaa[a, b]],
	TestID -> "0 smatrices: 2 massless different: reversed order"
];
Test[
	Spaa[x1 a, x2 b],
	x1 x2 Spaa[a, b],
	TestID -> "0 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spaa[a, SpM[P, uv1]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spaa[SpM[P, uv1], a],
	HoldPattern[-Spaa[a, SpM[P, uv1]]],
	TestID -> "0 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spaa[x1 a, x2 SpM[P, uv1]],
	x1 x2 Spaa[a, SpM[P, uv1]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestZero[
	Spaa[SpM[P, uv1], SpM[P, uv1]],
	TestID -> "0 smatrices: 2 massive same"
];
TestUnchanged[
	Spaa[SpM[P, uv1], SpM[Q, uv2]],
	TestID -> "0 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spaa[SpM[Q, uv2], SpM[P, uv1]],
	HoldPattern[-Spaa[SpM[P, uv1], SpM[Q, uv2]]],
	TestID -> "0 smatrices: 2 massive different: reversed order"
];
Test[
	Spaa[x1 SpM[P, uv1], x2 SpM[Q, uv2]],
	x1 x2 Spaa[SpM[P, uv1], SpM[Q, uv2]],
	TestID -> "0 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*1 smatrix*)


TestZero[
	Spaa[a, SmA, a],
	TestID -> "1 smatrices: 2 massless same"
];
TestZero[
	Spaa[a, SmA, b],
	TestID -> "1 smatrices: 2 massless different: ordered"
];
TestZero[
	Spaa[b, SmA, a],
	TestID -> "1 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spaa[x1 a, x2 SmA, x3 b],
	TestID -> "1 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spaa[a, SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spaa[SpM[P, uv1], SmA, a],
	HoldPattern[-Spaa[a, SmA, SpM[P, uv1]]],
	TestID -> "1 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spaa[x1 a, x2 SmA, x3 SpM[P, uv1]],
	x1 x2 x3 Spaa[a, SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spaa[SpM[P, uv1], SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 2 massive same"
];
TestUnchanged[
	Spaa[SpM[P, uv1], SmA, SpM[Q, uv2]],
	TestID -> "1 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spaa[SpM[Q, uv2], SmA, SpM[P, uv1]],
	HoldPattern[-Spaa[SpM[P, uv1], SmA, SpM[Q, uv2]]],
	TestID -> "1 smatrices: 2 massive different: reversed order"
];
Test[
	Spaa[x1 SpM[P, uv1], x2 SmA, x3 SpM[Q, uv2]],
	x1 x2 x3 Spaa[SpM[P, uv1], SmA, SpM[Q, uv2]],
	TestID -> "1 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*2 smatrices*)


TestUnchanged[
	Spaa[a, SmA, SmB, a],
	TestID -> "2 smatrices: 2 massless same: ordered"
];
TestMatch[
	Spaa[a, SmB, SmA, a],
	HoldPattern[-Spaa[a, SmA, SmB, a]],
	TestID -> "2 smatrices: 2 massless same: reversed order"
];
TestUnchanged[
	Spaa[a, SmA, SmB, b],
	TestID -> "2 smatrices: 2 massless different: ordered"
];
TestMatch[
	Spaa[b, SmA, SmB, a],
	HoldPattern[-Spaa[a, SmB, SmA, b]],
	TestID -> "2 smatrices: 2 massless different: reversed order"
];
Test[
	Spaa[x1 a, x2 SmA, x3 SmB, x4 b],
	x1 x2 x3 x4 Spaa[a, SmA, SmB, b],
	TestID -> "2 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spaa[a, SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spaa[SpM[P, uv1], SmA, SmB, a],
	HoldPattern[-Spaa[a, SmB, SmA, SpM[P, uv1]]],
	TestID -> "2 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spaa[x1 a, x2 SmA, x3 SmB, x4 SpM[P, uv1]],
	x1 x2 x3 x4 Spaa[a, SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spaa[SpM[P, uv1], SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 2 massive same: ordered"
];
TestMatch[
	Spaa[SpM[P, uv1], SmB, SmA, SpM[P, uv1]],
	HoldPattern[-Spaa[SpM[P, uv1], SmA, SmB, SpM[P, uv1]]],
	TestID -> "2 smatrices: 2 massive same: reversed order"
];
TestUnchanged[
	Spaa[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	TestID -> "2 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spaa[SpM[Q, uv2], SmA, SmB, SpM[P, uv1]],
	HoldPattern[-Spaa[SpM[P, uv1], SmB, SmA, SpM[Q, uv2]]],
	TestID -> "2 smatrices: 2 massive different: reversed order"
];
Test[
	Spaa[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SpM[Q, uv2]],
	x1 x2 x3 x4 Spaa[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	TestID -> "2 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*3 smatrices*)


TestZero[
	Spaa[a, SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 2 massless same: ordered"
];
TestZero[
	Spaa[a, SmC, SmB, SmA, a],
	TestID -> "3 smatrices: 2 massless same: reversed order"
];
TestZero[
	Spaa[a, SmA, SmB, SmC, b],
	TestID -> "3 smatrices: 2 massless different: ordered"
];
TestZero[
	Spaa[b, SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spaa[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 b],
	TestID -> "3 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spaa[a, SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered"
];
TestMatch[
	Spaa[SpM[P, uv1], SmA, SmB, SmC, a],
	HoldPattern[-Spaa[a, SmC, SmB, SmA, SpM[P, uv1]]],
	TestID -> "3 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spaa[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 SpM[P, uv1]],
	x1 x2 x3 x4 x5 Spaa[a, SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spaa[SpM[P, uv1], SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 2 massive same: ordered"
];
TestMatch[
	Spaa[SpM[P, uv1], SmC, SmB, SmA, SpM[P, uv1]],
	HoldPattern[-Spaa[SpM[P, uv1], SmA, SmB, SmC, SpM[P, uv1]]],
	TestID -> "3 smatrices: 2 massive same: reversed order"
];
TestUnchanged[
	Spaa[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	TestID -> "3 smatrices: 2 massive different: ordered"
];
TestMatch[
	Spaa[SpM[Q, uv2], SmA, SmB, SmC, SpM[P, uv1]],
	HoldPattern[-Spaa[SpM[P, uv1], SmC, SmB, SmA, SpM[Q, uv2]]],
	TestID -> "3 smatrices: 2 massive different: reversed order"
];
Test[
	Spaa[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SmC, x5 SpM[Q, uv2]],
	x1 x2 x3 x4 x5 Spaa[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	TestID -> "3 smatrices: 2 massive different: ordered: scaled"
];

(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
