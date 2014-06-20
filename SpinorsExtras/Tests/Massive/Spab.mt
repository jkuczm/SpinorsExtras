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
	Spab[a, a],
	TestID -> "0 smatrices: 2 massless same"
];
TestZero[
	Spab[a, b],
	TestID -> "0 smatrices: 2 massless different: ordered"
];
TestZero[
	Spab[b, a],
	TestID -> "0 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spab[x1 a, x2 b],
	TestID -> "0 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spab[a, SpM[P, uv1]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spab[SpM[P, uv1], a],
	TestID -> "0 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spab[x1 a, x2 SpM[P, uv1]],
	x1 x2 Spab[a, SpM[P, uv1]],
	TestID -> "0 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spab[SpM[P, uv1], SpM[P, uv1]],
	TestID -> "0 smatrices: 2 massive same"
];
TestUnchanged[
	Spab[SpM[P, uv1], SpM[Q, uv2]],
	TestID -> "0 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spab[SpM[Q, uv2], SpM[P, uv1]],
	TestID -> "0 smatrices: 2 massive different: reversed order"
];
Test[
	Spab[x1 SpM[P, uv1], x2 SpM[Q, uv2]],
	x1 x2 Spab[SpM[P, uv1], SpM[Q, uv2]],
	TestID -> "0 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*1 smatrix*)


TestUnchanged[
	Spab[a, SmA, a],
	TestID -> "1 smatrices: 2 massless same"
];
TestUnchanged[
	Spab[a, SmA, b],
	TestID -> "1 smatrices: 2 massless different: ordered"
];
TestUnchanged[
	Spab[b, SmA, a],
	TestID -> "1 smatrices: 2 massless different: reversed order"
];
Test[
	Spab[x1 a, x2 SmA, x3 b],
	x1 x2 x3 Spab[a, SmA, b],
	TestID -> "1 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spab[a, SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmA, a],
	TestID -> "1 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spab[x1 a, x2 SmA, x3 SpM[P, uv1]],
	x1 x2 x3 Spab[a, SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spab[SpM[P, uv1], SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 2 massive same"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmA, SpM[Q, uv2]],
	TestID -> "1 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spab[SpM[Q, uv2], SmA, SpM[P, uv1]],
	TestID -> "1 smatrices: 2 massive different: reversed order"
];
Test[
	Spab[x1 SpM[P, uv1], x2 SmA, x3 SpM[Q, uv2]],
	x1 x2 x3 Spab[SpM[P, uv1], SmA, SpM[Q, uv2]],
	TestID -> "1 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*2 smatrices*)


TestZero[
	Spab[a, SmA, SmB, a],
	TestID -> "2 smatrices: 2 massless same: ordered"
];
TestZero[
	Spab[a, SmB, SmA, a],
	TestID -> "2 smatrices: 2 massless same: reversed order"
];
TestZero[
	Spab[a, SmA, SmB, b],
	TestID -> "2 smatrices: 2 massless different: ordered"
];
TestZero[
	Spab[b, SmA, SmB, a],
	TestID -> "2 smatrices: 2 massless different: reversed order"
];
TestZero[
	Spab[x1 a, x2 SmA, x3 SmB, x4 b],
	TestID -> "2 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spab[a, SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmA, SmB, a],
	TestID -> "2 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spab[x1 a, x2 SmA, x3 SmB, x4 SpM[P, uv1]],
	x1 x2 x3 x4 Spab[a, SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spab[SpM[P, uv1], SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 2 massive same: ordered"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmB, SmA, SpM[P, uv1]],
	TestID -> "2 smatrices: 2 massive same: reversed order"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	TestID -> "2 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spab[SpM[Q, uv2], SmA, SmB, SpM[P, uv1]],
	TestID -> "2 smatrices: 2 massive different: reversed order"
];
Test[
	Spab[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SpM[Q, uv2]],
	x1 x2 x3 x4 Spab[SpM[P, uv1], SmA, SmB, SpM[Q, uv2]],
	TestID -> "2 smatrices: 2 massive different: ordered: scaled"
];


(* ::Subsection:: *)
(*3 smatrices*)


TestUnchanged[
	Spab[a, SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 2 massless same: ordered"
];
TestUnchanged[
	Spab[a, SmC, SmB, SmA, a],
	TestID -> "3 smatrices: 2 massless same: reversed order"
];
TestUnchanged[
	Spab[a, SmA, SmB, SmC, b],
	TestID -> "3 smatrices: 2 massless different: ordered"
];
TestUnchanged[
	Spab[b, SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 2 massless different: reversed order"
];
Test[
	Spab[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 b],
	x1 x2 x3 x4 x5 Spab[a, SmA, SmB, SmC, b],
	TestID -> "3 smatrices: 2 massless different: ordered: scaled"
];

TestUnchanged[
	Spab[a, SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmA, SmB, SmC, a],
	TestID -> "3 smatrices: 1 massive, 1 massless: reversed order"
];
Test[
	Spab[x1 a, x2 SmA, x3 SmB, x4 SmC, x5 SpM[P, uv1]],
	x1 x2 x3 x4 x5 Spab[a, SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: ordered: scaled"
];

TestUnchanged[
	Spab[SpM[P, uv1], SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 2 massive same: ordered"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmC, SmB, SmA, SpM[P, uv1]],
	TestID -> "3 smatrices: 2 massive same: reversed order"
];
TestUnchanged[
	Spab[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	TestID -> "3 smatrices: 2 massive different: ordered"
];
TestUnchanged[
	Spab[SpM[Q, uv2], SmA, SmB, SmC, SpM[P, uv1]],
	TestID -> "3 smatrices: 2 massive different: reversed order"
];
Test[
	Spab[x1 SpM[P, uv1], x2 SmA, x3 SmB, x4 SmC, x5 SpM[Q, uv2]],
	x1 x2 x3 x4 x5 Spab[SpM[P, uv1], SmA, SmB, SmC, SpM[Q, uv2]],
	TestID -> "3 smatrices: 2 massive different: ordered: scaled"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
