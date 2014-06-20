(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Massive`",
	"SpinorsExtras`SatMmodifications`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, a1, b1, a2, b2, a3, b3},
	"LVectors" -> {P, Q},
	"SMatrices" -> {Sm1, Sm2},
	"PlusMinusOnes" -> {uv1, uv2}
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*1 slashed matrix*)


Test[
	Spab[a, SmBA[b1, a1], b],
	Spaa[a, a1] Spbb[b1, b],
	TestID -> "1 smatrices: 2 massless"
];
Test[
	Spab[a, SmBA[b1, a1], SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless"
];
Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spab[a1, SpM[Q, uv2]]
	,
	TestID -> "1 smatrices: 2 massive"
];


(* ::Subsection:: *)
(*2 slashed matrices*)


TestZero[
	Spab[a, SmBA[b1, a1], Sm1, b],
	TestID -> "2 smatrices: 2 massless: SmBA first"
];
TestZero[
	Spab[a, Sm1, SmBA[b1, a1], b],
	TestID -> "2 smatrices: 2 massless: SmBA second"
];
TestZero[
	Spab[a, SmBA[b1, a1], SmBA[b2, a2], b],
	TestID -> "2 smatrices: 2 massless: SmBA first, second"
];

Test[
	Spab[a, SmBA[b1, a1], Sm1, SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, Sm1, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA first"
];
Test[
	Spab[a, Sm1, SmBA[b1, a1], SpM[P, uv1]],
	Spab[a, Sm1, b1] Spab[a1, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA second"
];
Test[
	Spab[a, SmBA[b1, a1], SmBA[b2, a2], SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, b2] Spab[a2, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA first, second"
];

Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], Sm1, SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, Sm1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spab[a1, Sm1, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA first"
];
Test[
	Spab[SpM[P, uv1], Sm1, SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], Sm1, b1] Spab[a1, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], Sm1, a1] Spbb[b1, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA second"
];
Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, b2] Spab[a2, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA first, second"
];


(* ::Subsection:: *)
(*3 slashed matrices*)


Test[
	Spab[a, SmBA[b1, a1], Sm1, Sm2, b],
	Spaa[a, a1] Spbb[b1, Sm1, Sm2, b],
	TestID -> "3 smatrices: 2 massless: SmBA first"
];
Test[
	Spab[a, Sm1, SmBA[b1, a1], Sm2, b],
	Spab[a, Sm1, b1] Spab[a1, Sm2, b],
	TestID -> "3 smatrices: 2 massless: SmBA second"
];
Test[
	Spab[a, Sm1, Sm2, SmBA[b1, a1], b],
	Spaa[a, Sm1, Sm2, a1] Spbb[b1, b],
	TestID -> "3 smatrices: 2 massless: SmBA third"
];
Test[
	Spab[a, SmBA[b1, a1], SmBA[b2, a2], Sm1, b],
	Spaa[a, a1] Spbb[b1, b2] Spab[a2, Sm1, b],
	TestID -> "3 smatrices: 2 massless: SmBA first, second"
];
Test[
	Spab[a, SmBA[b1, a1], Sm1, SmBA[b2, a2], b],
	Spaa[a, a1] Spba[b1, Sm1, a2] Spbb[b2, b],
	TestID -> "3 smatrices: 2 massless: SmBA first, third"
];
Test[
	Spab[a, Sm1, SmBA[b1, a1], SmBA[b2, a2], b],
	Spab[a, Sm1, b1] Spaa[a1, a2] Spbb[b2, b],
	TestID -> "3 smatrices: 2 massless: SmBA second, third"
];
Test[
	Spab[a, SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], b],
	Spaa[a, a1] Spbb[b1, b2] Spaa[a2, a3] Spbb[b3, b],
	TestID -> "3 smatrices: 2 massless: SmBA first, second, third"
];

Test[
	Spab[a, SmBA[b1, a1], Sm1, Sm2, SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, Sm1, Sm2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first"
];
Test[
	Spab[a, Sm1, SmBA[b1, a1], Sm2, SpM[P, uv1]],
	Spab[a, Sm1, b1] Spab[a1, Sm2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA second"
];
Test[
	Spab[a, Sm1, Sm2, SmBA[b1, a1], SpM[P, uv1]],
	Spaa[a, Sm1, Sm2, a1] Spbb[b1, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA third"
];
Test[
	Spab[a, SmBA[b1, a1], SmBA[b2, a2], Sm1, SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, b2] Spab[a2, Sm1, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, second"
];
Test[
	Spab[a, SmBA[b1, a1], Sm1, SmBA[b2, a2], SpM[P, uv1]],
	Spaa[a, a1] Spba[b1, Sm1, a2] Spbb[b2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, third"
];
Test[
	Spab[a, Sm1, SmBA[b1, a1], SmBA[b2, a2], SpM[P, uv1]],
	Spab[a, Sm1, b1] Spaa[a1, a2] Spbb[b2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA second, third"
];
Test[
	Spab[a, SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, b2] Spaa[a2, a3] Spbb[b3, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, second, third"
];

Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], Sm1, Sm2, SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, Sm1, Sm2, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spab[a1, Sm1, Sm2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first"
];
Test[
	Spab[SpM[P, uv1], Sm1, SmBA[b1, a1], Sm2, SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], Sm1, b1] Spab[a1, Sm2, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], Sm1, a1] Spbb[b1, Sm2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA second"
];
Test[
	Spab[SpM[P, uv1], Sm1, Sm2, SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], Sm1, Sm2, a1] Spbb[b1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], Sm1, Sm2, b1] Spab[a1, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA third"
];
Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], Sm1, SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, b2] Spab[a2, Sm1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, Sm1, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, second"
];
Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], Sm1, SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spba[b1, Sm1, a2] Spbb[b2, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spab[a1, Sm1, b2] Spab[a2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, third"
];
Test[
	Spab[SpM[P, uv1], Sm1, SmBA[b1, a1], SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], Sm1, b1] Spaa[a1, a2] Spbb[b2, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], Sm1, a1] Spbb[b1, b2] Spab[a2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA second, third"
];
Test[
	Spab[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, b2] Spaa[a2, a3] Spbb[b3, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, b3] Spab[a3, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, second, third"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
