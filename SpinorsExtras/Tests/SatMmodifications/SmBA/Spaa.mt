(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Massive`",
	"SpinorsExtras`SatMmodifications`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, c, a1, b1, a2, b2, a3, b3},
	"LVectors" -> {P, Q},
	"SMatrices" -> {Sm1, Sm2},
	"PlusMinusOnes" -> {uv1, uv2}
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*1 slashed matrix*)


TestZero[
	Spaa[a, SmBA[b1, a1], c],
	TestID -> "1 smatrices: 2 massless"
];
Test[
	Spaa[a, SmBA[b1, a1], SpM[P, uv1]],
	Spaa[a, a1] Spba[b1, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless"
];
Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], b1] Spaa[a1, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], a1] Spba[b1, SpM[Q, uv2]]
	,
	TestID -> "1 smatrices: 2 massive"
];


(* ::Subsection:: *)
(*2 slashed matrices*)


Test[
	Spaa[a, SmBA[b1, a1], Sm1, c],
	Spaa[a, a1] Spba[b1, Sm1, c],
	TestID -> "2 smatrices: 2 massless: SmBA first"
];
Test[
	Spaa[a, Sm1, SmBA[b1, a1], c],
	Spab[a, Sm1, b1] Spaa[a1, c],
	TestID -> "2 smatrices: 2 massless: SmBA second"
];
Test[
	Spaa[a, SmBA[b1, a1], SmBA[b2, a2], c],
	Spaa[a, a1] Spbb[b1, b2] Spaa[a2, c],
	TestID -> "2 smatrices: 2 massless: SmBA first, second"
];

Test[
	Spaa[a, SmBA[b1, a1], Sm1, SpM[P, uv1]],
	Spaa[a, a1] Spba[b1, Sm1, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA first"
];
Test[
	Spaa[a, Sm1, SmBA[b1, a1], SpM[P, uv1]],
	Spab[a, Sm1, b1] Spaa[a1, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA second"
];
Test[
	Spaa[a, SmBA[b1, a1], SmBA[b2, a2], SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, b2] Spaa[a2, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA first, second"
];

Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], Sm1, SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spba[b1, Sm1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, Sm1, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA first"
];
Test[
	Spaa[SpM[P, uv1], Sm1, SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], Sm1, b1] Spaa[a1, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], Sm1, a1] Spba[b1, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA second"
];
Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, b2] Spaa[a2, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, a2] Spba[b2, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA first, second"
];


(* ::Subsection:: *)
(*3 slashed matrices*)


TestZero[
	Spaa[a, SmBA[b1, a1], Sm1, Sm2, c],
	TestID -> "3 smatrices: 2 massless: SmBA first"
];
TestZero[
	Spaa[a, Sm1, SmBA[b1, a1], Sm2, c],
	TestID -> "3 smatrices: 2 massless: SmBA second"
];
TestZero[
	Spaa[a, Sm1, Sm2, SmBA[b1, a1], c],
	TestID -> "3 smatrices: 2 massless: SmBA third"
];
TestZero[
	Spaa[a, SmBA[b1, a1], SmBA[b2, a2], Sm1, c],
	TestID -> "3 smatrices: 2 massless: SmBA first, second"
];
TestZero[
	Spaa[a, SmBA[b1, a1], Sm1, SmBA[b2, a2], c],
	TestID -> "3 smatrices: 2 massless: SmBA first, third"
];
TestZero[
	Spaa[a, Sm1, SmBA[b1, a1], SmBA[b2, a2], c],
	TestID -> "3 smatrices: 2 massless: SmBA second, third"
];
TestZero[
	Spaa[a, SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], c],
	TestID -> "3 smatrices: 2 massless: SmBA first, second, third"
];

Test[
	Spaa[a, SmBA[b1, a1], Sm1, Sm2, SpM[P, uv1]],
	Spaa[a, a1] Spba[b1, Sm1, Sm2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first"
];
Test[
	Spaa[a, Sm1, SmBA[b1, a1], Sm2, SpM[P, uv1]],
	Spab[a, Sm1, b1] Spaa[a1, Sm2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA second"
];
Test[
	Spaa[a, Sm1, Sm2, SmBA[b1, a1], SpM[P, uv1]],
	Spaa[a, Sm1, Sm2, a1] Spba[b1, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA third"
];
Test[
	Spaa[a, SmBA[b1, a1], SmBA[b2, a2], Sm1, SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, b2] Spaa[a2, Sm1, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, second"
];
Test[
	Spaa[a, SmBA[b1, a1], Sm1, SmBA[b2, a2], SpM[P, uv1]],
	Spaa[a, a1] Spba[b1, Sm1, a2] Spba[b2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, third"
];
Test[
	Spaa[a, Sm1, SmBA[b1, a1], SmBA[b2, a2], SpM[P, uv1]],
	Spab[a, Sm1, b1] Spaa[a1, a2] Spba[b2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA second, third"
];
Test[
	Spaa[a, SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], SpM[P, uv1]],
	Spaa[a, a1] Spbb[b1, b2] Spaa[a2, a3] Spba[b3, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, second, third"
];

Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], Sm1, Sm2, SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spba[b1, Sm1, Sm2, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, Sm1, Sm2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first"
];
Test[
	Spaa[SpM[P, uv1], Sm1, SmBA[b1, a1], Sm2, SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], Sm1, b1] Spaa[a1, Sm2, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], Sm1, a1] Spba[b1, Sm2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA second"
];
Test[
	Spaa[SpM[P, uv1], Sm1, Sm2, SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], Sm1, Sm2, a1] Spba[b1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], Sm1, Sm2, b1] Spaa[a1, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA third"
];
Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], Sm1, SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, b2] Spaa[a2, Sm1, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, a2] Spba[b2, Sm1, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, second"
];
Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], Sm1, SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spba[b1, Sm1, a2] Spba[b2, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spab[a1, Sm1, b2] Spaa[a2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, third"
];
Test[
	Spaa[SpM[P, uv1], Sm1, SmBA[b1, a1], SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spab[SpM[P, uv1], Sm1, b1] Spaa[a1, a2] Spba[b2, SpM[Q, uv2]] +
	Spaa[SpM[P, uv1], Sm1, a1] Spbb[b1, b2] Spaa[a2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA second, third"
];
Test[
	Spaa[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], SpM[Q, uv2]]
	,
	Spaa[SpM[P, uv1], a1] Spbb[b1, b2] Spaa[a2, a3] Spba[b3, SpM[Q, uv2]] +
	Spab[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, b3] Spaa[a3, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, second, third"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
