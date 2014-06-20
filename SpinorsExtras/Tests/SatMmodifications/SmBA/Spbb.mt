(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Massive`",
	"SpinorsExtras`SatMmodifications`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {b, c, a1, b1, a2, b2, a3, b3},
	"LVectors" -> {P, Q},
	"SMatrices" -> {Sm1, Sm2},
	"PlusMinusOnes" -> {uv1, uv2}
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*1 slashed matrix*)


TestZero[
	Spbb[b, SmBA[b1, a1], c],
	TestID -> "1 smatrices: 2 massless"
];
Test[
	Spbb[b, SmBA[b1, a1], SpM[P, uv1]],
	Spbb[b, b1] Spab[a1, SpM[P, uv1]],
	TestID -> "1 smatrices: 1 massive, 1 massless"
];
Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spba[SpM[P, uv1], a1] Spbb[b1, SpM[Q, uv2]] +
	Spbb[SpM[P, uv1], b1] Spab[a1, SpM[Q, uv2]]
	,
	TestID -> "1 smatrices: 2 massive"
];


(* ::Subsection:: *)
(*2 slashed matrices*)


Test[
	Spbb[b, SmBA[b1, a1], Sm1, c],
	Spbb[b, b1] Spab[a1, Sm1, c],
	TestID -> "2 smatrices: 2 massless: SmBA first"
];
Test[
	Spbb[b, Sm1, SmBA[b1, a1], c],
	Spba[b, Sm1, a1] Spbb[b1, c],
	TestID -> "2 smatrices: 2 massless: SmBA second"
];
Test[
	Spbb[b, SmBA[b1, a1], SmBA[b2, a2], c],
	Spbb[b, b1] Spaa[a1, a2] Spbb[b2, c],
	TestID -> "2 smatrices: 2 massless: SmBA first, second"
];

Test[
	Spbb[b, SmBA[b1, a1], Sm1, SpM[P, uv1]],
	Spbb[b, b1] Spab[a1, Sm1, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA first"
];
Test[
	Spbb[b, Sm1, SmBA[b1, a1], SpM[P, uv1]],
	Spba[b, Sm1, a1] Spbb[b1, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA second"
];
Test[
	Spbb[b, SmBA[b1, a1], SmBA[b2, a2], SpM[P, uv1]],
	Spbb[b, b1] Spaa[a1, a2] Spbb[b2, SpM[P, uv1]],
	TestID -> "2 smatrices: 1 massive, 1 massless: SmBA first, second"
];

Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], Sm1, SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], b1] Spab[a1, Sm1, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], a1] Spbb[b1, Sm1, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA first"
];
Test[
	Spbb[SpM[P, uv1], Sm1, SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spba[SpM[P, uv1], Sm1, a1] Spbb[b1, SpM[Q, uv2]] +
	Spbb[SpM[P, uv1], Sm1, b1] Spab[a1, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA second"
];
Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], a1] Spbb[b1, b2] Spab[a2, SpM[Q, uv2]]
	,
	TestID -> "2 smatrices: 2 massive: SmBA first, second"
];


(* ::Subsection:: *)
(*3 slashed matrices*)


TestZero[
	Spbb[b, SmBA[b1, a1], Sm1, Sm2, c],
	TestID -> "3 smatrices: 2 massless: SmBA first"
];
TestZero[
	Spbb[b, Sm1, SmBA[b1, a1], Sm2, c],
	TestID -> "3 smatrices: 2 massless: SmBA second"
];
TestZero[
	Spbb[b, Sm1, Sm2, SmBA[b1, a1], c],
	TestID -> "3 smatrices: 2 massless: SmBA third"
];
TestZero[
	Spbb[b, SmBA[b1, a1], SmBA[b2, a2], Sm1, c],
	TestID -> "3 smatrices: 2 massless: SmBA first, second"
];
TestZero[
	Spbb[b, SmBA[b1, a1], Sm1, SmBA[b2, a2], c],
	TestID -> "3 smatrices: 2 massless: SmBA first, third"
];
TestZero[
	Spbb[b, Sm1, SmBA[b1, a1], SmBA[b2, a2], c],
	TestID -> "3 smatrices: 2 massless: SmBA second, third"
];
TestZero[
	Spbb[b, SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], c],
	TestID -> "3 smatrices: 2 massless: SmBA first, second, third"
];

Test[
	Spbb[b, SmBA[b1, a1], Sm1, Sm2, SpM[P, uv1]],
	Spbb[b, b1] Spab[a1, Sm1, Sm2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first"
];
Test[
	Spbb[b, Sm1, SmBA[b1, a1], Sm2, SpM[P, uv1]],
	Spba[b, Sm1, a1] Spbb[b1, Sm2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA second"
];
Test[
	Spbb[b, Sm1, Sm2, SmBA[b1, a1], SpM[P, uv1]],
	Spbb[b, Sm1, Sm2, b1] Spab[a1, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA third"
];
Test[
	Spbb[b, SmBA[b1, a1], SmBA[b2, a2], Sm1, SpM[P, uv1]],
	Spbb[b, b1] Spaa[a1, a2] Spbb[b2, Sm1, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, second"
];
Test[
	Spbb[b, SmBA[b1, a1], Sm1, SmBA[b2, a2], SpM[P, uv1]],
	Spbb[b, b1] Spab[a1, Sm1, b2] Spab[a2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, third"
];
Test[
	Spbb[b, Sm1, SmBA[b1, a1], SmBA[b2, a2], SpM[P, uv1]],
	Spba[b, Sm1, a1] Spbb[b1, b2] Spab[a2, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA second, third"
];
Test[
	Spbb[b, SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], SpM[P, uv1]],
	Spbb[b, b1] Spaa[a1, a2] Spbb[b2, b3] Spab[a3, SpM[P, uv1]],
	TestID -> "3 smatrices: 1 massive, 1 massless: SmBA first, second, third"
];

Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], Sm1, Sm2, SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], b1] Spab[a1, Sm1, Sm2, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], a1] Spbb[b1, Sm1, Sm2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first"
];
Test[
	Spbb[SpM[P, uv1], Sm1, SmBA[b1, a1], Sm2, SpM[Q, uv2]]
	,
	Spba[SpM[P, uv1], Sm1, a1] Spbb[b1, Sm2, SpM[Q, uv2]] +
	Spbb[SpM[P, uv1], Sm1, b1] Spab[a1, Sm2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA second"
];
Test[
	Spbb[SpM[P, uv1], Sm1, Sm2, SmBA[b1, a1], SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], Sm1, Sm2, b1] Spab[a1, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], Sm1, Sm2, a1] Spbb[b1, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA third"
];
Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], Sm1, SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, Sm1, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], a1] Spbb[b1, b2] Spab[a2, Sm1, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, second"
];
Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], Sm1, SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], b1] Spab[a1, Sm1, b2] Spab[a2, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], a1] Spba[b1, Sm1, a2] Spbb[b2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, third"
];
Test[
	Spbb[SpM[P, uv1], Sm1, SmBA[b1, a1], SmBA[b2, a2], SpM[Q, uv2]]
	,
	Spba[SpM[P, uv1], Sm1, a1] Spbb[b1, b2] Spab[a2, SpM[Q, uv2]] +
	Spbb[SpM[P, uv1], Sm1, b1] Spaa[a1, a2] Spbb[b2, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA second, third"
];
Test[
	Spbb[SpM[P, uv1], SmBA[b1, a1], SmBA[b2, a2], SmBA[b3, a3], SpM[Q, uv2]]
	,
	Spbb[SpM[P, uv1], b1] Spaa[a1, a2] Spbb[b2, b3] Spab[a3, SpM[Q, uv2]] +
	Spba[SpM[P, uv1], a1] Spbb[b1, b2] Spaa[a2, a3] Spbb[b3, SpM[Q, uv2]]
	,
	TestID -> "3 smatrices: 2 massive: SmBA first, second, third"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
