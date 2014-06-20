(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`MassiveUtilities`",
	"SpinorsExtras`Massive`",
	(* numerics for s with args with coefficients *)
	"SpinorsExtras`SatMmodifications`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c},
	"LVectors" -> {P, Q, R},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All,
	"SpAssocMomentaLVectors" -> All
];

N[x] = RandomReal[];
N[y] = RandomReal[];
N[z] = RandomReal[];


SetOptions[{TestSubexpression, TestCaseSymbolicNumeric},
	ApplyToInput -> ExpandSToSpinors
];

SetOptions[TestCaseSymbolicNumeric,
	InputWrapperN -> (SetPrecision[N[LightConeDecompose[#]], 10]&)
];


(* ::Section:: *)
(*Tests*)


SetOptions[ExpandSToSpinors, "Massive" -> True];

TestCaseSymbolicNumeric[
	{s[a, b]},
	Spaa[a, b] Spbb[b, a],
	TestID -> "Massive -> True: 2 spinors"
];
TestCaseSymbolicNumeric[
	{s[a, Q]},
	Spab[a, Q, a] + MP2[Q],
	TestID -> "Massive -> True: 1 spinor, 1 LVector"
];
TestCaseSymbolicNumeric[
	{s[P, b]},
	Spab[b, P, b] + MP2[P],
	TestID -> "Massive -> True: 1 spinor, 1 LVector (alternative order)"
];
TestCaseSymbolicNumeric[
	{s[P, Q]},
	Spab[SpM[P, +1], Q, SpM[P, +1]] + MP2[P] + MP2[Q],
	TestID -> "Massive -> True: 2 LVectors"
];

TestCaseSymbolicNumeric[
	{s[a, b, c]},
	Spaa[a, b] Spbb[b, a] + Spaa[a, c] Spbb[c, a] + Spaa[b, c] Spbb[c, b],
	TestID -> "Massive -> True: 3 spinors"
];
TestCaseSymbolicNumeric[
	{s[x a, y z b, c]}
	,
	x y z Spaa[a, b] Spbb[b, a]
	+ x Spaa[a, c] Spbb[c, a]
	+ y z Spaa[b, c] Spbb[c, b]
	,
	TestID -> "Massive -> True: 3 spinors (with coefficients)"
];
TestCaseSymbolicNumeric[
	{s[a, b, R]},
	Spaa[a, b] Spbb[b, a] + Spab[a, R, a] + Spab[b, R, b] + MP2[R],
	TestID -> "Massive -> True: 2 spinors, 1 LVector"
];
TestCaseSymbolicNumeric[
	{s[a, Q, R]}
	,
	Spab[a, Q, a] + Spab[a, R, a]
	+ Spab[SpM[Q, +1], R, SpM[Q, +1]]
	+ MP2[Q] + MP2[R]
	,
	TestID -> "Massive -> True: 1 spinor, 2 LVectors"
];
TestCaseSymbolicNumeric[
	{s[x a, y z Q, R]}
	,
	x y z Spab[a, Q, a] + x Spab[a, R, a]
	+ y z Spab[SpM[R, +1], Q, SpM[R, +1]]
	+ y^2 z^2 MP2[Q] + MP2[R]
	,
	TestID -> "Massive -> True: 1 spinor, 2 LVectors (with coefficients)"
];
TestCaseSymbolicNumeric[
	{s[P, Q, R]}
	,
	Spab[SpM[P, +1], Q, SpM[P, +1]]
	+ Spab[SpM[P, +1], R, SpM[P, +1]]
	+ Spab[SpM[Q, +1], R, SpM[Q, +1]]
	+ MP2[P] + MP2[Q] + MP2[R]
	,
	TestID -> "Massive -> True: 3 LVectors"
];
TestCaseSymbolicNumeric[
	{s[P, Q, R], "UOrVFunction" -> (Switch[#, P, +1, Q, -1]&)}
	,
	Spab[SpM[P, +1], Q, SpM[P, +1]]
	+ Spab[SpM[P, +1], R, SpM[P, +1]]
	+ Spab[SpM[Q, -1], R, SpM[Q, -1]]
	+ MP2[P] + MP2[Q] + MP2[R]
	,
	TestID -> "Massive -> True: 3 LVectors, UOrVFunction"
];


SetOptions[ExpandSToSpinors, "Massive" -> False];

TestCaseSymbolicNumeric[
	{s[a, b]},
	Spaa[a, b] Spbb[b, a],
	TestID -> "Massive -> False: 2 spinors"
];
TestSubexpression[
	{s[a, Q]},
	TestID -> "Massive -> False: 1 spinor, 1 LVector"
];
TestSubexpression[
	{s[P, Q]},
	TestID -> "Massive -> False: 2 LVectors"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
