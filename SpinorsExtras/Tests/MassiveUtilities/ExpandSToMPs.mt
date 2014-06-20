(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`MassiveUtilities`",
	(* numerics for s with args with coefficients *)
	"SpinorsExtras`SatMmodifications`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c},
	"LVectors" -> {P, Q, R},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All
];

N[x] = RandomReal[];
N[y] = RandomReal[];
N[z] = RandomReal[];


SetOptions[{TestSubexpression, TestCaseSymbolicNumeric},
	ApplyToInput -> ExpandSToMPs
];


(* ::Section:: *)
(*Tests*)


SetOptions[ExpandSToMPs, "Massive" -> True];

TestCaseSymbolicNumeric[
	{s[a, b]},
	2 MP[a, b],
	TestID -> "Massive -> True: 2 spinors"
];
TestCaseSymbolicNumeric[
	{s[a, Q]},
	2 MP[a, Q] + MP2[Q],
	TestID -> "Massive -> True: 1 spinor, 1 LVector"
];
TestCaseSymbolicNumeric[
	{s[P, Q]},
	2 MP[P, Q] + MP2[P] + MP2[Q],
	TestID -> "Massive -> True: 2 LVectors"
];

TestCaseSymbolicNumeric[
	{s[a, b, c]},
	2 MP[a, b] + 2 MP[a, c] + 2 MP[b, c],
	TestID -> "Massive -> True: 3 spinors"
];
TestCaseSymbolicNumeric[
	{s[x a, y z b, c]},
	2 x y z MP[a, b] + 2 x MP[a, c] + 2 y z MP[b, c],
	TestID -> "Massive -> True: 3 spinors (with coefficients)"
];
TestCaseSymbolicNumeric[
	{s[a, b, R]},
	2 MP[a, b] + 2 MP[a, R] + 2 MP[b, R] + MP2[R],
	TestID -> "Massive -> True: 2 spinors, 1 LVector"
];
TestCaseSymbolicNumeric[
	{s[a, Q, R]},
	2 MP[a, Q] + 2 MP[a, R] + 2 MP[Q, R] + MP2[Q] + MP2[R],
	TestID -> "Massive -> True: 1 spinor, 2 LVectors"
];
TestCaseSymbolicNumeric[
	{s[x a, y z Q, R]},
	2 x y z MP[a, Q] + 2 x MP[a, R] + 2 y z MP[Q, R] + y^2 z^2 MP2[Q] + MP2[R],
	TestID -> "Massive -> True: 1 spinor, 2 LVectors (with coefficients)"
];
TestCaseSymbolicNumeric[
	{s[P, Q, R]},
	2 MP[P, Q] + 2 MP[P, R] + 2 MP[Q, R] + MP2[P] + MP2[Q] + MP2[R],
	TestID -> "Massive -> True: 3 LVectors"
];


SetOptions[ExpandSToMPs, "Massive" -> False];

TestCaseSymbolicNumeric[
	{s[a, b]},
	2 MP[a, b],
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
