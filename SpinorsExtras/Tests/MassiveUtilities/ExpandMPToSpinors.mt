(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`MassiveUtilities`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c},
	"LVectors" -> {P, Q, R},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All,
	"SpRefMomentaLVectors" -> All,
	"SpAssocMomentaLVectors" -> All
];


SetOptions[
	{
		Test,
		TestSubexpression,
		TestCaseSymbolicNumeric,
		TestCasePatterns
	},
	ApplyToInput -> ExpandMPToSpinors
];

SetOptions[TestCaseSymbolicNumeric,
	InputWrapperN -> (SetPrecision[N[LightConeDecompose[#]], 8]&)
];

SetOptions[TestCasePatterns,
	Test -> TestCaseSymbolicNumeric
];


(* ::Section:: *)
(*Tests*)


Test[
	{MP[1, a], 1},
	1/2 Spab[1, a, 1],
	TestID -> "spinorize integer"
];


SetOptions[ExpandMPToSpinors,
	"Massive" -> True,
	"OpenMassiveSpinorChains" -> False
];

TestCaseSymbolicNumeric[
	{MP[a, a]},
	0,
	TestID -> "Massive -> True: 2 spinors same"
];
TestCaseSymbolicNumeric[
	{MP[a, b]},
	1/2 Spaa[a, b] Spbb[b, a],
	TestID -> "Massive -> True: 2 spinors"
];
TestCaseSymbolicNumeric[
	{MP[a, Q]},
	1/2 Spab[a, Q, a],
	TestID -> "Massive -> True: 1 spinor, 1 LVector"
];
TestCaseSymbolicNumeric[
	{MP[b, P]},
	1/2 Spab[b, P, b],
	TestID -> "Massive -> True: 1 spinor, 1 LVector, (alternative order)"
];
TestCaseSymbolicNumeric[
	{MP[P, Q]},
	1/2 Spab[SpM[P, +1], Q, SpM[P, +1]],
	TestID -> "Massive -> True: 2 LVectors"
];
TestSubexpression[
	{MP[P, P]},
	TestID -> "Massive -> True: 2 LVectors same"
];
TestCaseSymbolicNumeric[
	{MP[P, Q] + MP[Q, R], "UOrVFunction" -> (Switch[#, P, +1, Q, -1]&)}
	,
	1/2 Spab[SpM[P, +1], Q, SpM[P, +1]] + 1/2 Spab[SpM[Q, -1], R, SpM[Q, -1]]
	,
	TestID -> "Massive -> True: 2 LVectors, UOrVFunction"
];

TestCasePatterns[
	MP[a, b] + MP[a, c] + MP[b, c],
	{
		{
			1/2 Spab[a, b, a] + 1/2 Spab[a, c, a] + MP[b, c],
			TestID -> "Massive -> True: sum of 3 MPs massless only: a"
		}
		,
		{
			1/2 Spab[b, a, b] + MP[a, c] + 1/2 Spab[b, c, b],
			TestID -> "Massive -> True: sum of 3 MPs massless only: b"
		}
		,
		{
			1/2 Spaa[a, b] Spbb[b, a] + 1/2 Spab[a, c, a] + 1/2 Spab[b, c, b],
			TestID -> "Massive -> True: sum of 3 MPs massless only: a|b"
		}
		,
		{
			1/2 Spaa[a, b] Spbb[b, a] +
			1/2 Spaa[a, c] Spbb[c, a] +
			1/2 Spaa[b, c] Spbb[c, b]
			,
			TestID -> "Massive -> True: sum of 3 MPs massless only: no args"
		}
	},
	"Var1" -> a,
	"Var2" -> b,
	TestID -> "Massive -> True: sum of 3 MPs massless only"
];

TestCasePatterns[
	MP[a, Q] + MP[a, R] + MP[Q, R],
	{
		{
			1/2 Spab[a, Q, a] + 1/2 Spab[a, R, a] + MP[Q, R],
			TestID -> "Massive -> True: sum of 3 MPs with Massive: a"
		}
		,
		{
			1/2 Spab[SpM[Q, +1], a, SpM[Q, +1]] +
			MP[a, R] +
			1/2 Spab[SpM[Q, +1], R, SpM[Q, +1]]
			,
			TestID -> "Massive -> True: sum of 3 MPs with Massive: Q"
		}
		,
		{
			1/2 Spab[a, Q, a] +
			1/2 Spab[a, R, a] +
			1/2 Spab[SpM[Q, +1], R, SpM[Q, +1]]
			,
			TestID -> "Massive -> True: sum of 3 MPs with Massive: a|Q"
		}
		,
		{
			1/2 Spab[a, Q, a] +
			1/2 Spab[a, R, a] +
			1/2 Spab[SpM[Q, +1], R, SpM[Q, +1]]
			,
			TestID -> "Massive -> True: sum of 3 MPs with Massive: no args"
		}
	},
	"Var1" -> a,
	"Var2" -> Q,
	TestID -> "Massive -> True: sum of 3 MPs with Massive"
];

SetOptions[ExpandMPToSpinors,
	"Massive" -> False,
	"OpenMassiveSpinorChains" -> False
];

TestCaseSymbolicNumeric[
	{MP[a, b]},
	1/2 Spaa[a, b] Spbb[b, a],
	TestID -> "Massive -> False: 2 spinors"
];
TestCaseSymbolicNumeric[
	{MP[a, Q]},
	1/2 Spab[a, Q, a],
	TestID -> "Massive -> False: 1 spinor, 1 LVector"
];
TestCaseSymbolicNumeric[
	{MP[P, Q]},
	MP[P, Q],
	TestID -> "Massive -> False: 2 LVectors"
];

TestCasePatterns[
	MP[a, Q] + MP[a, R] + MP[Q, R],
	{
		{1/2 Spab[a, Q, a] + 1/2 Spab[a, R, a] + MP[Q, R],
			TestID -> "Massive -> False: sum of 3 MPs with Massive: a"}
		,
		{MP[a, Q] + MP[a, R] + MP[Q, R],
			{
				HoldForm @ Message[
					ExpandMPToSpinors::massiveFalse,
					Q,
					False,
					HoldForm[
						ExpandMPToSpinors[MP[a, Q] + MP[a, R] + MP[Q, R], Q]
					]
				]
			},
			TestID -> "Massive -> False: sum of 3 MPs with Massive: Q"}
		,
		{1/2 Spab[a, Q, a] + 1/2 Spab[a, R, a] + MP[Q, R],
			TestID -> "Massive -> False: sum of 3 MPs with Massive: a|Q"}
		,
		{1/2 Spab[a, Q, a] + 1/2 Spab[a, R, a] + MP[Q, R],
			TestID -> "Massive -> False: sum of 3 MPs with Massive: no args"}
	},
	"Var1" -> a,
	"Var2" -> Q,
	TestID -> "Massive -> False: sum of 3 MPs with Massive"
];

TestSubexpression[
	{MP[a, P], Q},
	{
		HoldForm @ Message[
			ExpandMPToSpinors::massiveFalse,
			Q,
			False, HoldForm[ExpandMPToSpinors[MP[a, P], Q]]
		]
	},
	TestID -> "Massive -> False: expand massive"
];


SetOptions[ExpandMPToSpinors,
	"Massive" -> True,
	"OpenMassiveSpinorChains" -> True
];

TestCasePatterns[
	MP[a, Q] + MP[a, R] + MP[Q, R],
	{
		{
			1/2 Spab[SpM[Q, +1], a, SpM[Q, +1]] +
			MP[a, R] +
			1/2 Spab[SpM[Q, +1], R, SpM[Q, +1]]
			,
			TestID -> "open massive sp chains: sum of 3 MPs with Massive: Q"
		}
		,
		{
			MP[a, Q] +
			1/2 Spab[SpM[R, +1], a, SpM[R, +1]] +
			1/2 Spab[SpM[R, +1], Q, SpM[R, +1]]
			,
			TestID -> "open massive sp chains: sum of 3 MPs with Massive: R"
		}
		,
		{
			1/2 Spab[SpM[Q, +1], a, SpM[Q, +1]] +
			1/2 Spab[SpM[R, +1], a, SpM[R, +1]] +
			1/2 Spaa[SpM[Q, +1], SpM[R, +1]] Spbb[SpM[R, +1], SpM[Q, +1]] +
			1/2 Spab[SpM[Q, +1], SpM[R, +1]] Spab[SpM[R, +1], SpM[Q, +1]] -
			1/4 Spab[SpM[Q, +1], SpM[Q, +1]] Spab[SpM[R, +1], SpM[R, +1]]
			,
			TestID -> "open massive sp chains: sum of 3 MPs with Massive: Q|R"
		}
		,
		{
			1/2 Spaa[SpM[Q, +1], a] Spbb[a, SpM[Q, +1]] +
			1/2 Spab[SpM[Q, +1], a] Spab[a, SpM[Q, +1]] +
			1/2 Spaa[SpM[R, +1], a] Spbb[a, SpM[R, +1]] +
			1/2 Spab[SpM[R, +1], a] Spab[a, SpM[R, +1]] +
			1/2 Spaa[SpM[Q, +1], SpM[R, +1]] Spbb[SpM[R, +1], SpM[Q, +1]] +
			1/2 Spab[SpM[Q, +1], SpM[R, +1]] Spab[SpM[R, +1], SpM[Q, +1]] -
			1/4 Spab[SpM[Q, +1], SpM[Q, +1]] Spab[SpM[R, +1], SpM[R, +1]]
			,
			TestID ->
				"open massive sp chains: sum of 3 MPs with Massive: no args"
		}
	},
	"Var1" -> Q,
	"Var2" -> R,
	TestID -> "open massive sp chains: sum of 3 MPs with Massive"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
