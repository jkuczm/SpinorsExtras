(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`SatMmodifications`",
	"SpinorsExtras`Massive`",
	"SpinorsExtras`MassiveUtilities`"
}];
SetUpSpinorsTestEnvironment[
	"IntegerSpinors" -> Range[4],
	"LVectors" -> {P, Q, L, R},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All,
	"SpAssocMomentaLVectors" -> All
];

uQ = SpM[Q, +1];
vL = SpM[L, -1];


SetOptions[TestCaseSymbolicNumeric,
	N -> (SetPrecision[N[LightConeDecompose[#]], 10]&)
];


SetOptions[
	{
		TestCasePatterns,
		TestCasePatternsDefault,
		TestSubexpression,
		TestCaseSymbolicNumeric
	}
	,
	ApplyToInput -> SpOpen
];


SetOptions[{TestCasePatterns, TestCasePatternsDefault},
	"Var1" -> Sp[1],
	"Var2" -> Sp[2]
];

SetOptions[TestCasePatterns,
	Test -> TestCaseSymbolicNumeric,
	Options[TestCaseSymbolicNumeric, InputWrapper]
];

SetOptions[TestCasePatternsDefault,
	Test -> TestSubexpression
];


(* ::Section:: *)
(*Tests*)


TestSubexpression[
	{Spab[1, P, 2], P},
	TestID -> "Spab explicit opening on no-Spinor"
];
TestCasePatternsDefault[
	Spab[1, P, 2],
	TestID -> "Spab only massive inside"
];


TestCaseSymbolicNumeric[
	{Spab[1, 2, 3], 2},
	Spaa[1, 2] Spbb[2, 3],
	TestID -> "Spab integer arg"
];


TestCasePatterns[
	Spab[2, 1, 3],
	{
		{Spaa[2, 1] Spbb[1, 3], TestID -> "Spab massles: Sp[1]"},
		{Spab[2, 1, 3], TestID -> "Spab massles: Sp[2]"},
		{Spaa[2, 1] Spbb[1, 3], TestID -> "Spab massles: Sp[1]|Sp[2]"},
		{Spaa[2, 1] Spbb[1, 3], TestID -> "Spab massles: no args"}
	},
	TestID -> "Spab massles"
];
TestCasePatterns[
	Spab[uQ, 2, 1],
	{
		{Spab[uQ, 2, 1], TestID -> "Spab start massive: Sp[1]"},
		{Spaa[uQ, 2] Spbb[2, 1], TestID -> "Spab start massive: Sp[2]"},
		{Spaa[uQ, 2] Spbb[2, 1], TestID -> "Spab start massive: Sp[1]|Sp[2]"},
		{Spaa[uQ, 2] Spbb[2, 1], TestID -> "Spab start massive: no args"}
	}
];
TestCasePatterns[
	Spbb[3, 1, 2, vL],
	{
		{Spbb[3, 1] Spab[1, 2, vL], TestID -> "Spbb end massive: Sp[1]"},
		{Spba[3, 1, 2] Spbb[2, vL], TestID -> "Spbb end massive: Sp[2]"},
		{
			Spbb[3, 1] Spaa[1, 2] Spbb[2, vL],
			TestID -> "Spbb end massive: Sp[1]|Sp[2]"
		},
		{
			Spbb[3, 1] Spaa[1, 2] Spbb[2, vL],
			TestID -> "Spbb end massive: no args"
		}
	}
];


SetOptions[SpOpen, "BothEndsMassive" -> False];

TestCasePatternsDefault[
	Spba[uQ, 1, 3, 2, vL],
	TestID -> "Spba start, end massive, \"BothEndsMassive\"->False"
];


SetOptions[SpOpen, "BothEndsMassive" -> True];

TestCasePatterns[
	Spba[uQ, 1, 3, 2, vL],
	{
		{
			Spbb[uQ, 1] Spaa[1, 3, 2, vL] + Spba[uQ, 1] Spba[1, 3, 2, vL],
			TestID ->
				"Spba start, end massive, \"BothEndsMassive\"->True: Sp[1]"
		}
		,
		{
			Spbb[uQ, 1, 3, 2] Spaa[2, vL] + Spba[uQ, 1, 3, 2] Spba[2, vL],
			TestID ->
				"Spba start, end massive, \"BothEndsMassive\"->True: Sp[2]"
		}
		,
		{
			Spbb[uQ, 1] Spab[1, 3, 2] Spaa[2, vL] +
			Spba[uQ, 1] Spba[1, 3, 2] Spba[2, vL],
			TestID ->
				"Spba start, end massive, \"BothEndsMassive\"->True: Sp[1]|Sp[2]"
		}
		,
		{
			Spbb[uQ, 1] Spaa[1, 3] Spbb[3, 2] Spaa[2, vL]
			+ Spba[uQ, 1] Spbb[1, 3] Spaa[3, 2] Spba[2, vL],
			TestID ->
				"Spba start, end massive, \"BothEndsMassive\"->True: no args"
		}
	}
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
