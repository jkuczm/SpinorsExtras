(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`RefUtilities`",
	"SpinorsExtras`Ref`", (* SpRef *)
	"SpinorsExtras`Massive`", (* SpAssoc, SpM *)
	"SpinorsExtras`Pol`" (* PolVec *)
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P, Q, R},
	"PlusMinusOnes" -> {uv}
];


SetOptions[{TestCasePatterns, TestCasePatternsDefault},
	ApplyToInput -> ExplicitRef
];
SetOptions[{TestCasePatterns, TestCasePatternsDefault},
	"Var1" -> P,
	"Var2" -> Q
];
SetOptions[TestCasePatternsDefault, Test -> TestSubexpression];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*SpAssoc*)


TestCasePatterns[
	SpAssoc[P],
	{
		{SpAssoc[P, SpRef[P]], TestID -> "SpAssoc: implicit: same"},
		{SpAssoc[P], TestID -> "SpAssoc: implicit: different"},
		{SpAssoc[P, SpRef[P]], TestID -> "SpAssoc: implicit: same|different"},
		{SpAssoc[P, SpRef[P]], TestID -> "SpAssoc: implicit: no args"}
	},
	TestID -> "SpAssoc: implicit"
];


TestCasePatternsDefault[
	SpAssoc[P, a],
	TestID -> "SpAssoc: explicit massless"
];


TestCasePatternsDefault[
	SpAssoc[P, Q],
	TestID -> "SpAssoc: explicit massive"
];


(* ::Subsection:: *)
(*SpM*)


TestCasePatterns[
	SpM[P, uv],
	{
		{SpM[P, uv, SpRef[P]], TestID -> "SpM: implicit: same"},
		{SpM[P, uv], TestID -> "SpM: implicit: different"},
		{SpM[P, uv, SpRef[P]], TestID -> "SpM: implicit: same|different"},
		{SpM[P, uv, SpRef[P]], TestID -> "SpM: implicit: no args"}
	},
	TestID -> "SpM: implicit"
];


TestCasePatternsDefault[
	SpM[P, uv, a],
	TestID -> "SpM: explicit"
];


(* ::Subsection:: *)
(*PolVec*)


TestCasePatterns[
	PolVec[P, pol],
	{
		{PolVec[P, pol, SpRef[P]], TestID -> "PolVec: implicit: same"},
		{PolVec[P, pol], TestID -> "PolVec: implicit: different"},
		{PolVec[P, pol, SpRef[P]],
			TestID -> "PolVec: implicit: same|different"},
		{PolVec[P, pol, SpRef[P]], TestID -> "PolVec: implicit: no args"}
	},
	TestID -> "PolVec: implicit"
];


TestCasePatternsDefault[
	PolVec[P, pol, a],
	TestID -> "PolVec: explicit"
];


(* ::Subsection:: *)
(*Complicated expression*)


TestCasePatterns[
	Spaa[SpAssoc[Q, a], PolVec[P, 1]]
		* MP[PolVec[Q, pol], SpAssoc[P]]
		* Spab[SpM[Q, uv], PolVec[Q, -1, b], SpM[R, 1]]
	,
	{
		{
			Spaa[SpAssoc[Q, a], PolVec[P, 1, SpRef[P]]]
				* MP[PolVec[Q, pol], SpAssoc[P, SpRef[P]]]
				* Spab[SpM[Q, uv], PolVec[Q, -1, b], SpM[R, 1]]
			,
			TestID -> "Complicated expression: P"
		},
		{
			Spaa[SpAssoc[Q, a], PolVec[P, 1]]
				* MP[PolVec[Q, pol, SpRef[Q]], SpAssoc[P]]
				* Spab[SpM[Q, uv, SpRef[Q]], PolVec[Q, -1, b], SpM[R, 1]]
			,
			TestID -> "Complicated expression: Q"
		},
		{
			Spaa[SpAssoc[Q, a], PolVec[P, 1, SpRef[P]]]
				* MP[PolVec[Q, pol, SpRef[Q]], SpAssoc[P, SpRef[P]]]
				* Spab[SpM[Q, uv, SpRef[Q]], PolVec[Q, -1, b], SpM[R, 1]]
			,
			TestID -> "Complicated expression: P|Q"
		},
		{
			Spaa[SpAssoc[Q, a], PolVec[P, 1, SpRef[P]]]
				* MP[PolVec[Q, pol, SpRef[Q]], SpAssoc[P, SpRef[P]]]
				* Spab[
					SpM[Q, uv, SpRef[Q]], PolVec[Q, -1, b], SpM[R, 1, SpRef[R]]
				]
			,
			TestID -> "Complicated expression: no args"
		}
	},
	TestID -> "Complicated expression"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
