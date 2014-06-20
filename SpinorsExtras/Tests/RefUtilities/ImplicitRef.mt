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
	"Spinors" -> {a, b, c},
	"LVectors" -> {P, Q, R}
];


SetOptions[{TestCasePatterns, TestCasePatternsDefault},
	ApplyToInput -> ImplicitRef
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


(* ::Subsubsection:: *)
(*don't HideNonDefault*)


SetOptions[ImplicitRef, "HideNonDefault" -> False];


TestCasePatterns[
	SpAssoc[P, SpRef[P]],
	{
		{
			SpAssoc[P],
			TestID -> "SpAssoc: don't HideNonDefault: explicit default: same"
		},
		{
			SpAssoc[P, SpRef[P]],
			TestID ->
				"SpAssoc: don't HideNonDefault: explicit default: different"
		},
		{
			SpAssoc[P],
			TestID -> "SpAssoc: don't HideNonDefault: \
explicit default: same|different"
		},
		{
			SpAssoc[P],
			TestID ->
				"SpAssoc: don't HideNonDefault: explicit default: no args"
		}
	},
	TestID -> "SpAssoc: don't HideNonDefault: explicit default"
];


TestCaseRepeated[
	TestCasePatternsDefault[
		SpAssoc[P, ref],
		TestID -> "SpAssoc: don't HideNonDefault: explicit non-default"
	];
	,
	TestID -> "SpAssoc: don't HideNonDefault: explicit non-default: test case",
	RepeatFor -> Distribute[ref -> {SpRef[Q], a, Q}, List]
];


TestCasePatternsDefault[
	SpAssoc[P],
	TestID -> "SpAssoc: don't HideNonDefault: implicit"
];


(* ::Subsubsection:: *)
(*HideNonDefault*)


SetOptions[ImplicitRef, "HideNonDefault" -> True];


TestCaseRepeated[
	TestCasePatterns[
		SpAssoc[P, ref],
		{
			{SpAssoc[P],
				TestID -> "SpAssoc: HideNonDefault: explicit: same"},
			{SpAssoc[P, ref],
				TestID -> "SpAssoc: HideNonDefault: explicit: different"},
			{SpAssoc[P],
				TestID -> "SpAssoc: HideNonDefault: explicit: same|different"},
			{SpAssoc[P],
				TestID -> "SpAssoc: HideNonDefault: explicit: no args"}
		},
		TestID -> "SpAssoc: HideNonDefault: explicit"
	];
	,
	TestID -> "SpAssoc: HideNonDefault: explicit: test case",
	RepeatFor -> Distribute[ref -> {SpRef[P], SpRef[Q], a, Q}, List]
];


TestCasePatternsDefault[
	SpAssoc[P],
	TestID -> "SpAssoc: HideNonDefault: implicit"
];


(* ::Subsection:: *)
(*SpM, PolVec*)


TestCaseRepeated[
	(*don't HideNonDefault*)
	SetOptions[ImplicitRef, "HideNonDefault" -> False];
	
	
	TestCasePatterns[
		label[P, x, SpRef[P]],
		{
			{
				label[P, x],
				TestID -> "SpM, PolVec: don't HideNonDefault: \
explicit default: same"
			},
			{
				label[P, x, SpRef[P]],
				TestID -> "SpM, PolVec: don't HideNonDefault: \
explicit default: different"
			},
			{
				label[P, x],
				TestID -> "SpM, PolVec: don't HideNonDefault: \
explicit default: same|different"
			},
			{
				label[P, x],
				TestID -> "SpM, PolVec: don't HideNonDefault: \
explicit default: no args"
			}
		},
		TestID -> "SpM, PolVec: don't HideNonDefault: explicit default"
	];
	
	
	TestCaseRepeated[
		TestCasePatternsDefault[
			label[P, x, ref],
			TestID -> "SpM, PolVec: don't HideNonDefault: explicit non-default"
		];
		,
		TestID -> "SpM, PolVec: don't HideNonDefault: \
explicit non-default: test case",
		RepeatFor -> Distribute[ref -> {SpRef[Q], a}, List]
	];
	
	
	TestCasePatternsDefault[
		label[P, x],
		TestID -> "SpM, PolVec: don't HideNonDefault: implicit"
	];
	
	
	(* ::Subsubsection:: *)
	(*HideNonDefault*)
	
	
	SetOptions[ImplicitRef, "HideNonDefault" -> True];
	
	
	TestCaseRepeated[
		TestCasePatterns[
			label[P, x, ref],
			{
				{
					label[P, x],
					TestID -> "SpM, PolVec: HideNonDefault: explicit: same"
				},
				{
					label[P, x, ref],
					TestID ->
						"SpM, PolVec: HideNonDefault: explicit: different"
				},
				{
					label[P, x],
					TestID ->
						"SpM, PolVec: HideNonDefault: explicit: same|different"
				},
				{
					label[P, x],
					TestID -> "SpM, PolVec: HideNonDefault: explicit: no args"
				}
			},
			TestID -> "SpM, PolVec: HideNonDefault: explicit"
		];
		,
		TestID -> "SpM, PolVec: HideNonDefault: explicit: test case",
		RepeatFor -> Distribute[ref -> {SpRef[P], SpRef[Q], a}, List]
	];
	
	
	TestCasePatternsDefault[
		label[P, x],
		TestID -> "SpM, PolVec: HideNonDefault: implicit"
	];
	,
	TestID -> "SpM, PolVec: test case",
	RepeatFor -> Distribute[label -> {SpM, PolVec}, List]
];


(* ::Subsection:: *)
(*Complicated expression*)


(* ::Subsubsection:: *)
(*don't HideNonDefault*)
	
	
SetOptions[ImplicitRef, "HideNonDefault" -> False];


TestCasePatterns[
	Spaa[SpAssoc[Q, SpRef[Q]], PolVec[P, 1, SpRef[P]]]
		* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
		* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
		* Spbb[SpAssoc[P, c], SpM[R, -1, SpRef[R]]]
	,
	{
		{
			Spaa[SpAssoc[Q, SpRef[Q]], PolVec[P, 1]]
				* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
				* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
				* Spbb[SpAssoc[P, c], SpM[R, -1, SpRef[R]]]
			,
			TestID -> "Complicated expression: don't HideNonDefault: P"
		},
		{
			Spaa[SpAssoc[Q], PolVec[P, 1, SpRef[P]]]
				* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
				* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
				* Spbb[SpAssoc[P, c], SpM[R, -1, SpRef[R]]]
			,
			TestID -> "Complicated expression: don't HideNonDefault: Q"
		},
		{
			Spaa[SpAssoc[Q], PolVec[P, 1]]
				* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
				* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
				* Spbb[SpAssoc[P, c], SpM[R, -1, SpRef[R]]]
			,
			TestID -> "Complicated expression: don't HideNonDefault: P|Q"
		},
		{
			Spaa[SpAssoc[Q], PolVec[P, 1]]
				* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
				* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
				* Spbb[SpAssoc[P, c], SpM[R, -1]]
			,
			TestID -> "Complicated expression: don't HideNonDefault: no args"
		}
	},
	TestID -> "Complicated expression: don't HideNonDefault"
];


(* ::Subsubsection:: *)
(*HideNonDefault*)
	
	
SetOptions[ImplicitRef, "HideNonDefault" -> True];


TestCasePatterns[
	Spaa[SpAssoc[Q, SpRef[Q]], PolVec[P, 1, SpRef[P]]]
		* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
		* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
		* Spbb[SpAssoc[P, c], SpM[R, -1, SpRef[R]]]
	,
	{
		{
			Spaa[SpAssoc[Q, SpRef[Q]], PolVec[P, 1]]
				* MP[PolVec[Q, pol], SpAssoc[P]]
				* Spab[SpM[Q, uv], PolVec[Q, 0, a], SpM[R, 1, b]]
				* Spbb[SpAssoc[P], SpM[R, -1, SpRef[R]]]
			,
			TestID -> "Complicated expression: HideNonDefault: P"
		},
		{
			Spaa[SpAssoc[Q], PolVec[P, 1, SpRef[P]]]
				* MP[PolVec[Q, pol], SpAssoc[P, SpRef[Q]]]
				* Spab[SpM[Q, uv], PolVec[Q, 0], SpM[R, 1, b]]
				* Spbb[SpAssoc[P, c], SpM[R, -1, SpRef[R]]]
			,
			TestID -> "Complicated expression: HideNonDefault: Q"
		},
		{
			Spaa[SpAssoc[Q], PolVec[P, 1]]
				* MP[PolVec[Q, pol], SpAssoc[P]]
				* Spab[SpM[Q, uv], PolVec[Q, 0], SpM[R, 1, b]]
				* Spbb[SpAssoc[P], SpM[R, -1, SpRef[R]]]
			,
			TestID -> "Complicated expression: HideNonDefault: P|Q"
		},
		{
			Spaa[SpAssoc[Q], PolVec[P, 1]]
				* MP[PolVec[Q, pol], SpAssoc[P]]
				* Spab[SpM[Q, uv], PolVec[Q, 0], SpM[R, 1]]
				* Spbb[SpAssoc[P], SpM[R, -1]]
			,
			TestID -> "Complicated expression: HideNonDefault: no args"
		}
	},
	TestID -> "Complicated expression: HideNonDefault"
];


(* ::Subsection:: *)
(*Explicit option*)


Test[
	ImplicitRef[SpAssoc[P, a], "HideNonDefault" -> True],
	SpAssoc[P],
	TestID -> "Explicit option"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
