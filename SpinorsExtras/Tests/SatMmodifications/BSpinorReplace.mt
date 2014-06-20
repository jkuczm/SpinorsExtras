(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`SatMmodifications`",
	"MUnitExtras`Package`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {tR, r, left, inside, inside2, right},
	"LVectors" -> {L, P, Q, R}
];


AssignTestFeatures[SmTestCase]

SmTestCase[sm_, smBA_, opts:OptionsPattern[]] :=
	TestCaseEnvironment[
		{opts, Options[SmTestCase]}
		,
		Test[
			{sm[tR]},
			smBA[r, tR],
			TestFailureMessage -> "Sm"
		];
		Test[
			{smBA[tR, x]},
			smBA[r, x],
			TestFailureMessage -> "SmBA: A to replace"
		];
		TestSubexpression[
			{smBA[x, tR]},
			TestFailureMessage -> "SmBA: B to replace"
		];
		,
		"CommonOptionsFor" -> {Test, TestSubexpression}
	]


SetOptions[
	{
		Test, TestSubexpression,
		TestCaseSpxyLeftInsideRight, SmTestCase
	},
	ApplyToInput -> (BSpinorReplace[#, tR, r]&)
];


SetOptions[TestCaseSpxyLeftInsideRight,
	"ReplaceBy" -> tR,
	"Left" -> left,
	"Inside" -> inside,
	"Inside2" -> inside2,
	"Right" -> right
];


(* ::Section:: *)
(*Tests*)


TestCaseSpxyLeftInsideRight[
	Spbb[left, L, inside, P, inside2, Q, R, right],
	{
		{Spbb[r, L, inside, P, inside2, Q, R, right],
			TestID -> "Spbb: left"},
		{Spba[left, L, tR] Spbb[r, P, inside2, Q, R, right],
			TestID -> "Spbb: inside"},
		{Spbb[left, L, inside, P, inside2, Q, R, r],
			TestID -> "Spbb: right"}
		,
		{Spba[r, L, tR] Spbb[r, P, inside2, Q, R, right],
			TestID -> "Spbb: left and inside"},
		{Spbb[r, L, inside, P, inside2, Q, R, r],
			TestID -> "Spbb: left and right"},
		{Spba[left, L, tR] Spbb[r, P, inside2, Q, R, r],
			TestID -> "Spbb: inside and right"}
		,
		{Spba[r, L, tR] Spbb[r, P, inside2, Q, R, r],
			TestID -> "Spbb: left, inside and right"},
		{Spba[r, L, tR] Spba[r, P, tR] Spbb[r, Q, R, r],
			TestID -> "Spbb: left, inside double and right"}
	},
	TestID -> "Spbb"
];
TestCaseSpxyLeftInsideRight[
	Spaa[left, L, inside, P, inside2, Q, R, right],
	{
		{Spaa[tR, L, inside, P, inside2, Q, R, right],
			TestID -> "Spaa: left"},
		{Spab[left, L, r] Spaa[tR, P, inside2, Q, R, right],
			TestID -> "Spaa: inside"},
		{Spaa[left, L, inside, P, inside2, Q, R, tR],
			TestID -> "Spaa: right"}
		,
		{Spab[tR, L, r] Spaa[tR, P, inside2, Q, R, right],
			TestID -> "Spaa: left and inside"},
		{Spaa[tR, L, inside, P, inside2, Q, R, tR],
			TestID -> "Spaa: left and right"},
		{Spab[left, L, r] Spaa[tR, P, inside2, Q, R, tR],
			TestID -> "Spaa: inside and right"}
		,
		{Spab[tR, L, r] Spaa[tR, P, inside2, Q, R, tR],
			TestID -> "Spaa: left, inside and right"},
		{Spab[tR, L, r] Spab[tR, P, r] Spaa[tR, Q, R, tR],
			TestID -> "Spaa: left, inside double and right"}
	},
	TestID -> "Spaa"
];
TestCaseSpxyLeftInsideRight[
	Spba[left, L, inside, P, inside2, Q, right],
	{
		{Spba[r, L, inside, P, inside2, Q, right],
			TestID -> "Spba: left"},
		{Spba[left, L, tR] Spba[r, P, inside2, Q, right],
			TestID -> "Spba: inside"},
		{Spba[left, L, inside, P, inside2, Q, tR],
			TestID -> "Spba: right"}
		,
		{Spba[r, L, tR] Spba[r, P, inside2, Q, right],
			TestID -> "Spba: left and inside"},
		{Spba[r, L, inside, P, inside2, Q, tR],
			TestID -> "Spba: left and right"},
		{Spba[left, L, tR] Spba[r, P, inside2, Q, tR],
			TestID -> "Spba: inside and right"}
		,
		{Spba[r, L, tR] Spba[r, P, inside2, Q, tR],
			TestID -> "Spba: left, inside and right"},
		{Spba[r, L, tR] Spba[r, P, tR] Spba[r, Q, tR],
			TestID -> "Spba: left, inside double and right"}
	},
	TestID -> "Spba"
];
TestCaseSpxyLeftInsideRight[
	Spab[left, L, inside, P, inside2, Q, right],
	{
		{Spab[tR, L, inside, P, inside2, Q, right],
			TestID -> "Spab: left"},
		{Spab[left, L, r] Spab[tR, P, inside2, Q, right],
			TestID -> "Spab: inside"},
		{Spab[left, L, inside, P, inside2, Q, r],
			TestID -> "Spab: right"}
		,
		{Spab[tR, L, r] Spab[tR, P, inside2, Q, right],
			TestID -> "Spab: left and inside"},
		{Spab[tR, L, inside, P, inside2, Q, r],
			TestID -> "Spab: left and right"},
		{Spab[left, L, r] Spab[tR, P, inside2, Q, r],
			TestID -> "Spab: inside and right"}
		,
		{Spab[tR, L, r] Spab[tR, P, inside2, Q, r],
			TestID -> "Spab: left, inside and right"},
		{Spab[tR, L, r] Spab[tR, P, r] Spab[tR, Q, r],
			TestID -> "Spab: left, inside double and right"}
	},
	TestID -> "Spab"
];


Test[
	{Spab[x, tR, y]},
	Spaa[x, tR] Spbb[r, y] + Spab[x, r] Spab[tR, y],
	TestID -> "Spab with non-spinors on ends"
];


Test[
	{#[tR]},
	#[r],
	TestID -> "2,4-comp spinors: One arg replaced"
]& /@
	{Lat, CLat, USpb, UbarSpb};

TestSubexpression[
	{#[tR]},
	TestID -> "2,4-comp spinors: One arg not-replaced"
]& /@
	{La, CLa, USpa, UbarSpa};


SmTestCase[Sm, SmBA, TestID -> "SmBA"];
SmTestCase[Sm4, SmBA4, TestID -> "SmBA4"];
SmTestCase[Sm2, SmBA2, TestID -> "SmBA2"];
SmTestCase[CSm2, CSmBA2, TestID -> "CSmBA2"];


Test[
	{MP[x, tR]},
	1/2 Spab[tR, x, r],
	TestID -> "MP with nonspinor"
];
Test[
	{MP[1, 2], 1, r},
	1/2 Spab[1, 2, r],
	ApplyToInput -> BSpinorReplace,
	TestID -> "MP first"
];
Test[
	{MP[1, 2], 2, r},
	1/2 Spab[2, 1, r],
	ApplyToInput -> BSpinorReplace,
	TestID -> "MP second"
];


Test[
	{s[x, tR]},
	MP2[x] + Spba[r, x, tR],
	TestID -> "s: one left"
];
Test[
	{s[x, y, tR, z]},
	s[x, y, z] + Spba[r, x, tR] + Spba[r, y, tR] + Spba[r, z, tR],
	TestID -> "s: three left"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
