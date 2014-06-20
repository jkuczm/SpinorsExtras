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
			smBA[tR, r],
			TestFailureMessage -> "Sm"
		];
		Test[
			{smBA[x, tR]},
			smBA[x, r],
			TestFailureMessage -> "SmBA: A to replace"
		];
		TestSubexpression[
			{smBA[tR, x]},
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
	ApplyToInput -> (ASpinorReplace[#, tR, r]&)
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
	Spaa[left, L, inside, P, inside2, Q, R, right],
	{
		{Spaa[r, L, inside, P, inside2, Q, R, right],
			TestID -> "Spaa: left"},
		{Spab[left, L, tR] Spaa[r, P, inside2, Q, R, right],
			TestID -> "Spaa: inside"},
		{Spaa[left, L, inside, P, inside2, Q, R, r],
			TestID -> "Spaa: right"}
		,
		{Spab[r, L, tR] Spaa[r, P, inside2, Q, R, right],
			TestID -> "Spaa: left and inside"},
		{Spaa[r, L, inside, P, inside2, Q, R, r],
			TestID -> "Spaa: left and right"},
		{Spab[left, L, tR] Spaa[r, P, inside2, Q, R, r],
			TestID -> "Spaa: inside and right"}
		,
		{Spab[r, L, tR] Spaa[r, P, inside2, Q, R, r],
			TestID -> "Spaa: left, inside and right"},
		{Spab[r, L, tR] Spab[r, P, tR] Spaa[r, Q, R, r],
			TestID -> "Spaa: left, inside double and right"}
	}
];
TestCaseSpxyLeftInsideRight[
	Spbb[left, L, inside, P, inside2, Q, R, right],
	{
		{Spbb[tR, L, inside, P, inside2, Q, R, right],
			TestID -> "Spbb: left"},
		{Spba[left, L, r] Spbb[tR, P, inside2, Q, R, right],
			TestID -> "Spbb: inside"},
		{Spbb[left, L, inside, P, inside2, Q, R, tR],
			TestID -> "Spbb: right"}
		,
		{Spba[tR, L, r] Spbb[tR, P, inside2, Q, R, right],
			TestID -> "Spbb: left and inside"},
		{Spbb[tR, L, inside, P, inside2, Q, R, tR],
			TestID -> "Spbb: left and right"},
		{Spba[left, L, r] Spbb[tR, P, inside2, Q, R, tR],
			TestID -> "Spbb: inside and right"}
		,
		{Spba[tR, L, r] Spbb[tR, P, inside2, Q, R, tR],
			TestID -> "Spbb: left, inside and right"},
		{Spba[tR, L, r] Spba[tR, P, r] Spbb[tR, Q, R, tR],
			TestID -> "Spbb: left, inside double and right"}
	}
];
TestCaseSpxyLeftInsideRight[
	Spab[left, L, inside, P, inside2, Q, right],
	{
		{Spab[r, L, inside, P, inside2, Q, right],
			TestID -> "Spab: left"},
		{Spab[left, L, tR] Spab[r, P, inside2, Q, right],
			TestID -> "Spab: inside"},
		{Spab[left, L, inside, P, inside2, Q, tR],
			TestID -> "Spab: right"}
		,
		{Spab[r, L, tR] Spab[r, P, inside2, Q, right],
			TestID -> "Spab: left and inside"},
		{Spab[r, L, inside, P, inside2, Q, tR],
			TestID -> "Spab: left and right"},
		{Spab[left, L, tR] Spab[r, P, inside2, Q, tR],
			TestID -> "Spab: inside and right"}
		,
		{Spab[r, L, tR] Spab[r, P, inside2, Q, tR],
			TestID -> "Spab: left, inside and right"},
		{Spab[r, L, tR] Spab[r, P, tR] Spab[r, Q, tR],
			TestID -> "Spab: left, inside double and right"}
	}
];
TestCaseSpxyLeftInsideRight[
	Spba[left, L, inside, P, inside2, Q, right],
	{
		{Spba[tR, L, inside, P, inside2, Q, right],
			TestID -> "Spba: left"},
		{Spba[left, L, r] Spba[tR, P, inside2, Q, right],
			TestID -> "Spba: inside"},
		{Spba[left, L, inside, P, inside2, Q, r],
			TestID -> "Spba: right"}
		,
		{Spba[tR, L, r] Spba[tR, P, inside2, Q, right],
			TestID -> "Spba: left and inside"},
		{Spba[tR, L, inside, P, inside2, Q, r],
			TestID -> "Spba: left and right"},
		{Spba[left, L, r] Spba[tR, P, inside2, Q, r],
			TestID -> "Spba: inside and right"}
		,
		{Spba[tR, L, r] Spba[tR, P, inside2, Q, r],
			TestID -> "Spba: left, inside and right"},
		{Spba[tR, L, r] Spba[tR, P, r] Spba[tR, Q, r],
			TestID -> "Spba: left, inside double and right"}
	}
];


Test[
	{Spba[x, tR, y]},
	Spbb[x, tR] Spaa[r, y] + Spba[x, r] Spba[tR, y],
	TestID -> "Spba with non-spinors on ends"
];


Test[
	{#[tR]},
	#[r],
	TestID -> "2,4-comp spinors: One arg replaced"
]& /@
	{La, CLa, USpa, UbarSpa};

TestSubexpression[
	{#[tR]},
	TestID -> "2,4-comp spinors: One arg not-replaced"
]& /@
	{Lat, CLat, USpb, UbarSpb};


SmTestCase[Sm, SmBA, TestID -> "SmBA"];
SmTestCase[Sm4, SmBA4, TestID -> "SmBA4"];
SmTestCase[Sm2, SmBA2, TestID -> "SmBA2"];
SmTestCase[CSm2, CSmBA2, TestID -> "CSmBA2"];


Test[
	{MP[x, tR]},
	1/2 Spab[r, x, tR],
	TestID -> "MP with nonspinor"
];
Test[
	{MP[1, 2], 1, r},
	1/2 Spba[1, 2, r],
	ApplyToInput -> ASpinorReplace,
	TestID -> "MP first"
];
Test[
	{MP[1, 2], 2, r},
	1/2 Spba[2, 1, r],
	ApplyToInput -> ASpinorReplace,
	TestID -> "MP second"
];


Test[
	{s[x, tR]},
	MP2[x] + Spab[r, x, tR],
	TestID -> "s: one left"
];
Test[
	{s[x, y, tR, z]},
	s[x, y, z] + Spab[r, x, tR] + Spab[r, y, tR] + Spab[r, z, tR],
	TestID -> "s: three left"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
