(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Composite`",
	"SpinorsExtras`Massive`", (* SpM *)
	"SpinorsExtras`MassiveUtilities`", (* LightConeDecompose *)
	"MUnitExtras`Package`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, q1, q2, nonNumSp1, nonNumSp2},
	"LVectors" -> {P, Q, lVNoAssoc1, lVNoAssoc2, nonNumLV1, nonNumLV2},
	"RandomMomentaSpinors" -> {a, b, q1, q2},
	"RandomMomentaLVectors" -> {P, Q, lVNoAssoc1, lVNoAssoc2},
	"SpRefMomentaLVectors" -> {P, Q},
	"SpAssocMomentaLVectors" -> {P, Q},
	"SpAssocMomentaLVectorRefPairs" -> {{P, q1}, {Q, q2}}
];


AssignTestFeatures[TestCase]

TestCase[b_, a_, opts:OptionsPattern[]] :=
	TestCaseEnvironment[
		{opts, Options[TestCase]}
		,
		TestNull[
			DeclareLVectorMomentum[LvBA[b, a]],
			TestFailureMessage -> "DeclareLVectorMomentum evaluation"
		];
		Test[
			Num4V[LvBA[b, a]]
			,
			(Spba[b, #, a] / 2) & /@ {Gamma0, Gamma1, Gamma2, Gamma3}
				// LightConeDecompose // N
			,
			TestFailureMessage -> "Num4V",
			EquivalenceFunction -> (TrueQ[Equal[##]]&)
		];
		,
		"CommonOptionsFor" -> {TestNull, Test}
	];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Succesful declarations*)


TestCase[b, a, TestID -> "massless, massless"]


TestCase[SpM[P, +1], a, TestID -> "massive + impl ref, massless"]
TestCase[SpM[P, -1], a, TestID -> "massive - impl ref, massless"]
TestCase[SpM[P, +1, q1], a, TestID -> "massive + expl ref, massless"]
TestCase[SpM[P, -1, q1], a, TestID -> "massive - expl ref, massless"]


TestCase[b, SpM[Q, +1], TestID -> "massless, massive + impl ref"]
TestCase[b, SpM[Q, -1], TestID -> "massless, massive - impl ref"]
TestCase[b, SpM[Q, +1, q2], TestID -> "massless, massive + expl ref"]
TestCase[b, SpM[Q, -1, q2], TestID -> "massless, massive - expl ref"]


TestCase[
	SpM[P, +1], SpM[Q, +1],
	TestID -> "massive + impl ref, massive + impl ref"
]
TestCase[
	SpM[P, +1], SpM[Q, -1],
	TestID -> "massive + impl ref, massive - impl ref"
]
TestCase[
	SpM[P, +1], SpM[Q, +1, q2],
	TestID -> "massive + impl ref, massive + expl ref"
]
TestCase[
	SpM[P, +1], SpM[Q, -1, q2],
	TestID -> "massive + impl ref, massive - expl ref"
]

TestCase[
	SpM[P, -1], SpM[Q, +1],
	TestID -> "massive - impl ref, massive + impl ref"
]
TestCase[
	SpM[P, -1], SpM[Q, -1],
	TestID -> "massive - impl ref, massive - impl ref"
]
TestCase[
	SpM[P, -1], SpM[Q, +1, q2],
	TestID -> "massive - impl ref, massive + expl ref"
]
TestCase[
	SpM[P, -1], SpM[Q, -1, q2],
	TestID -> "massive - impl ref, massive - expl ref"
]

TestCase[
	SpM[P, +1, q1], SpM[Q, +1],
	TestID -> "massive + expl ref, massive + impl ref"
]
TestCase[
	SpM[P, +1, q1], SpM[Q, -1],
	TestID -> "massive + expl ref, massive - impl ref"
]
TestCase[
	SpM[P, +1, q1], SpM[Q, +1, q2],
	TestID -> "massive + expl ref, massive + expl ref"
]
TestCase[
	SpM[P, +1, q1], SpM[Q, -1, q2],
	TestID -> "massive + expl ref, massive - expl ref"
]

TestCase[
	SpM[P, -1, q1], SpM[Q, +1],
	TestID -> "massive - expl ref, massive + impl ref"
]
TestCase[
	SpM[P, -1, q1], SpM[Q, -1],
	TestID -> "massive - expl ref, massive - impl ref"
]
TestCase[
	SpM[P, -1, q1], SpM[Q, +1, q2],
	TestID -> "massive - expl ref, massive + expl ref"
]
TestCase[
	SpM[P, -1, q1], SpM[Q, -1, q2],
	TestID -> "massive - expl ref, massive - expl ref"
]


(* ::Subsection:: *)
(*Automatic declarations of default values for needed spinors*)


TestCase[
	SpM[lVNoAssoc1, +1], SpM[Q, +1],
	TestID -> "massive + impl ref assoc not declared, massive + impl ref"
]
TestTrue[
	Spinors`Private`NumSpinorQ[SpAssoc[lVNoAssoc1]],
	TestID ->
		"massive + impl ref assoc not declared, massive + impl ref: assoc num"
]


TestCase[
	SpM[P, +1], SpM[lVNoAssoc2, -1, q1],
	TestID -> "massive + impl ref, massive - expl ref assoc not declared"
]
TestTrue[
	Spinors`Private`NumSpinorQ[SpAssoc[lVNoAssoc2, q1]],
	TestID ->
		"massless, massive + impl ref- expl ref assoc not declared: assoc num"
]


(* ::Subsection:: *)
(*Failed declarations*)


SetOptions[TestFailed,
	InputWrapper -> DeclareLVectorMomentum
]


TestFailed[
	LvBA[b, nonNumSp2],
	{
		HoldForm @ Message[
			DeclareLVectorMomentum::firstDeclare,
			LvBA[b, nonNumSp2],
			{nonNumSp2}
		]
	},
	TestID -> "massless, massless not declared"
];
TestFailed[
	LvBA[nonNumSp1, a],
	{
		HoldForm @ Message[
			DeclareLVectorMomentum::firstDeclare,
			LvBA[nonNumSp1, a],
			{nonNumSp1}
		]
	},
	TestID -> "massless not declared, massless"
];
TestFailed[
	LvBA[nonNumSp1, nonNumSp2],
	{
		HoldForm @ Message[
			DeclareLVectorMomentum::firstDeclare,
			LvBA[nonNumSp1, nonNumSp2],
			{nonNumSp1, nonNumSp2}
		]
	},
	TestID -> "massless not declared, massless not declared"
];
TestFailed[
	LvBA[SpM[P, -1], SpM[Q, +1, nonNumSp2]],
	{
		HoldForm @ Message[
			DeclareLVectorMomentum::firstDeclare,
			LvBA[SpM[P, -1], SpM[Q, 1, nonNumSp2]],
			{nonNumSp2}
		]
	},
	TestID -> "massive, massive not declared ref"
];
TestFailed[
	LvBA[SpM[nonNumLV1, +1, q1], SpM[Q, -1]],
	{
		HoldForm @ Message[
			DeclareLVectorMomentum::firstDeclare,
			LvBA[SpM[nonNumLV1, 1, q1], SpM[Q, -1]],
			{nonNumLV1}
		]
	},
	TestID -> "massive not declared LVec, massive"
];
TestFailed[
	LvBA[SpM[nonNumLV1, +1, nonNumSp1], SpM[nonNumLV2, -1, nonNumSp2]],
	{
		HoldForm @ Message[
			DeclareLVectorMomentum::firstDeclare,
			LvBA[SpM[nonNumLV1, 1, nonNumSp1], SpM[nonNumLV2, -1, nonNumSp2]],
			{nonNumLV1, nonNumSp1, nonNumLV2, nonNumSp2}
		]
	},
	TestID ->
		"massive not declared LVec and ref, massive not declared LVec and ref"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
