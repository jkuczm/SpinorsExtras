(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Massive`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Composite`", (* LvBA *)
	"SpinorsExtras`Package`", (* QuietSpinorPrint *)
	"SpinorsExtras`MassiveUtilities`", (* LightConeDecompose *)
	"MUnitExtras`Package`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, eta, nonNumSp1, nonNumSp2},
	"LVectors" -> {P, Q, R, etaMassive, nonNumLV1, nonNumLV2},
	"RandomMomentaSpinors" -> {a, b},
	"RandomMomentaLVectors" -> {P, Q, R},
	"SpRefMomentaLVectors" -> {P, Q, R}
	(*
		Don't automatically declare momenta for SpAssoc since that's exactly
		what we're testing here.
	*)
];


compMassiveLv = LvBA[SpM[Q, 1], SpM[R, 1]];


QuietSpinorPrint[
	DeclareSpinorMomentum[eta, La[a] // N, Lat[b] // N];
	DeclareSpinorMomentum[LvBA[b, a]];
	
	DeclareSpinorMomentum /@ SpAssoc /@ {Q, R};
	DeclareLVectorMomentum[
		etaMassive
		,
		Spba[SpM[Q, 1], #, SpM[R, 1]]/2 & /@ {Gamma0, Gamma1, Gamma2, Gamma3}
			// LightConeDecompose // N
	];
	DeclareLVectorMomentum[compMassiveLv];
];


AssignTestFeatures[TestCase]

TestCase[massive_, associated_, masslessRef_, opts:OptionsPattern[]] :=
	TestCaseEnvironment[
		{opts, Options[TestCase]}
		,
		TestZero[
			MP2[Num4V[associated]],
			TestFailureMessage -> "maslessnes"
		];
		Test[
			Num4V[associated] + MP2[Num4V[massive]] /
				(2 MP[Num4V[massive], Num4V[masslessRef]]) *
				Num4V[masslessRef]
			,
			Num4V[massive],
			TestFailureMessage -> "reproduce massive FV"
		];
		,
		"CommonOptionsFor" -> {TestZero, Test}
	];


SetOptions[TestCase,
	EquivalenceFunction -> Equal,
	InputWrapper-> (SetAccuracy[#, 8]&)
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Succesful declarations*)


(* ::Subsubsection:: *)
(*Implicit reference vector*)


TestNull[
	DeclareSpinorMomentum[SpAssoc[P]],
	TestID -> "implicit ref: DeclareSpinorMomentum evaluation"
];
TestCase[
	P, SpAssoc[P], SpRef[P],
	TestID -> "implicit ref"
];


(* ::Subsubsection:: *)
(*Massless real reference vector*)


TestNull[
	DeclareSpinorMomentum[SpAssoc[P, a]],
	TestID -> "massless real ref: DeclareSpinorMomentum evaluation"
];
TestCase[
	P, SpAssoc[P, a], a,
	TestID -> "massless real ref"
];


(* ::Subsubsection:: *)
(*Massless complex reference vector*)


TestNull[
	DeclareSpinorMomentum[SpAssoc[P, eta]],
	TestID -> "massless complex ref: DeclareSpinorMomentum evaluation"
];
TestCase[
	P, SpAssoc[P, eta], eta,
	TestID -> "massless complex ref"
];


(* ::Subsubsection:: *)
(*Massless complex reference vector given as composite vector*)
TestNull[
	DeclareSpinorMomentum[SpAssoc[P, LvBA[b, a]]],
	TestID -> "massless complex ref as composite: DeclareSpinorMomentum evaluation"
];
TestCase[
	P, SpAssoc[P, LvBA[b, a]], eta,
	TestID -> "massless complex ref as composite"
];


(* ::Subsubsection:: *)
(*Massive real reference vector*)


TestNull[
	DeclareSpinorMomentum[SpAssoc[P, Q]],
	TestID -> "massive real ref P: DeclareSpinorMomentum evaluation"
];
TestNull[
	DeclareSpinorMomentum[SpAssoc[Q, P]],
	TestID -> "massive real ref Q: DeclareSpinorMomentum evaluation"
];
TestCase[
	P, SpAssoc[P, Q], SpAssoc[Q, P],
	TestID -> "massive real ref P"
];
TestCase[
	Q, SpAssoc[Q, P], SpAssoc[P, Q],
	TestID -> "massive real ref Q"
];


(* ::Subsubsection:: *)
(*Massive complex reference vector given as composite vector*)


TestNull[
	DeclareSpinorMomentum[SpAssoc[P, compMassiveLv]],
	TestID ->
		"massive complex ref as composite: DeclareSpinorMomentum evaluation"
];
TestNull[
	DeclareSpinorMomentum[SpAssoc[compMassiveLv, P]],
	TestID -> "massive complex base LVector: DeclareSpinorMomentum evaluation"
];
TestCase[
	P, SpAssoc[P, compMassiveLv], SpAssoc[compMassiveLv, P],
	TestID -> "massive complex complex base LVec"
];
TestCase[
	compMassiveLv, SpAssoc[compMassiveLv, P], SpAssoc[P, compMassiveLv],
	TestID -> "massive complex ref as composite"
];


(* ::Subsection:: *)
(*Failed declarations*)


SetOptions[TestFailed,
	InputWrapper -> DeclareSpinorMomentum
]


TestFailed[
	SpAssoc[nonNumLV1],
	{
		HoldForm @ Message[
			DeclareSpinorMomentum::firstDeclare,
			SpAssoc[nonNumLV1],
			{nonNumLV1}
		]
	},
	TestID -> "Momentum not declared: default ref: base LVector"
];

TestFailed[
	SpAssoc[nonNumLV1, Q],
	{
		HoldForm @ Message[
			DeclareSpinorMomentum::firstDeclare,
			SpAssoc[nonNumLV1, Q],
			{nonNumLV1}
		]
	},
	TestID -> "Momentum not declared: explicit ref: base LVector"
];
TestFailed[
	SpAssoc[P, nonNumLV2],
	{
		HoldForm @ Message[
			DeclareSpinorMomentum::firstDeclare,
			SpAssoc[P, nonNumLV2],
			{nonNumLV2}
		]
	},
	TestID -> "Momentum not declared: explicit ref: ref LVector"
];
TestFailed[
	SpAssoc[nonNumLV1, nonNumLV2],
	{
		HoldForm @ Message[
			DeclareSpinorMomentum::firstDeclare,
			SpAssoc[nonNumLV1, nonNumLV2],
			{nonNumLV1, nonNumLV2}
		]
	},
	TestID -> "Momentum not declared: explicit ref: base LVector, ref LVector"
];

TestUnchanged[
	DeclareSpinorMomentum[SpAssoc[P, {b, a}]],
	TestID -> "pair of spinors as reference vector"
]


EndSpinorsTestEnvironment[];
