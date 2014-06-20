(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Composite`",
	"SpinorsExtras`Massive`" (* SpM *)
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, q1, q2, nonNumSp1, nonNumSp2},
	"LVectors" -> {P, Q, nonNumLV1, nonNumLV2},
	"RandomMomentaSpinors" -> {a, b, q1, q2},
	"RandomMomentaLVectors" -> {P, Q},
	"SpRefMomentaLVectors" -> {P, Q},
	"SpAssocMomentaLVectors" -> {P, Q},
	"SpAssocMomentaLVectorRefPairs" -> {{P, q1}, {Q, q2}}
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Succesful declarations*)


SetOptions[{TestZero, Test},
	InputWrapper -> (SetAccuracy[#, 9]&),
	EquivalenceFunction -> (TrueQ[Equal[##]]&)
];


TestNull[
	DeclareSpinorMomentum[LvBA[b, a]],
	TestID -> "massless, massless: DeclareSpinorMomentum evaluation"
];
TestZero[
	MP2[Num4V[LvBA[b, a]]],
	TestID -> "massless, massless: maslessnes"
];
Test[
	Num4V[LvBA[b, a]],
	(Spba[b, #, a] / 2)& /@ {Gamma0, Gamma1, Gamma2, Gamma3} // N,
	TestID -> "massless, massless: Num4V"
];
Test[
	Spinors`Private`NumLa[LvBA[b, a]],
	Spinors`Private`NumLa[a],
	TestID -> "massless, massless: NumLa"
];
Test[
	Spinors`Private`NumCLa[LvBA[b, a]],
	Spinors`Private`NumCLa[a],
	TestID -> "massless, massless: NumCLa"
];
Test[
	Spinors`Private`NumLat[LvBA[b, a]],
	Spinors`Private`NumLat[b],
	TestID -> "massless, massless: NumLat"
];
Test[
	Spinors`Private`NumCLat[LvBA[b, a]],
	Spinors`Private`NumCLat[b],
	TestID -> "massless, massless: NumCLat"
];


(* ::Subsection:: *)
(*Failed declarations*)


SetOptions[TestFailed,
	InputWrapper -> DeclareSpinorMomentum
]


TestFailed[
	LvBA[b, nonNumSp2],
	{
		HoldForm @ Message[
			DeclareSpinorMomentum::firstDeclare,
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
			DeclareSpinorMomentum::firstDeclare,
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
			DeclareSpinorMomentum::firstDeclare,
			LvBA[nonNumSp1, nonNumSp2],
			{nonNumSp1, nonNumSp2}
		]
	},
	TestID -> "massless not declared, massless not declared"
];


TestUnchanged[
	DeclareSpinorMomentum[LvBA[SpM[P, 1], SpM[Q, -1]]],
	TestID -> "massive, massive"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
