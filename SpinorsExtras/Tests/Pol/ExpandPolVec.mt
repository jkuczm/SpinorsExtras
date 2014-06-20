(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Pol`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`",
	"SpinorsExtras`MassiveUtilities`", (* LightConeDecompose *)
	"SpinorsExtras`SatMmodifications`" (* simplifications for s *)
	,
	"MUnitExtras`Package`",
	"OptionsUtilities`"
}];
SetUpSpinorsTestEnvironment["Spinors" -> {q}, "LVectors" -> {P, Q, R}];


explicitRefRules = {
	PolVec[mom_, pol_] :> PolVec[mom, pol, q],
	SpAssoc[mom_] -> SpAssoc[mom, q],
	SpRef[_] -> q
};

AssignTestFeatures[TestCaseExplImplRef];
PrependToOptions[TestCaseExplImplRef,
	Test -> Test,
	"LightConeDecompose" -> False
];

TestCaseExplImplRef[input_, expected_, opts:OptionsPattern[]] :=
	Module[
		{
			inputWrapperImpl,
			inputWrapperExpl,
			test = OptionValue[Test]
		}
		,
		If[OptionValue["LightConeDecompose"],
			inputWrapperImpl = LightConeDecompose[#, P]&;
			inputWrapperExpl =
				LightConeDecompose[# /. explicitRefRules, P -> q]&;
		(* else *),
			inputWrapperImpl = None;
			inputWrapperExpl = # /. explicitRefRules &;
		];
		TestCaseEnvironment[
			{opts, Options[TestCaseExplImplRef]}
			,
			
			test[
				input,
				expected,
				InputWrapper -> inputWrapperImpl,
				TestFailureMessage -> "implicit ref"
			];
			
			With[
				{
					inputExpl = input /. explicitRefRules,
					expectedExpl = expected /. explicitRefRules
				}
				,
				test[
					inputExpl,
					expectedExpl,
					InputWrapper -> inputWrapperExpl,
					TestFailureMessage -> ": explicit ref"
				];
			];
			,
			"CommonOptionsFor" -> {test}
		];
	];


AddTestDefaultFunction[TestCaseExplImplRefZero, 0, TestCaseExplImplRef];


(* ::Section:: *)
(*Tests*)


SetOptions[{TestCasePatterns, TestCaseExplImplRef},
	ApplyToInput -> ExpandPolVec
];


(* ::Subsection:: *)
(*Pattern interface*)

SetOptions[TestCasePatterns,
	InputWrapper -> Expand,
	"Var1" -> PolVec[P, +1],
	"Var2" -> PolVec[Q, __]
];


TestCasePatterns[
	s[PolVec[P, +1], PolVec[Q, -1, q], PolVec[R, 0]],
	{
		{
			s[PolVec[Q, -1, q], PolVec[R, 0]]
			+ Sqrt[2] / Spbb[SpRef[P], SpAssoc[P]] *
				Spab[SpAssoc[P], PolVec[Q, -1, q], SpRef[P]]
			+ Sqrt[2] / Spbb[SpRef[P], SpAssoc[P]] *
				Spab[SpAssoc[P], PolVec[R, 0], SpRef[P]]
			,
			TestID -> "s: PolVec[P, +1]"
		}
		,
		{
			s[PolVec[P, +1], PolVec[R, 0]]
			+ Sqrt[2] / Spaa[q, SpAssoc[Q, q]] *
				Spab[q, PolVec[P, +1], SpAssoc[Q, q]]
			+ Sqrt[2] / Spaa[q, SpAssoc[Q, q]] *
				Spab[q, PolVec[R, 0], SpAssoc[Q, q]]
			,
			TestID -> "s: PolVec[Q, __]"
		}
		,
		{
			MP2[PolVec[R, 0]]
			+ Sqrt[2] / Spbb[SpRef[P], SpAssoc[P]] *
				Spab[SpAssoc[P], PolVec[R, 0], SpRef[P]]
			+ Sqrt[2] / Spaa[q, SpAssoc[Q, q]] *
				Spab[q, PolVec[R, 0], SpAssoc[Q, q]]
			+ 2 /(Spbb[SpRef[P], SpAssoc[P]] Spaa[q, SpAssoc[Q, q]]) *
				Spaa[SpAssoc[P], q] Spbb[SpAssoc[Q, q], SpRef[P]]
			,
			TestID -> "s: PolVec[P, +1] | PolVec[Q, __]"
		}
		,
		{
			-MP2[R]/Abs[MP2[R]]
			+ Sqrt[2] / (Spbb[SpRef[P], SpAssoc[P]] Sqrt[Abs[MP2[R]]]) *
				Spab[SpAssoc[P], R, SpRef[P]]
			- Sqrt[2] MP2[R] /
				(
					Spbb[SpRef[P], SpAssoc[P]] *
					Sqrt[Abs[MP2[R]]] MP[R, SpRef[R]]
				) *
				Spab[SpAssoc[P], SpRef[R], SpRef[P]]
			+ Sqrt[2] / (Spaa[q, SpAssoc[Q, q]] Sqrt[Abs[MP2[R]]]) *
				Spab[q, R, SpAssoc[Q, q]]
			- Sqrt[2] MP2[R] /
				(Spaa[q, SpAssoc[Q, q]] Sqrt[Abs[MP2[R]]] MP[R, SpRef[R]]) *
				Spab[q, SpRef[R], SpAssoc[Q, q]]
			+ 2 /(Spbb[SpRef[P], SpAssoc[P]] Spaa[q, SpAssoc[Q, q]]) *
				Spaa[SpAssoc[P], q] Spbb[SpAssoc[Q, q], SpRef[P]]
			,
			TestID -> "s: no args"
		}
	}
	,
	TestID -> "s"
];

TestCasePatterns[
	Spab[PolVec[P, +1], PolVec[R, 0], PolVec[Q, -1, q]],
	{
		{
			Sqrt[2] Spab[SpAssoc[P], PolVec[R, 0], PolVec[Q, -1, q]]
				/ Spbb[SpRef[P], SpAssoc[P]]
			,
			TestID -> "Spab: PolVec[P, +1]"
		}
		,
		{
			Sqrt[2] Spab[PolVec[P, +1], PolVec[R, 0], SpAssoc[Q, q]]
				/ Spaa[q, SpAssoc[Q, q]]
			,
			TestID -> "Spab: PolVec[Q, __]"
		}
		,
		{
			2 Spab[SpAssoc[P], PolVec[R, 0], SpAssoc[Q, q]]
				/ (Spbb[SpRef[P], SpAssoc[P]] Spaa[q, SpAssoc[Q, q]])
			,
			TestID -> "Spab: PolVec[P, +1] | PolVec[Q, __]"
		}
		,
		{
			2 Spab[SpAssoc[P], R, SpAssoc[Q, q]]
				/ (Spbb[SpRef[P], SpAssoc[P]] Spaa[q, SpAssoc[Q, q]] *
					Sqrt[Abs[MP2[R]]])
			- 2 MP2[R] Spab[SpAssoc[P], SpRef[R], SpAssoc[Q, q]]
				/ (Spbb[SpRef[P], SpAssoc[P]] Spaa[q, SpAssoc[Q, q]] *
					Sqrt[Abs[MP2[R]]] MP[R, SpRef[R]])
			,
			TestID -> "Spab: no args"
		}
	}
	,
	TestID -> "Spab"
];


(* ::Subsection:: *)
(*Basic LVector decompositions*)


TestCaseExplImplRef[
	{Sm[PolVec[P, -1]]},
	Sqrt[2] SmBA[SpAssoc[P], SpRef[P]] / Spaa[SpRef[P], SpAssoc[P]],
	TestID -> "E_-"
];
TestCaseExplImplRef[
	{Sm[PolVec[P, +1]]},
	Sqrt[2] SmBA[SpRef[P], SpAssoc[P]] / Spbb[SpRef[P], SpAssoc[P]],
	TestID -> "E_+"
];
TestCaseExplImplRef[
	{Sm[PolVec[P, 0]]}
	,
	Sm[P / Sqrt[Abs[MP2[P]]]]
		- MP2[P] Sm[SpRef[P]] / (Sqrt[Abs[MP2[P]]] MP[P, SpRef[P]])
	,
	TestID -> "E_0"
];
TestCaseExplImplRef[
	{Sm[PolVec[P, "S"]]},
	Sm[P / Sqrt[Abs[MP2[P]]]],
	TestID -> "E_S"
];


(* ::Subsection:: *)
(*Identities*)


TestCaseExplImplRef[
	{MP[PolVec[P, -1], SpRef[P]]},
	0,
	TestID -> "E_-.ref = 0"
];
TestCaseExplImplRef[
	{MP[PolVec[P, +1], SpRef[P]]},
	0,
	TestID -> "E_+.ref = 0"
];


SetOptions[{TestCaseExplImplRef, TestCaseExplImplRefZero},
	"LightConeDecompose" -> True
];

TestCaseSparse[
	{
		{
			{-1, +1},
			1,
			TestID -> "E_-1.E_+1 = 1"
		}
		,
		{
			{0, 0},
			-MP2[P]/Abs[MP2[P]],
			TestID -> "E_0.E_0 = -1"
		}
		,
		{
			{"S", "S"},
			MP2[P]/Abs[MP2[P]],
			TestID -> "E_S.E_S = 1"
		}
	},
	TestID -> "E_x.E_y identities",
	Test -> TestCaseExplImplRef,
	TestDefault -> TestCaseExplImplRefZero,
	(* All possible pairs of polarization vectors *)
	AllTestsArgs -> List /@ Union[Sort /@ Tuples[{-1, +1, 0, "S"}, 2]],
	ApplyToInput -> (ExpandPolVec[MP[PolVec[P, #1], PolVec[P, #2]]]&)
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
