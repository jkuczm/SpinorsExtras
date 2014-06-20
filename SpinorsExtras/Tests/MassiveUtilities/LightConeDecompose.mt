(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`MassiveUtilities`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`",
	"SpinorsExtras`Utilities`",
	"SpinorsExtras`SatMmodifications`"(* simplifications for s *)
	,
	"MUnitExtras`Package`",
	"OptionsUtilities`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, ref, ref1, ref2, ref[P], ref[Q]},
	"LVectors" -> {P, Q, L, mRef},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All,
	"SpRefMomentaLVectors" -> {P, Q, L},
	"SpAssocMomentaLVectors" -> {P, Q, L},
	"SpAssocMomentaLVectorRefPairs" -> {
		{P, ref}, {P, ref[P]}, {Q, ref}, {Q, ref[Q]},
		{L, ref}, {P, ref2}, {Q, mRef}, {mRef, Q}
	},
	"PlusMinusOnes" -> {pm1, pm2}
];


SetOptions[
	{TestCasePatternsRules, Test, TestCaseSymbolicNumeric, TestSubexpression},
	ApplyToInput -> LightConeDecompose
];

SetOptions[TestCaseSymbolicNumeric,
	InputWrapperN ->
		(*
			If result is a list than elements of this list should be equal
			to each oher.
		*)
		(Module[
			{result = SetPrecision[N[#], 10]}
			,
			If[MatchQ[result, _List] && Equal @@ result,
				result = First[result]
			];
			
			result
		]&)
];

SetOptions[TestCasePatternsRules,
	Test -> TestCaseSymbolicNumeric,
	InputWrapper -> Expand,
	"Rule1" -> (P -> ref),
	"Rule2" -> (Q -> mRef)
];


(* ::Section:: *)
(*Tests*)


TestCasePatternsRules[
	s[P, Q, L],
	{
		{
			s[SpAssoc[P], MP2[P] / (2 MP[SpAssoc[P], SpRef[P]]) SpRef[P], Q, L]
			,
			TestID -> "s: P"
		}
		,
		{
			s[SpAssoc[P, ref], MP2[P] / (2 MP[SpAssoc[P, ref], ref]) ref, Q, L]
			,
			TestID -> "s: P -> ref"
		}
		,
		{
			s[P, SpAssoc[Q], MP2[Q] / (2 MP[SpAssoc[Q], SpRef[Q]]) SpRef[Q], L]
			,
			TestID -> "s: Q"
		}
		,
		{
			s[
				P,
				SpAssoc[Q, mRef],
				MP2[Q] / (2 MP[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]) *
					SpAssoc[mRef, Q],
				L
			]
			,
			TestID -> "s: Q -> mRef"
		}
		,
		{
			s[
				SpAssoc[P],
				MP2[P] / (2 MP[SpAssoc[P], SpRef[P]]) SpRef[P],
				SpAssoc[Q],
				MP2[Q] / (2 MP[SpAssoc[Q], SpRef[Q]]) SpRef[Q],
				L
			]
			,
			TestID -> "s: P|Q"
		}
		,
		{
			s[
				SpAssoc[P, ref],
				MP2[P] / (2 MP[SpAssoc[P, ref], ref]) ref,
				SpAssoc[Q, ref],
				MP2[Q] / (2 MP[SpAssoc[Q, ref], ref]) ref,
				L
			]
			,
			TestID -> "s: P|Q -> ref"
		}
		,
		{
			s[
				SpAssoc[P],
				MP2[P] / (2 MP[SpAssoc[P], SpRef[P]]) SpRef[P],
				SpAssoc[Q],
				MP2[Q] / (2 MP[SpAssoc[Q], SpRef[Q]]) SpRef[Q],
				SpAssoc[L],
				MP2[L] / (2 MP[SpAssoc[L], SpRef[L]]) SpRef[L]
			]
			,
			TestID -> "s: no args"
		}
		,
		{
			s[
				SpAssoc[P, ref],
				MP2[P] / (2 MP[SpAssoc[P, ref], ref]) ref,
				SpAssoc[Q, ref],
				MP2[Q] / (2 MP[SpAssoc[Q, ref], ref]) ref,
				SpAssoc[L, ref],
				MP2[L] / (2 MP[SpAssoc[L, ref], ref]) ref
			]
			,
			TestID -> "s: _ -> ref"
		}
		,
		{
			s[
				SpAssoc[P, ref[P]],
				MP2[P] / (2 MP[SpAssoc[P, ref[P]], ref[P]]) ref[P],
				SpAssoc[Q, ref[Q]],
				MP2[Q] / (2 MP[SpAssoc[Q, ref[Q]], ref[Q]]) ref[Q],
				L
			]
			,
			TestID -> "s: w:(P|Q) -> ref[w]"
		}
		,
		{
			s[
				SpAssoc[P],
				MP2[P] / (2 MP[SpAssoc[P], SpRef[P]]) SpRef[P],
				SpAssoc[Q, mRef],
				MP2[Q] / (2 MP[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]) *
					SpAssoc[mRef, Q],
				L
			]
			,
			TestID -> "s: {P, Q -> mRef}"
		}
		,
		{
			s[
				SpAssoc[P, ref],
				MP2[P] / (2 MP[SpAssoc[P, ref], ref]) ref,
				SpAssoc[Q, mRef],
				MP2[Q] / (2 MP[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]) *
					SpAssoc[mRef, Q],
				L
			]
			,
			TestID -> "s: {P -> ref, Q -> mRef}"
		}
	}
	,
	TestID -> "s"
];


SetOptions[TestCasePatternsRules, Test -> Test];


SetOptions[LightConeDecompose, "ForceRefChange" -> False];


TestCasePatternsRules[
	Spab[SpM[P, pm1], L, SpM[Q, pm2]],
	{
		{
			Spab[SpAssoc[P], L, SpM[Q, pm2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P], SpRef[P]] *
				Spbb[SpRef[P], L, SpM[Q, pm2]]
			,
			TestID -> "no forcing: Spab: no ref: P"
		}
		,
		{
			Spab[SpAssoc[P, ref], L, SpM[Q, pm2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P, ref], ref] *
				Spbb[ref, L, SpM[Q, pm2]]
			,
			TestID -> "no forcing: Spab: no ref: P -> ref"
		}
		,
		{
			Spab[SpM[P, pm1], L, SpAssoc[Q]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q], SpRef[Q]] *
				Spaa[SpM[P, pm1], L, SpRef[Q]]
			,
			TestID -> "no forcing: Spab: no ref: Q"
		}
		,
		{
			Spab[SpM[P, pm1], L, SpAssoc[Q, mRef]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]] *
				Spaa[SpM[P, pm1], L, SpAssoc[mRef, Q]]
			,
			TestID -> "no forcing: Spab: no ref: Q -> mRef"
		}
		,
		{
			Spab[SpAssoc[P], L,  SpAssoc[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P], SpRef[P]] Spaa[SpAssoc[Q], SpRef[Q]]) *
				Spba[SpRef[P], L, SpRef[Q]]
			,
			TestID -> "no forcing: Spab: no ref: P|Q"
		}
		,
		{
			Spab[SpAssoc[P, ref], L,  SpAssoc[Q, ref]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref], ref] Spaa[SpAssoc[Q, ref], ref]) *
					Spba[ref, L, ref]
			,
			TestID -> "no forcing: Spab: no ref: P|Q -> ref"
		}
		,
		{
			Spab[SpAssoc[P], SpAssoc[L],  SpAssoc[Q]]
			+ MP2[L] / (2 MP[SpAssoc[L], SpRef[L]]) *
				Spab[SpAssoc[P], SpRef[L], SpAssoc[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P], SpRef[P]] Spaa[SpAssoc[Q], SpRef[Q]]) *
					Spba[SpRef[P], SpAssoc[L], SpRef[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] MP2[L] /
				(
					2 Spbb[SpAssoc[P], SpRef[P]] *
					Spaa[SpAssoc[Q], SpRef[Q]] *
					MP[SpAssoc[L], SpRef[L]]
				) Spba[SpRef[P], SpRef[L], SpRef[Q]]
			,
			TestID -> "no forcing: Spab: no ref: no args"
		}
		,
		{
			Spab[SpAssoc[P, ref], SpAssoc[L, ref],  SpAssoc[Q, ref]]
			+ MP2[L] / (2 MP[SpAssoc[L, ref], ref]) *
				Spab[SpAssoc[P, ref], ref,  SpAssoc[Q, ref]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref], ref] Spaa[SpAssoc[Q, ref], ref]) *
					Spba[ref, SpAssoc[L, ref], ref]
			,
			TestID -> "no forcing: Spab: no ref: _ -> ref"
		}
		,
		{
			Spab[SpAssoc[P, ref[P]], L,  SpAssoc[Q, ref[Q]]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P, ref[P]], ref[P]] *
					Spaa[SpAssoc[Q, ref[Q]], ref[Q]]
				) Spba[ref[P], L, ref[Q]]
			,
			TestID -> "no forcing: Spab: no ref: w:(P|Q) -> ref[w]"
		}
		,
		{
			Spab[SpAssoc[P], L,  SpAssoc[Q, mRef]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P], SpRef[P]] *
					Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]
				) Spba[SpRef[P], L, SpAssoc[mRef, Q]]
			,
			TestID -> "no forcing: Spab: no ref: {P, Q -> mRef}"
		}
		,
		{
			Spab[SpAssoc[P, ref], L,  SpAssoc[Q, mRef]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P, ref], ref] *
					Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]
				) Spba[ref, L, SpAssoc[mRef, Q]]
			,
			TestID -> "no forcing: Spab: no ref: {P -> ref, Q -> mRef}"
		}
	}
	,
	TestID -> "no forcing: Spab: no ref"
];
TestCasePatternsRules[
	Spab[SpM[P, pm1, ref1], L, SpM[Q, pm2, ref2]],
	{
		{
			Spab[SpAssoc[P, ref1], L, SpM[Q, pm2, ref2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P, ref1], ref1] *
				Spbb[ref1, L, SpM[Q, pm2, ref2]]
			,
			TestID -> "no forcing: Spab: with ref: P"
		}
		,
		{
			Spab[SpAssoc[P, ref1], L, SpM[Q, pm2, ref2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P, ref1], ref1] *
				Spbb[ref1, L, SpM[Q, pm2, ref2]]
			,
			TestID -> "no forcing: Spab: with ref: P -> ref"
		}
		,
		{
			Spab[SpM[P, pm1, ref1], L, SpAssoc[Q, ref2]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q, ref2], ref2] *
				Spaa[SpM[P, pm1, ref1], L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: Q"
		}
		,
		{
			Spab[SpM[P, pm1, ref1], L, SpAssoc[Q, ref2]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q, ref2], ref2] *
				Spaa[SpM[P, pm1, ref1], L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: Q -> mRef"
		}
		,
		{
			Spab[SpAssoc[P, ref1], L,  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: P|Q"
		}
		,
		{
			Spab[SpAssoc[P, ref1], L,  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: P|Q -> ref"
		}
		,
		{
			Spab[SpAssoc[P, ref1], SpAssoc[L],  SpAssoc[Q, ref2]]
			+ MP2[L] / (2 MP[SpAssoc[L], SpRef[L]]) *
				Spab[SpAssoc[P, ref1], SpRef[L],  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, SpAssoc[L], ref2]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] MP2[L] /
				(
					2 Spbb[SpAssoc[P, ref1], ref1] *
					Spaa[SpAssoc[Q, ref2], ref2] *
					MP[SpAssoc[L], SpRef[L]]
				) Spba[ref1, SpRef[L], ref2]
			,
			TestID -> "no forcing: Spab: with ref: no args"
		}
		,
		{
			Spab[SpAssoc[P, ref1], SpAssoc[L, ref],  SpAssoc[Q, ref2]]
			+ MP2[L] / (2 MP[SpAssoc[L, ref], ref]) *
				Spab[SpAssoc[P, ref1], ref,  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, SpAssoc[L, ref], ref2]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] MP2[L] /
				(
					2 Spbb[SpAssoc[P, ref1], ref1] *
					Spaa[SpAssoc[Q, ref2], ref2] *
					MP[SpAssoc[L, ref], ref]
				) Spba[ref1, ref, ref2]
			,
			TestID -> "no forcing: Spab: with ref: _ -> ref"
		}
		,
		{
			Spab[SpAssoc[P, ref1], L,  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: w:(P|Q) -> ref[w]"
		}
		,
		{
			Spab[SpAssoc[P, ref1], L,  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: {P, Q -> mRef}"
		}
		,
		{
			Spab[SpAssoc[P, ref1], L,  SpAssoc[Q, ref2]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref1], ref1] Spaa[SpAssoc[Q, ref2], ref2]) *
					Spba[ref1, L, ref2]
			,
			TestID -> "no forcing: Spab: with ref: {P -> ref, Q -> mRef}"
		}
	}
	,
	TestID -> "no forcing: Spab: with ref"
];


SetOptions[LightConeDecompose, "ForceRefChange" -> True];


TestCasePatternsRules[
	Spab[SpM[P, pm1], L, SpM[Q, pm2]],
	{
		{
			Spab[SpAssoc[P], L, SpM[Q, pm2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P], SpRef[P]] *
				Spbb[SpRef[P], L, SpM[Q, pm2]]
			,
			TestID -> "force ref change: Spab: no ref: P"
		}
		,
		{
			Spab[SpAssoc[P, ref], L, SpM[Q, pm2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P, ref], ref] *
				Spbb[ref, L, SpM[Q, pm2]]
			,
			TestID -> "force ref change: Spab: no ref: P -> ref"
		}
		,
		{
			Spab[SpM[P, pm1], L, SpAssoc[Q]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q], SpRef[Q]] *
				Spaa[SpM[P, pm1], L, SpRef[Q]]
			,
			TestID -> "force ref change: Spab: no ref: Q"
		}
		,
		{
			Spab[SpM[P, pm1], L, SpAssoc[Q, mRef]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]] *
				Spaa[SpM[P, pm1], L, SpAssoc[mRef, Q]]
			,
			TestID -> "force ref change: Spab: no ref: Q -> mRef"
		}
		,
		{
			Spab[SpAssoc[P], L,  SpAssoc[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P], SpRef[P]] Spaa[SpAssoc[Q], SpRef[Q]]) *
					Spba[SpRef[P], L, SpRef[Q]]
			,
			TestID -> "force ref change: Spab: no ref: P|Q"
		}
		,
		{
			Spab[SpAssoc[P, ref], L,  SpAssoc[Q, ref]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref], ref] Spaa[SpAssoc[Q, ref], ref]) *
					Spba[ref, L, ref]
			,
			TestID -> "force ref change: Spab: no ref: P|Q -> ref"
		}
		,
		{
			Spab[SpAssoc[P], SpAssoc[L],  SpAssoc[Q]]
			+ MP2[L] / (2 MP[SpAssoc[L], SpRef[L]]) *
				Spab[SpAssoc[P], SpRef[L],  SpAssoc[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P], SpRef[P]] Spaa[SpAssoc[Q], SpRef[Q]]) *
					Spba[SpRef[P], SpAssoc[L], SpRef[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] MP2[L] /
				(
					2 Spbb[SpAssoc[P], SpRef[P]] *
					Spaa[SpAssoc[Q], SpRef[Q]] *
					MP[SpAssoc[L], SpRef[L]]
				) Spba[SpRef[P], SpRef[L], SpRef[Q]]
			,
			TestID -> "force ref change: Spab: no ref: no args"
		}
		,
		{
			Spab[SpAssoc[P, ref], SpAssoc[L, ref],  SpAssoc[Q, ref]]
			+ MP2[L] / (2 MP[SpAssoc[L, ref], ref]) *
				Spab[SpAssoc[P, ref], ref,  SpAssoc[Q, ref]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref], ref] Spaa[SpAssoc[Q, ref], ref]) *
					Spba[ref, SpAssoc[L, ref], ref]
			,
			TestID -> "force ref change: Spab: no ref: _ -> ref"
		}
		,
		{
			Spab[SpAssoc[P, ref[P]], L,  SpAssoc[Q, ref[Q]]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P, ref[P]], ref[P]] *
					Spaa[SpAssoc[Q, ref[Q]], ref[Q]]
				) Spba[ref[P], L, ref[Q]]
			,
			TestID -> "force ref change: Spab: no ref: w:(P|Q) -> ref[w]"
		}
		,
		{
			Spab[SpAssoc[P], L,  SpAssoc[Q, mRef]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P], SpRef[P]] *
					Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]
				) Spba[SpRef[P], L, SpAssoc[mRef, Q]]
			,
			TestID -> "force ref change: Spab: no ref: {P, Q -> mRef}"
		}
		,
		{
			Spab[SpAssoc[P, ref], L,  SpAssoc[Q, mRef]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P, ref], ref] *
					Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]
				) Spba[ref, L, SpAssoc[mRef, Q]]
			,
			TestID -> "force ref change: Spab: no ref: {P -> ref, Q -> mRef}"
		}
	}
	,
	TestID -> "force ref change: Spab: no ref"
];
TestCasePatternsRules[
	Spab[SpM[P, pm1, ref1], L, SpM[Q, pm2, ref2]],
	{
		{
			Spab[SpAssoc[P], L, SpM[Q, pm2, ref2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P], SpRef[P]] *
				Spbb[SpRef[P], L, SpM[Q, pm2, ref2]]
			,
			TestID -> "force ref change: Spab: with ref: P"
		}
		,
		{
			Spab[SpAssoc[P, ref], L, SpM[Q, pm2, ref2]]
			- pm1 Sqrt[MP2[P]] / Spbb[SpAssoc[P, ref], ref] *
				Spbb[ref, L, SpM[Q, pm2, ref2]]
			,
			TestID -> "force ref change: Spab: with ref: P -> ref"
		}
		,
		{
			Spab[SpM[P, pm1, ref1], L, SpAssoc[Q]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q], SpRef[Q]] *
				Spaa[SpM[P, pm1, ref1], L, SpRef[Q]]
			,
			TestID -> "force ref change: Spab: with ref: Q"
		}
		,
		{
			Spab[SpM[P, pm1, ref1], L, SpAssoc[Q, mRef]]
			+ pm2 Sqrt[MP2[Q]] / Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]] *
				Spaa[SpM[P, pm1, ref1], L, SpAssoc[mRef, Q]]
			,
			TestID -> "force ref change: Spab: with ref: Q -> mRef"
		}
		,
		{
			Spab[SpAssoc[P], L,  SpAssoc[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P], SpRef[P]] Spaa[SpAssoc[Q], SpRef[Q]]) *
					Spba[SpRef[P], L, SpRef[Q]]
			,
			TestID -> "force ref change: Spab: with ref: P|Q"
		}
		,
		{
			Spab[SpAssoc[P, ref], L,  SpAssoc[Q, ref]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref], ref] Spaa[SpAssoc[Q, ref], ref]) *
					Spba[ref, L, ref]
			,
			TestID -> "force ref change: Spab: with ref: P|Q -> ref"
		}
		,
		{
			Spab[SpAssoc[P], SpAssoc[L],  SpAssoc[Q]]
			+ MP2[L] / (2 MP[SpAssoc[L], SpRef[L]]) *
				Spab[SpAssoc[P], SpRef[L],  SpAssoc[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P], SpRef[P]] Spaa[SpAssoc[Q], SpRef[Q]]) *
					Spba[SpRef[P], SpAssoc[L], SpRef[Q]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] MP2[L] /
				(
					2 Spbb[SpAssoc[P], SpRef[P]] *
					Spaa[SpAssoc[Q], SpRef[Q]] *
					MP[SpAssoc[L], SpRef[L]]
				) Spba[SpRef[P], SpRef[L], SpRef[Q]]
			,
			TestID -> "force ref change: Spab: with ref: no args"
		}
		,
		{
			Spab[SpAssoc[P, ref], SpAssoc[L, ref],  SpAssoc[Q, ref]]
			+ MP2[L] / (2 MP[SpAssoc[L, ref], ref]) *
				Spab[SpAssoc[P, ref], ref,  SpAssoc[Q, ref]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(Spbb[SpAssoc[P, ref], ref] Spaa[SpAssoc[Q, ref], ref]) *
					Spba[ref, SpAssoc[L, ref], ref]
			,
			TestID -> "force ref change: Spab: with ref: _ -> ref"
		}
		,
		{
			Spab[SpAssoc[P, ref[P]], L,  SpAssoc[Q, ref[Q]]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P, ref[P]], ref[P]] *
					Spaa[SpAssoc[Q, ref[Q]], ref[Q]]
				) Spba[ref[P], L, ref[Q]]
			,
			TestID -> "force ref change: Spab: with ref: w:(P|Q) -> ref[w]"
		}
		,
		{
			Spab[SpAssoc[P], L,  SpAssoc[Q, mRef]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P], SpRef[P]] *
					Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]
				) Spba[SpRef[P], L, SpAssoc[mRef, Q]]
			,
			TestID -> "force ref change: Spab: with ref: {P, Q -> mRef}"
		}
		,
		{
			Spab[SpAssoc[P, ref], L,  SpAssoc[Q, mRef]]
			- pm1 pm2 Sqrt[MP2[Q]] Sqrt[MP2[P]] /
				(
					Spbb[SpAssoc[P, ref], ref] *
					Spaa[SpAssoc[Q, mRef], SpAssoc[mRef, Q]]
				) Spba[ref, L, SpAssoc[mRef, Q]]
			,
			TestID -> "force ref change: Spab: with ref: {P -> ref, Q -> mRef}"
		}
	}
	,
	TestID -> "force ref change: Spab: with ref"
];


AssignTestFeatures[SpxyTestCase];
PrependToOptions[SpxyTestCase, Test -> Test];

SpxyTestCase[
	toDecompose_,
	refVec_,
	assocVec_,
	mass_,
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[SpxyTestCase]}
		,
		
		OptionValue[Test][
			{Spaa[toDecompose, toDecompose]},
			0,
			TestFailureMessage -> "A Spinor orthogonality"
		];	
		OptionValue[Test][
			{Spbb[toDecompose, toDecompose]},
			0,
			TestFailureMessage -> "B Spinor orthogonality"
		];
		OptionValue[Test][
			{Spab[toDecompose, toDecompose]},
			2 mass,
			TestFailureMessage -> "AB Spinor normalization"
		];
		OptionValue[Test][
			{Spba[toDecompose, toDecompose]},
			2 mass,
			TestFailureMessage -> "BA Spinor normalization"
		];
		
		OptionValue[Test][
			{Spaa[toDecompose, a]},
			Spaa[assocVec, a],
			TestFailureMessage -> "Spaa massless left"
		];
		OptionValue[Test][
			{Spab[toDecompose, a]},
			-Spbb[refVec, a] mass / Spbb[assocVec, refVec],
			TestFailureMessage -> "Spab massless left"
		];
		OptionValue[Test][
			{Spbb[toDecompose, a]},
			Spbb[assocVec, a],
			TestFailureMessage -> "Spbb massless left"
		];
		OptionValue[Test][
			{Spba[toDecompose, a]},
			-Spaa[refVec, a] mass / Spaa[assocVec, refVec],
			TestFailureMessage -> "Spba massless left"
		];
		
		OptionValue[Test][
			{Spaa[a, toDecompose]},
			Spaa[a, assocVec],
			TestFailureMessage -> "Spaa massless right"
		];
		OptionValue[Test][
			{Spba[a, toDecompose]},
			Spbb[a, refVec] mass / Spbb[assocVec, refVec],
			TestFailureMessage -> "Spba massless right"
		];
		OptionValue[Test][
			{Spbb[a, toDecompose]},
			Spbb[a, assocVec],
			TestFailureMessage -> "Spbb massless right"
		];
		OptionValue[Test][
			{Spab[a, toDecompose]},
			Spaa[a, refVec] mass / Spaa[assocVec, refVec],
			TestFailureMessage -> "Spab massless right"
		];
		
		OptionValue[Test][
			{Spaa[toDecompose, Q]}
			,
			Spaa[assocVec, Q]
			- Spba[refVec, Q] mass / Spbb[assocVec, refVec]
			,
			TestFailureMessage -> "Spaa massive left"
		];
		OptionValue[Test][
			{Spbb[toDecompose, Q]}
			,
			Spbb[assocVec, Q]
			- Spab[refVec, Q] mass / Spaa[assocVec, refVec]
			,
			TestFailureMessage -> "Spbb massive left"
		];
		OptionValue[Test][
			{Spab[toDecompose, Q]}
			,
			Spab[assocVec, Q]
			- Spbb[refVec, Q] mass / Spbb[assocVec, refVec]
			,
			TestFailureMessage -> "Spab massive left"
		];
		OptionValue[Test][
			{Spba[toDecompose, Q]}
			,
			Spba[assocVec, Q]
			-Spaa[refVec, Q] mass / Spaa[assocVec, refVec]
			,
			TestFailureMessage -> "Spba massive left"
		];
		
		OptionValue[Test][
			{Spaa[Q, toDecompose]}
			,
			Spaa[Q, assocVec]
			+ Spab[Q, refVec] mass / Spbb[assocVec, refVec]
			,
			TestFailureMessage -> "Spaa massive right"
		];
		OptionValue[Test][
			{Spba[Q, toDecompose]}
			,
			Spba[Q, assocVec]
			+ Spbb[Q, refVec] mass / Spbb[assocVec, refVec]
			,
			TestFailureMessage -> "Spba massive right"
		];
		OptionValue[Test][
			{Spbb[Q, toDecompose]}
			,
			Spbb[Q, assocVec]
			+ Spba[Q, refVec] mass / Spaa[assocVec, refVec]
			,
			TestFailureMessage -> "Spbb massive right"
		];
		OptionValue[Test][
			{Spab[Q, toDecompose]}
			,
			Spab[Q, assocVec]
			+ Spaa[Q, refVec] mass / Spaa[assocVec, refVec]
			,
			TestFailureMessage -> "Spab massive right"
		];
		,
		"CommonOptionsFor" -> {OptionValue[Test]}
	];


SetOptions[LightConeDecompose, "ForceRefChange" -> True];

SpxyTestCase[
	SpM[P, pm1],
	SpRef[P],
	SpAssoc[P],
	pm1 Sqrt[MP2[P]],
	TestID -> "implict ref",
	ApplyToInput -> (LightConeDecompose[#, P]&)
];
SpxyTestCase[
	SpM[P, pm1],
	ref,
	SpAssoc[P, ref],
	pm1 Sqrt[MP2[P]],
	TestID -> "explicit massless ref",
	ApplyToInput -> (LightConeDecompose[#, P -> ref]&)
];
SpxyTestCase[
	SpM[P, pm1],
	SpAssoc[mRef, P],
	SpAssoc[P, mRef],
	pm1 Sqrt[MP2[P]],
	TestID -> "explicit massive ref",
	ApplyToInput -> (LightConeDecompose[#, P -> mRef]&)
];


TestSubexpression[
	{Sm[SpAssoc[P]], P},
	TestID -> "P inside SpAssoc: expresion not changed"
];
TestSubexpression[
	{Sm[SpRef[P]], P},
	TestID -> "P inside SpRef: expresion not changed"
];


Test[
	{Sm[x], x}
	,
	Sm[SpAssoc[x]] +
	(MP[x, x] Sm[SpRef[x]])/(2 MP[SpAssoc[x], SpRef[x]])
	,
	TestID -> "arbitrary symbol: given as arg"
];
TestSubexpression[
	{Sm[x]},
	TestID -> "arbitrary symbol: no args"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
