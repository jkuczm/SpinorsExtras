(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Utilities`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`"
}];
SetUpSpinorsTestEnvironment["Spinors" -> {a, b, c}, "LVectors" -> {P, L}];


SetOptions[{Test, TestSubexpression, TestCaseRules},
	ApplyToInput -> ReplaceLVector
];


(* ::Section:: *)
(*Tests*)


Test[
	{Spaa[a, P, b, c], b -> Q},
	Spaa[a, P, Q, c],
	TestID -> "Spaa spinor"
];
Test[
	{Spaa[a, P, 1, c], 1 -> Q},
	Spaa[a, P, Q, c],
	TestID -> "Spaa from integer"
];
Test[
	{Spaa[a, P, 1, c], {{1 -> Q}}},
	{Spaa[a, P, Q, c]},
	TestID -> "Spaa from integer deep"
];
Test[
	{Spaa[a, P, b, c], P -> 1},
	Spaa[a, Sp[1], b, c],
	TestID -> "Spaa to integer"
];
Test[
	{Spaa[a, P, b, c], {{P -> 1}}},
	{Spaa[a, Sp[1], b, c]},
	TestID -> "Spaa to integer deep"
];


TestSubexpression[
	{0, a -> Q},
	TestID -> "zero replace spinor"
];
TestSubexpression[
	{Spaa[a, b], a -> Q},
	TestID -> "don't replace spinor et end of chain"
];
TestSubexpression[
	{Spab[a, b, P], P -> Q},
	TestID -> "don't replace LVector et end of chain"
];
TestSubexpression[
	{Spab[a, SpAssoc[P], SpRef[P], SpM[P, +1]], P -> Q},
	TestID -> "don't replace LVector inside other labels"
];


SetOptions[TestCaseRules,
	"Rule1" -> (P -> Q),
	"Rule2" -> (b -> Q2)
];


TestCaseRules[
	Spab[a, P, b, L, c],
	{
		{Spab[a, Q, b, L, c], TestID -> "Spab basic: P -> Q"},
		{Spab[a, P, Q2, L, c], TestID -> "Spab basic: b -> Q2"},
		{Spab[a, Q, Q, L, c], TestID -> "Spab basic: P | b -> Q"},
		{Spab[a, Q, Q, Q, c], TestID -> "Spab basic: _ -> Q"},
		{Spab[a, Q[P], Q[b], L, c], TestID -> "Spab basic: w:(P | b) -> Q[w]"},
		{Spab[a, Q, Q2, L, c], TestID -> "Spab basic: {P -> Q, b -> Q2}"}
	},
	TestID -> "Spab basic"
];
TestCaseRules[
	Spaa[a, P, b, Q, x, c],
	{
		{Spaa[a, Q, b, Q, x, c], TestID -> "Spaa with non-LVector: P -> Q"},
		{Spaa[a, P, Q2, Q, x, c], TestID -> "Spaa with non-LVector: b -> Q2"},
		{Spaa[a, Q, Q, Q, x, c],
			TestID -> "Spaa with non-LVector: P | b -> Q"},
		{Spaa[a, Q, Q, Q, Q, c], TestID -> "Spaa with non-LVector: _ -> Q"},
		{Spaa[a, Q[P], Q[b], Q, x, c],
			TestID -> "Spaa with non-LVector: w:(P | b) -> Q[w]"},
		{Spaa[a, Q, Q2, Q, x, c],
			TestID -> "Spaa with non-LVector: {P -> Q, b -> Q2}"}
	},
	TestID -> "Spaa with non-LVector"
];
TestCaseRules[
	Spaa[a, P, P, c],
	{
		{Spaa[a, Q, Q, c], TestID -> "Spaa P two times: P -> Q"},
		{Spaa[a, P, P, c], TestID -> "Spaa P two times: b -> Q2"},
		{Spaa[a, Q, Q, c], TestID -> "Spaa P two times: P | b -> Q"},
		{Spaa[a, Q, Q, c], TestID -> "Spaa P two times: _ -> Q"},
		{Spaa[a, Q[P], Q[P], c],
			TestID -> "Spaa P two times: w:(P | b) -> Q[w]"},
		{Spaa[a, Q, Q, c], TestID -> "Spaa P two times: {P -> Q, b -> Q2}"}
	},
	TestID -> "Spaa P two times"
];
TestCaseRules[
	s[b, P],
	{
		{s[b, Q], TestID -> "s: P -> Q"},
		{s[Q2, P], TestID -> "s: b -> Q2"},
		{s[Q, Q], TestID -> "s: P | b -> Q"},
		{s[Q, Q], TestID -> "s: _ -> Q"},
		{s[Q[b], Q[P]], TestID -> "s: w:(P | b) -> Q[w]"},
		{s[Q2, Q], TestID -> "s: {P -> Q, b -> Q2}"}
	},
	TestID -> "s"
];
TestCaseRules[
	MP[b, P],
	{
		{MP[b, Q], TestID -> "MP: P -> Q"},
		{MP[Q2, P], TestID -> "MP: b -> Q2"},
		{MP[Q, Q], TestID -> "MP: P | b -> Q"},
		{MP[Q, Q], TestID -> "MP: _ -> Q"},
		{MP[Q[b], Q[P]], TestID -> "MP: w:(P | b) -> Q[w]"},
		{MP[Q2, Q], TestID -> "MP: {P -> Q, b -> Q2}"}
	},
	TestID -> "MP"
];
TestCaseRules[
	MP[P, P],
	{
		{MP[Q, Q], TestID -> "MP same: P -> Q"},
		{MP[P, P], TestID -> "MP same: b -> Q2"},
		{MP[Q, Q], TestID -> "MP same: P | b -> Q"},
		{MP[Q, Q], TestID -> "MP same: _ -> Q"},
		{MP[Q[P], Q[P]], TestID -> "MP same: w:(P | b) -> Q[w]"},
		{MP[Q, Q], TestID -> "MP same: {P -> Q, b -> Q2}"}
	},
	TestID -> "MP same"
];
TestCaseRules[
	Sm[P],
	{
		{Sm[Q], TestID -> "Sm: P -> Q"},
		{Sm[P], TestID -> "Sm: b -> Q2"},
		{Sm[Q], TestID -> "Sm: P | b -> Q"},
		{Sm[Q], TestID -> "Sm: _ -> Q"},
		{Sm[Q[P]], TestID -> "Sm: w:(P | b) -> Q[w]"},
		{Sm[Q], TestID -> "Sm: {P -> Q, b -> Q2}"}
	},
	TestID -> "Sm"
];
TestCaseRules[
	Spab[a, P, b],
	{
		{Q Spab[a, P, b],
			TestID -> "replacement contains old LVector: P -> Q P"},
		{Spab[a, P, b],
			TestID -> "replacement contains old LVector: b -> Q2 b"},
		{Q Spab[a, P, b],
			TestID -> "replacement contains old LVector: P | b -> Q P"},
		{Q Spab[a, P, b],
			TestID -> "replacement contains old LVector: _ -> Q P"},
		{Q[P]Spab[a, P, b],
			TestID -> "replacement contains old LVector: w:(P | b) -> Q[w] P"},
		{Q Spab[a, P, b],
			TestID -> "replacement contains old LVector: {P -> Q P, b -> Q2 b}"}
	},
	TestID -> "replacement contains old LVector",
	"Rule1" -> (P -> Q P),
	"Rule2" -> (b -> Q2 b),
	"RuleDelayedFunction" -> (P Q[#]&)
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
