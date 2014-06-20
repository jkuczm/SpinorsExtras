(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Utilities`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`"
}];


SetOptions[{TestCaseRulesDouble, TestCaseRules, Test, TestSubexpression},
	ApplyToInput -> ReplaceASpinor
];


(* ::Section:: *)
(*Tests*)


Test[
	{Spaa[1, c, d, e], 1 -> r1},
	Spaa[r1, c, d, e],
	TestID -> "from integer"
];
Test[
	{Spaa[1, c, d, e], {{1 -> r1}}},
	{Spaa[r1, c, d, e]},
	TestID -> "from integer deep"
];
Test[
	{Spaa[x, c, d, e], x -> 1},
	Spaa[Sp[1], c, d, e],
	TestID -> "to integer"
];
Test[
	{Spaa[x, c, d, e], {{x -> {2, 1}}}},
	{Spaa[Sp[2], c, d, e] - Spba[Sp[1], c, d, e]},
	TestID -> "to integer deep"
];


TestSubexpression[
	{Spba[x, c, d], x -> r1},
	TestID -> "don't replace B spinor: Spba"
];
TestSubexpression[
	{Spab[c, d, x], x -> r1},
	TestID -> "don't replace B spinor: Spab"
];
TestSubexpression[
	{Spbb[x, c, d, x], x -> r1},
	TestID -> "don't replace B spinor: Spbb"
];
TestSubexpression[
	{Spba[c, x, d], x -> r1},
	TestID -> "don't replace arg inside spinor chain"
];
TestSubexpression[
	{Spaa[SpAssoc[x], c, d, SpRef[x]], x -> r1},
	TestID -> "don't replace arg inside other labels"
];


SetOptions[TestCaseRulesDouble,
	"x" -> x, "r1" -> r1, "b1" -> a1, "a1" -> b1,
	"y" -> y, "r2" -> r2, "b2" -> a2, "a2" -> b2
];


TestCaseRulesDouble[
	Spaa[x, c, d, y],
	{
		{Spaa[r1, c, d, y],
			TestID -> "Spaa different left right: x -> r1"},
		{Spaa[a1, c, d, y] - Spba[b1, c, d, y],
			TestID -> "Spaa different left right: x -> {b1, a1}"}
		,
		{Spaa[x, c, d, r2],
			TestID -> "Spaa different left right: y -> r2"},
		{Spaa[x, c, d, a2] + Spab[x, c, d, b2],
			TestID -> "Spaa different left right: y -> {b2, a2}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa different left right: x | y -> r1"},
		{
			Spaa[a1, c, d, a1] - Spba[b1, c, d, a1] + Spab[a1, c, d, b1]
			- Spbb[b1, c, d, b1]
			,
			TestID -> "Spaa different left right: x | y -> {b1, a1}"
		}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa different left right: _ -> r1"},
		{
			Spaa[a1, c, d, a1] - Spba[b1, c, d, a1] + Spab[a1, c, d, b1]
			- Spbb[b1, c, d, b1]
			,
			TestID -> "Spaa different left right: _ -> {b1, a1}"}
		,
		{Spaa[r1[x], c, d, r1[y]],
			TestID -> "Spaa different left right: w:(x | y) :> r1[w]"},
		{
			Spaa[a1[x], c, d, a1[y]] - Spba[b1[x], c, d, a1[y]]
			+ Spab[a1[x], c, d, b1[y]] - Spbb[b1[x], c, d, b1[y]]
			,
			TestID -> "Spaa different left right: w:(x | y) :> {b1[w], a1[w]}"
		}
		,
		{Spaa[r1, c, d, r2],
			TestID -> "Spaa different left right: {x -> r1, y -> r2}"},
		{
			Spaa[a1, c, d, a2] - Spba[b1, c, d, a2] + Spab[a1, c, d, b2]
			- Spbb[b1, c, d, b2]
			,
			TestID ->
				"Spaa different left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spaa different left right"
];
TestCaseRulesDouble[
	Spaa[x, c, y, d],
	{
		{Spaa[r1, c, y, d],
			TestID -> "Spaa different left and inside: x -> r1"},
		{Spaa[a1, c, y, d] - Spba[b1, c, y, d],
			TestID -> "Spaa different left and inside: x -> {b1, a1}"}
		,
		{Spaa[x, c, y, d],
			TestID -> "Spaa different left and inside: y -> r2"},
		{Spaa[x, c, y, d],
			TestID -> "Spaa different left and inside: y -> {b2, a2}"}
		,
		{Spaa[r1, c, y, d],
			TestID -> "Spaa different left and inside: x | y -> r1"},
		{Spaa[a1, c, y, d] - Spba[b1, c, y, d],
			TestID -> "Spaa different left and inside: x | y -> {b1, a1}"}
		,
		{Spaa[r1, c, y, r1],
			TestID -> "Spaa different left and inside: _ -> r1"},
		{
			Spaa[a1, c, y, a1] - Spba[b1, c, y, a1] + Spab[a1, c, y, b1]
			- Spbb[b1, c, y, b1]
			,
			TestID -> "Spaa different left and inside: _ -> {b1, a1}"
		}
		,
		{Spaa[r1[x], c, y, d],
			TestID -> "Spaa different left and inside: w:(x | y) :> r1[w]"},
		{
			Spaa[a1[x], c, y, d] - Spba[b1[x], c, y, d]
			,
			TestID ->
				"Spaa different left and inside: w:(x | y) :> {b1[w], a1[w]}"
		}
		,
		{Spaa[r1, c, y, d],
			TestID -> "Spaa different left and inside: {x -> r1, y -> r2}"},
		{
			Spaa[a1, c, y, d] - Spba[b1, c, y, d]
			,
			TestID -> "Spaa different left and inside: \
{x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spaa different left and inside"
];
TestCaseRulesDouble[
	Spaa[x, c, d, x],
	{
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: x -> r1"},
		{
			Spaa[a1, c, d, a1] - Spba[b1, c, d, a1] + Spab[a1, c, d, b1]
			- Spbb[b1, c, d, b1]
			,
			TestID -> "Spaa same left right: x -> {b1, a1}"
		}
		,
		{Spaa[x, c, d, x],
			TestID -> "Spaa same left right: y -> r2"},
		{Spaa[x, c, d, x],
			TestID -> "Spaa same left right: y -> {b2, a2}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: x | y -> r1"},
		{
			Spaa[a1, c, d, a1] - Spba[b1, c, d, a1] + Spab[a1, c, d, b1]
			- Spbb[b1, c, d, b1]
			,
			TestID -> "Spaa same left right: x | y -> {b1, a1}"
		}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: _ -> r1"},
		{
			Spaa[a1, c, d, a1] - Spba[b1, c, d, a1] + Spab[a1, c, d, b1]
			- Spbb[b1, c, d, b1]
			,
			TestID -> "Spaa same left right: _ -> {b1, a1}"
		}
		,
		{Spaa[r1[x], c, d, r1[x]],
			TestID -> "Spaa same left right: w:(x | y) :> r1[w]"},
		{
			Spaa[a1[x], c, d, a1[x]] - Spba[b1[x], c, d, a1[x]]
			+ Spab[a1[x], c, d, b1[x]] - Spbb[b1[x], c, d, b1[x]]
			,
			TestID -> "Spaa same left right: w:(x | y) :> {b1[w], a1[w]}"
		}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: {x -> r1, y -> r2}"},
		{
			Spaa[a1, c, d, a1] - Spba[b1, c, d, a1] + Spab[a1, c, d, b1]
			- Spbb[b1, c, d, b1]
			,
			TestID -> "Spaa same left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spaa same left right"
];
TestCaseRulesDouble[
	Spba[x, c, y],
	{
		{Spba[x, c, y],
			TestID -> "Spba different left right: x -> r1"},
		{Spba[x, c, y],
			TestID -> "Spba different left right: x -> {b1, a1}"}
		,
		{Spba[x, c, r2],
			TestID -> "Spba different left right: y -> r2"},
		{Spba[x, c, a2] + Spbb[x, c, b2],
			TestID -> "Spba different left right: y -> {b2, a2}"}
		,
		{Spba[x, c, r1],
			TestID -> "Spba different left right: x | y -> r1"},
		{Spba[x, c, a1] + Spbb[x, c, b1],
			TestID -> "Spba different left right: x | y -> {b1, a1}"}
		,
		{Spba[x, c, r1],
			TestID -> "Spba different left right: _ -> r1"},
		{Spba[x, c, a1] + Spbb[x, c, b1],
			TestID -> "Spba different left right: _ -> {b1, a1}"}
		,
		{Spba[x, c, r1[y]],
			TestID -> "Spba different left right: w:(x | y) :> r1[w]"},
		{Spba[x, c, a1[y]] + Spbb[x, c, b1[y]],
			TestID -> "Spba different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spba[x, c, r2],
			TestID -> "Spba different left right: {x -> r1, y -> r2}"},
		{
			Spba[x, c, a2] + Spbb[x, c, b2]
			,
			TestID ->
				"Spba different left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spba different left right"
];
TestCaseRulesDouble[
	Spba[x, c, x],
	{
		{Spba[x, c, r1],
			TestID -> "Spba same left right: x -> r1"},
		{Spba[x, c, a1] + Spbb[x, c, b1],
			TestID -> "Spba same left right: x -> {b1, a1}"}
		,
		{Spba[x, c, x],
			TestID -> "Spba same left right: y -> r2"},
		{Spba[x, c, x],
			TestID -> "Spba same left right: y -> {b2, a2}"}
		,
		{Spba[x, c, r1],
			TestID -> "Spba same left right: x | y -> r1"},
		{Spba[x, c, a1] + Spbb[x, c, b1],
			TestID -> "Spba same left right: x | y -> {b1, a1}"}
		,
		{Spba[x, c, r1],
			TestID -> "Spba same left right: _ -> r1"},
		{Spba[x, c, a1] + Spbb[x, c, b1],
			TestID -> "Spba same left right: _ -> {b1, a1}"}
		,
		{Spba[x, c, r1[x]],
			TestID -> "Spba same left right: w:(x | y) :> r1[w]"},
		{Spba[x, c, a1[x]] + Spbb[x, c, b1[x]],
			TestID -> "Spba same left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spba[x, c, r1],
			TestID -> "Spba same left right: {x -> r1, y -> r2}"},
		{Spba[x, c, a1] + Spbb[x, c, b1],
			TestID -> "Spba same left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spba same left right"
];

(* Replacement in SmBA with list of non-spinors is not yet implemented. *)
TestCaseRulesDouble[
	SmBA[x, y],
	{
		{SmBA[x, y],
			TestID -> "SmBA different left right: x -> r1"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: x -> {b1, a1}"}
		,
		{SmBA[x, r2],
			TestID -> "SmBA different left right: y -> r2"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: y -> {b2, a2}"}
		,
		{SmBA[x, r1],
			TestID -> "SmBA different left right: x | y -> r1"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: x | y -> {b1, a1}"}
		,
		{SmBA[x, r1],
			TestID -> "SmBA different left right: _ -> r1"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: _ -> {b1, a1}"}
		,
		{SmBA[x, r1[y]],
			TestID -> "SmBA different left right: w:(x | y) :> r1[w]"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{SmBA[x, r2],
			TestID -> "SmBA different left right: {x -> r1, y -> r2}"},
		{
			SmBA[x, y]
			,
			TestID ->
				"SmBA different left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "SmBA different left right"
];
TestCaseRules[
	SmBA[x, y],
	{
		{SmBA[x, y],
			TestID -> "SmBA different left right: x -> {1, 2}"}
		,
		{SmBA[x, 3] + Spbb[x, 4] ProjPlus,
			TestID -> "SmBA different left right: y -> {3, 4}"}
		,
		{SmBA[x, 1] + Spbb[x, 2] ProjPlus,
			TestID -> "SmBA different left right: x | y -> {1, 2}"}
		,
		{SmBA[x, 1] + Spbb[x, 2] ProjPlus,
			TestID -> "SmBA different left right: _ -> {1, 2}"}
		,
		{SmBA[x, y],
			TestID -> "SmBA different left right: w:(x | y) :> {1[w], 2[w]}"}
		,
		{SmBA[x, 3] + Spbb[x, 4] ProjPlus,
			TestID -> "SmBA different left right: {x -> {1, 2}, y -> {3, 4}}"}
	},
	TestID -> "SmBA different left right, replace with list of spinors",
	"Rule1" -> (x -> {1, 2}),
	"Rule2" -> (y -> {3, 4})
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
