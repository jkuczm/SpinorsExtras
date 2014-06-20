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
	ApplyToInput -> ReplaceBSpinor
];


(* ::Section:: *)
(*Tests*)


Test[
	{Spbb[1, c, d, e], 1 -> r1},
	Spbb[r1, c, d, e],
	TestID -> "from integer"
];
Test[
	{Spbb[1, c, d, e], {{1 -> r1}}},
	{Spbb[r1, c, d, e]},
	TestID -> "from integer deep"
];
Test[
	{Spbb[x, c, d, e], x -> 1},
	Spbb[Sp[1], c, d, e],
	TestID -> "to integer"
];
Test[
	{Spbb[x, c, d, e], {{x -> {1, 2}}}},
	{Spbb[Sp[1], c, d, e] - Spab[Sp[2], c, d, e]},
	TestID -> "to integer deep"
];


TestSubexpression[
	{Spab[x, c, d], x -> r1},
	TestID -> "don't replace A spinor: Spab"
];
TestSubexpression[
	{Spba[c, d, x], x -> r1},
	TestID -> "don't replace A spinor: Spba"
];
TestSubexpression[
	{Spaa[x, c, d, x], x -> r1},
	TestID -> "don't replace A spinor: Spaa"
];
TestSubexpression[
	{Spab[c, x, d], x -> r1},
	TestID -> "don't replace arg inside spinor chain"
];
TestSubexpression[
	{Spbb[SpAssoc[x], c, d, SpRef[x]], x -> r1},
	TestID -> "don't replace arg inside other labels"
];


SetOptions[TestCaseRulesDouble,
	"x" -> x, "r1" -> r1, "b1" -> b1, "a1" -> a1,
	"y" -> y, "r2" -> r2, "b2" -> b2, "a2" -> a2
];


TestCaseRulesDouble[
	Spbb[x, c, d, y],
	{
		{Spbb[r1, c, d, y],
			TestID -> "Spbb different left right: x -> r1"},
		{Spbb[b1, c, d, y] - Spab[a1, c, d, y],
			TestID -> "Spbb different left right: x -> {b1, a1}"}
		,
		{Spbb[x, c, d, r2],
			TestID -> "Spbb different left right: y -> r2"},
		{Spbb[x, c, d, b2] + Spba[x, c, d, a2],
			TestID -> "Spbb different left right: y -> {b2, a2}"}
		,
		{Spbb[r1, c, d, r1],
			TestID -> "Spbb different left right: x | y -> r1"},
		{
			Spbb[b1, c, d, b1] - Spab[a1, c, d, b1] + Spba[b1, c, d, a1]
			- Spaa[a1, c, d, a1]
			,
			TestID -> "Spbb different left right: x | y -> {b1, a1}"
		}
		,
		{Spbb[r1, c, d, r1],
			TestID -> "Spbb different left right: _ -> r1"},
		{
			Spbb[b1, c, d, b1] - Spab[a1, c, d, b1] + Spba[b1, c, d, a1]
			- Spaa[a1, c, d, a1]
			,
			TestID -> "Spbb different left right: _ -> {b1, a1}"
		}
		,
		{Spbb[r1[x], c, d, r1[y]],
			TestID -> "Spbb different left right: w:(x | y) :> r1[w]"},
		{
			Spbb[b1[x], c, d, b1[y]] - Spab[a1[x], c, d, b1[y]]
			+ Spba[b1[x], c, d, a1[y]] - Spaa[a1[x], c, d, a1[y]]
			,
			TestID -> "Spbb different left right: w:(x | y) :> {b1[w], a1[w]}"
		}
		,
		{Spbb[r1, c, d, r2],
			TestID -> "Spbb different left right: {x -> r1, y -> r2}"},
		{
			Spbb[b1, c, d, b2] - Spab[a1, c, d, b2] + Spba[b1, c, d, a2]
			- Spaa[a1, c, d, a2]
			,
			TestID ->
				"Spbb different left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spbb different left right"
];
TestCaseRulesDouble[
	Spbb[x, c, y, d],
	{
		{Spbb[r1, c, y, d],
			TestID -> "Spbb different left and inside: x -> r1"},
		{Spbb[b1, c, y, d] - Spab[a1, c, y, d],
			TestID -> "Spbb different left and inside: x -> {b1, a1}"}
		,
		{Spbb[x, c, y, d],
			TestID -> "Spbb different left and inside: y -> r2"},
		{Spbb[x, c, y, d],
			TestID -> "Spbb different left and inside: y -> {b2, a2}"}
		,
		{Spbb[r1, c, y, d],
			TestID -> "Spbb different left and inside: x | y -> r1"},
		{Spbb[b1, c, y, d] - Spab[a1, c, y, d],
			TestID -> "Spbb different left and inside: x | y -> {b1, a1}"}
		,
		{Spbb[r1, c, y, r1],
			TestID -> "Spbb different left and inside: _ -> r1"},
		{
			Spbb[b1, c, y, b1] - Spab[a1, c, y, b1] + Spba[b1, c, y, a1]
			- Spaa[a1, c, y, a1]
			,
			TestID -> "Spbb different left and inside: _ -> {b1, a1}"
		}
		,
		{Spbb[r1[x], c, y, d],
			TestID -> "Spbb different left and inside: w:(x | y) :> r1[w]"},
		{
			Spbb[b1[x], c, y, d] - Spab[a1[x], c, y, d]
			,
			TestID ->
				"Spbb different left and inside: w:(x | y) :> {b1[w], a1[w]}"
		}
		,
		{Spbb[r1, c, y, d],
			TestID -> "Spbb different left and inside: {x -> r1, y -> r2}"},
		{
			Spbb[b1, c, y, d] - Spab[a1, c, y, d]
			,
			TestID -> "Spbb different left and inside: \
{x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spbb different left and inside"
];
TestCaseRulesDouble[
	Spbb[x, c, d, x],
	{
		{Spbb[r1, c, d, r1],
			TestID -> "Spbb same left right: x -> r1"},
		{
			Spbb[b1, c, d, b1] - Spab[a1, c, d, b1] + Spba[b1, c, d, a1]
			- Spaa[a1, c, d, a1]
			,
			TestID -> "Spbb same left right: x -> {b1, a1}"
		}
		,
		{Spbb[x, c, d, x],
			TestID -> "Spbb same left right: y -> r2"},
		{Spbb[x, c, d, x],
			TestID -> "Spbb same left right: y -> {b2, a2}"}
		,
		{Spbb[r1, c, d, r1],
			TestID -> "Spbb same left right: x | y -> r1"},
		{
			Spbb[b1, c, d, b1] - Spab[a1, c, d, b1] + Spba[b1, c, d, a1]
			- Spaa[a1, c, d, a1]
			,
			TestID -> "Spbb same left right: x | y -> {b1, a1}"
		}
		,
		{Spbb[r1, c, d, r1],
			TestID -> "Spbb same left right: _ -> r1"},
		{
			Spbb[b1, c, d, b1] - Spab[a1, c, d, b1] + Spba[b1, c, d, a1]
			- Spaa[a1, c, d, a1]
			,
			TestID -> "Spbb same left right: _ -> {b1, a1}"
		}
		,
		{Spbb[r1[x], c, d, r1[x]],
			TestID -> "Spbb same left right: w:(x | y) :> r1[w]"},
		{
			Spbb[b1[x], c, d, b1[x]] - Spab[a1[x], c, d, b1[x]]
			+ Spba[b1[x], c, d, a1[x]] - Spaa[a1[x], c, d, a1[x]]
			,
			TestID -> "Spbb same left right: w:(x | y) :> {b1[w], a1[w]}"
		}
		,
		{Spbb[r1, c, d, r1],
			TestID -> "Spbb same left right: {x -> r1, y -> r2}"},
		{
			Spbb[b1, c, d, b1] - Spab[a1, c, d, b1] + Spba[b1, c, d, a1]
			- Spaa[a1, c, d, a1]
			,
			TestID -> "Spbb same left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spbb same left right"
];
TestCaseRulesDouble[
	Spab[x, c, y],
	{
		{Spab[x, c, y],
			TestID -> "Spab different left right: x -> r1"},
		{Spab[x, c, y],
			TestID -> "Spab different left right: x -> {b1, a1}"}
		,
		{Spab[x, c, r2],
			TestID -> "Spab different left right: y -> r2"},
		{Spab[x, c, b2] + Spaa[x, c, a2],
			TestID -> "Spab different left right: y -> {b2, a2}"}
		,
		{Spab[x, c, r1],
			TestID -> "Spab different left right: x | y -> r1"},
		{Spab[x, c, b1] + Spaa[x, c, a1],
			TestID -> "Spab different left right: x | y -> {b1, a1}"}
		,
		{Spab[x, c, r1],
			TestID -> "Spab different left right: _ -> r1"},
		{Spab[x, c, b1] + Spaa[x, c, a1],
			TestID -> "Spab different left right: _ -> {b1, a1}"}
		,
		{Spab[x, c, r1[y]],
			TestID -> "Spab different left right: w:(x | y) :> r1[w]"},
		{Spab[x, c, b1[y]] + Spaa[x, c, a1[y]],
			TestID -> "Spab different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spab[x, c, r2],
			TestID -> "Spab different left right: {x -> r1, y -> r2}"},
		{
			Spab[x, c, b2] + Spaa[x, c, a2]
			,
			TestID ->
				"Spab different left right: {x -> {b1, a1}, y -> {b2, a2}}"
		}
	},
	TestID -> "Spab different left right"
];
TestCaseRulesDouble[
	Spab[x, c, x],
	{
		{Spab[x, c, r1],
			TestID -> "Spab same left right: x -> r1"},
		{Spab[x, c, b1] + Spaa[x, c, a1],
			TestID -> "Spab same left right: x -> {b1, a1}"}
		,
		{Spab[x, c, x],
			TestID -> "Spab same left right: y -> r2"},
		{Spab[x, c, x],
			TestID -> "Spab same left right: y -> {b2, a2}"}
		,
		{Spab[x, c, r1],
			TestID -> "Spab same left right: x | y -> r1"},
		{Spab[x, c, b1] + Spaa[x, c, a1],
			TestID -> "Spab same left right: x | y -> {b1, a1}"}
		,
		{Spab[x, c, r1],
			TestID -> "Spab same left right: _ -> r1"},
		{Spab[x, c, b1] + Spaa[x, c, a1],
			TestID -> "Spab same left right: _ -> {b1, a1}"}
		,
		{Spab[x, c, r1[x]],
			TestID -> "Spab same left right: w:(x | y) :> r1[w]"},
		{Spab[x, c, b1[x]] + Spaa[x, c, a1[x]],
			TestID -> "Spab same left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spab[x, c, r1],
			TestID -> "Spab same left right: {x -> r1, y -> r2}"},
		{Spab[x, c, b1] + Spaa[x, c, a1],
			TestID -> "Spab same left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spab same left right"
];

(* Replacement in SmBA with list of non-spinors is not yet implemented. *)
TestCaseRulesDouble[
	SmBA[x, y],
	{
		{SmBA[r1, y],
			TestID -> "SmBA different left right: x -> r1"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: x -> {b1, a1}"}
		,
		{SmBA[x, y],
			TestID -> "SmBA different left right: y -> r2"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: y -> {b2, a2}"}
		,
		{SmBA[r1, y],
			TestID -> "SmBA different left right: x | y -> r1"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: x | y -> {b1, a1}"}
		,
		{SmBA[r1, y],
			TestID -> "SmBA different left right: _ -> r1"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: _ -> {b1, a1}"}
		,
		{SmBA[r1[x], y],
			TestID -> "SmBA different left right: w:(x | y) :> r1[w]"},
		{SmBA[x, y],
			TestID -> "SmBA different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{SmBA[r1, y],
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
		{SmBA[1, y] + Spaa[y, 2] ProjMinus,
			TestID -> "SmBA different left right: x -> {1, 2}"}
		,
		{SmBA[x, y],
			TestID -> "SmBA different left right: y -> {3, 4}"}
		,
		{SmBA[1, y] + Spaa[y, 2] ProjMinus,
			TestID -> "SmBA different left right: x | y -> {1, 2}"}
		,
		{SmBA[1, y] + Spaa[y, 2] ProjMinus,
			TestID -> "SmBA different left right: _ -> {1, 2}"}
		,
		{SmBA[x, y],
			TestID -> "SmBA different left right: w:(x | y) :> {1[w], 2[w]}"}
		,
		{SmBA[1, y] + Spaa[y, 2] ProjMinus,
			TestID -> "SmBA different left right: {x -> {1, 2}, y -> {3, 4}}"}
	},
	TestID -> "SmBA different left right, replace with list of spinors",
	"Rule1" -> (x -> {1, 2}),
	"Rule2" -> (y -> {3, 4})
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
