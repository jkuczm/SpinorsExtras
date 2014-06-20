(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Utilities`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`"
}];


SetOptions[{TestCaseRulesDouble, Test, TestSubexpression},
	ApplyToInput -> ReplaceSpinor
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
	{Spab[x, c, x], {{x -> {1, 2}}}},
	{Spab[Sp[2], c, Sp[1]]},
	TestID -> "to integer deep"
];


TestSubexpression[
	{Spab[c, x, d], x -> r1},
	TestID -> "don't replace arg inside spinor chain"
];
TestSubexpression[
	{Spab[SpAssoc[x], c, SpRef[x]], x -> r1},
	TestID -> "don't replace arg inside other labels"
];


SetOptions[TestCaseRulesDouble,
	"x" -> x, "r1" -> r1, "b1" -> b1, "a1" -> a1,
	"y" -> y, "r2" -> r2, "b2" -> b2, "a2" -> a2
];


TestCaseRulesDouble[
	Spaa[x, c, d, y],
	{
		{Spaa[r1, c, d, y],
			TestID -> "Spaa different left right: x -> r1"},
		{Spaa[a1, c, d, y],
			TestID -> "Spaa different left right: x -> {b1, a1}"}
		,
		{Spaa[x, c, d, r2],
			TestID -> "Spaa different left right: y -> r2"},
		{Spaa[x, c, d, a2],
			TestID -> "Spaa different left right: y -> {b2, a2}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa different left right: x | y -> r1"},
		{Spaa[a1, c, d, a1],
			TestID -> "Spaa different left right: x | y -> {b1, a1}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa different left right: _ -> r1"},
		{Spaa[a1, c, d, a1],
			TestID -> "Spaa different left right: _ -> {b1, a1}"}
		,
		{Spaa[r1[x], c, d, r1[y]],
			TestID -> "Spaa different left right: w:(x | y) :> r1[w]"},
		{Spaa[a1[x], c, d, a1[y]],
			TestID -> "Spaa different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spaa[r1, c, d, r2],
			TestID -> "Spaa different left right: {x -> r1, y -> r2}"},
		{Spaa[a1, c, d, a2],
			TestID -> "Spaa different left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spaa different left right"
];
TestCaseRulesDouble[
	Spaa[x, c, y, d],
	{
		{Spaa[r1, c, y, d],
			TestID -> "Spaa different left and inside: x -> r1"},
		{Spaa[a1, c, y, d],
			TestID -> "Spaa different left and inside: x -> {b1, a1}"}
		,
		{Spaa[x, c, y, d],
			TestID -> "Spaa different left and inside: y -> r2"},
		{Spaa[x, c, y, d],
			TestID -> "Spaa different left and inside: y -> {b2, a2}"}
		,
		{Spaa[r1, c, y, d],
			TestID -> "Spaa different left and inside: x | y -> r1"},
		{Spaa[a1, c, y, d],
			TestID -> "Spaa different left and inside: x | y -> {b1, a1}"}
		,
		{Spaa[r1, c, y, r1],
			TestID -> "Spaa different left and inside: _ -> r1"},
		{Spaa[a1, c, y, a1],
			TestID -> "Spaa different left and inside: _ -> {b1, a1}"}
		,
		{Spaa[r1[x], c, y, d],
			TestID -> "Spaa different left and inside: w:(x | y) :> r1[w]"},
		{Spaa[a1[x], c, y, d],
			TestID -> "Spaa different left and inside: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spaa[r1, c, y, d],
			TestID -> "Spaa different left and inside: {x -> r1, y -> r2}"},
		{Spaa[a1, c, y, d],
			TestID -> "Spaa different left and inside: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spaa different left and inside"
];
TestCaseRulesDouble[
	Spaa[x, c, d, x],
	{
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: x -> r1"},
		{Spaa[a1, c, d, a1],
			TestID -> "Spaa same left right: x -> {b1, a1}"}
		,
		{Spaa[x, c, d, x],
			TestID -> "Spaa same left right: y -> r2"},
		{Spaa[x, c, d, x],
			TestID -> "Spaa same left right: y -> {b2, a2}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: x | y -> r1"},
		{Spaa[a1, c, d, a1],
			TestID -> "Spaa same left right: x | y -> {b1, a1}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: _ -> r1"},
		{Spaa[a1, c, d, a1],
			TestID -> "Spaa same left right: _ -> {b1, a1}"}
		,
		{Spaa[r1[x], c, d, r1[x]],
			TestID -> "Spaa same left right: w:(x | y) :> r1[w]"},
		{Spaa[a1[x], c, d, a1[x]],
			TestID -> "Spaa same left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spaa[r1, c, d, r1],
			TestID -> "Spaa same left right: {x -> r1, y -> r2}"},
		{Spaa[a1, c, d, a1],
			TestID -> "Spaa same left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spaa same left right"
];
TestCaseRulesDouble[
	Spab[x, c, y],
	{
		{Spab[r1, c, y],
			TestID -> "Spab different left right: x -> r1"},
		{Spab[a1, c, y],
			TestID -> "Spab different left right: x -> {b1, a1}"}
		,
		{Spab[x, c, r2],
			TestID -> "Spab different left right: y -> r2"},
		{Spab[x, c, b2],
			TestID -> "Spab different left right: y -> {b2, a2}"}
		,
		{Spab[r1, c, r1],
			TestID -> "Spab different left right: x | y -> r1"},
		{Spab[a1, c, b1],
			TestID -> "Spab different left right: x | y -> {b1, a1}"}
		,
		{Spab[r1, c, r1],
			TestID -> "Spab different left right: _ -> r1"},
		{Spab[a1, c, b1],
			TestID -> "Spab different left right: _ -> {b1, a1}"}
		,
		{Spab[r1[x], c, r1[y]],
			TestID -> "Spab different left right: w:(x | y) :> r1[w]"},
		{Spab[a1[x], c, b1[y]],
			TestID -> "Spab different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spab[r1, c, r2],
			TestID -> "Spab different left right: {x -> r1, y -> r2}"},
		{Spab[a1, c, b2],
			TestID -> "Spab different left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spab different left right"
];
TestCaseRulesDouble[
	Spab[x, c, x],
	{
		{Spab[r1, c, r1],
			TestID -> "Spab same left right: x -> r1"},
		{Spab[a1, c, b1],
			TestID -> "Spab same left right: x -> {b1, a1}"}
		,
		{Spab[x, c, x],
			TestID -> "Spab same left right: y -> r2"},
		{Spab[x, c, x],
			TestID -> "Spab same left right: y -> {b2, a2}"}
		,
		{Spab[r1, c, r1],
			TestID -> "Spab same left right: x | y -> r1"},
		{Spab[a1, c, b1],
			TestID -> "Spab same left right: x | y -> {b1, a1}"}
		,
		{Spab[r1, c, r1],
			TestID -> "Spab same left right: _ -> r1"},
		{Spab[a1, c, b1],
			TestID -> "Spab same left right: _ -> {b1, a1}"}
		,
		{Spab[r1[x], c, r1[x]],
			TestID -> "Spab same left right: w:(x | y) :> r1[w]"},
		{Spab[a1[x], c, b1[x]],
			TestID -> "Spab same left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{Spab[r1, c, r1],
			TestID -> "Spab same left right: {x -> r1, y -> r2}"},
		{Spab[a1, c, b1],
			TestID -> "Spab same left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "Spab same left right"
];
TestCaseRulesDouble[
	SmBA[x, y],
	{
		{SmBA[r1, y],
			TestID -> "SmBA different left right: x -> r1"},
		{SmBA[b1, y],
			TestID -> "SmBA different left right: x -> {b1, a1}"}
		,
		{SmBA[x, r2],
			TestID -> "SmBA different left right: y -> r2"},
		{SmBA[x, a2],
			TestID -> "SmBA different left right: y -> {b2, a2}"}
		,
		{SmBA[r1, r1],
			TestID -> "SmBA different left right: x | y -> r1"},
		{SmBA[b1, a1],
			TestID -> "SmBA different left right: x | y -> {b1, a1}"}
		,
		{SmBA[r1, r1],
			TestID -> "SmBA different left right: _ -> r1"},
		{SmBA[b1, a1],
			TestID -> "SmBA different left right: _ -> {b1, a1}"}
		,
		{SmBA[r1[x], r1[y]],
			TestID -> "SmBA different left right: w:(x | y) :> r1[w]"},
		{SmBA[b1[x], a1[y]],
			TestID -> "SmBA different left right: w:(x | y) :> {b1[w], a1[w]}"}
		,
		{SmBA[r1, r2],
			TestID -> "SmBA different left right: {x -> r1, y -> r2}"},
		{SmBA[b1, a2],
			TestID -> "SmBA different left right: {x -> {b1, a1}, y -> {b2, a2}}"}
	},
	TestID -> "SmBA different left right"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
