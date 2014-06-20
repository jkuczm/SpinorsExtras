(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`" (* SpAssoc *),
	"SpinorsExtras`RefUtilities`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {p1, p2, p3, a, b, nonNum},
	"LVectors" -> {P, Q},
	"RandomMomentaSpinors" -> {p1, p2, p3, a, b},
	"RandomMomentaLVectors" -> All,
	"SpRefMomentaLVectors" -> All
];

(*
	Make |p2] and |p3] proportional to |p1] so that functions of type
	[q|p1]/[q|p2] are independent of q
*)
DeclareSpinorMomentum[p2, La[p2] // N, RandomComplex[] Lat[p1] // N];
DeclareSpinorMomentum[p3, La[p3] // N, RandomComplex[] Lat[p1] // N];


invariantExpr =
	(Spbb[SpRef[P], p3] Spbb[SpRef[Q], p1]) /
	(Spbb[SpRef[P], p2] Spbb[SpRef[Q], p3]);

nonInvariantExpr = Spaa[p1, SpRef[P]] Spaa[p2, SpRef[Q]];


SetOptions[RefInvariantQ, "Accuracy" -> 15];
SetOptions[{TestCasePatternsRules, TestMatch, TestTrue, TestFalse},
	ApplyToInput -> RefInvariantQ
];
SetOptions[TestCasePatternsRules,
	"Rule1" -> (SpRef[P] -> {a, b}),
	"Rule2" -> (SpRef[Q] -> {a, b})
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Interface*)


TestCasePatternsRules[
	invariantExpr,
	{
		{True, TestID -> "interface: invariant: SpRef[P]"},
		{True, TestID -> "interface: invariant: SpRef[P] -> {a, b}"}
		,
		{True, TestID -> "interface: invariant: SpRef[Q]"},
		{True, TestID -> "interface: invariant: SpRef[Q] -> {a, b}"}
		,
		{True, TestID -> "interface: invariant: SpRef[P] | SpRef[Q]"},
		{True, TestID -> "interface: invariant: SpRef[P] | SpRef[Q] -> {a, b}"}
		,
		{True, TestID -> "interface: invariant: no args"},
		{
			(* All expressions are matched, so whole expression is replaced *)
			a == b,
			{HoldForm[Message[RefInvariantQ::NotANumber, {a, b}]]},
			TestID -> "interface: invariant: _ -> {a, b}"
		}
		,
		{
			True,
			TestID ->
				"interface: invariant: w:(SpRef[P] | SpRef[Q]) :> {a[w], b[w]}"
		}
		,
		{
			True,
			TestID -> "interface: invariant: {SpRef[P], SpRef[Q] -> {a, b}}"},
		{
			True,
			TestID -> "interface: invariant: \
{SpRef[P] -> {a, b}, SpRef[Q] -> {a, b}}"
		}
	},
	TestID -> "interface: invariant"
];


TestCasePatternsRules[
	nonInvariantExpr,
	{
		{False, TestID -> "interface: non-invariant: SpRef[P]"},
		{False, TestID -> "interface: non-invariant: SpRef[P] -> {a, b}"}
		,
		{False, TestID -> "interface: non-invariant: SpRef[Q]"},
		{False, TestID -> "interface: non-invariant: SpRef[Q] -> {a, b}"}
		,
		{False, TestID -> "interface: non-invariant: SpRef[P] | SpRef[Q]"},
		{False,
			TestID ->
				"interface: non-invariant: SpRef[P] | SpRef[Q] -> {a, b}"}
		,
		{
			False,
			{HoldForm @ Message[
				RefInvariantQ::NonInvariantForRef, {SpRef[P], SpRef[Q]}
			]},
			TestID -> "interface: non-invariant: no args"
		},
		{
			(* All expressions are matched, so whole expression is replaced *)
			a == b,
			{HoldForm[Message[RefInvariantQ::NotANumber, {a, b}]]},
			TestID -> "interface: non-invariant: _ -> {a, b}"
		}
		,
		{
			False,
			TestID -> "interface: non-invariant: \
w:(SpRef[P] | SpRef[Q]) :> {a[w], b[w]}"
		}
		,
		{
			False,
			{HoldForm @ Message[
				RefInvariantQ::NonInvariantForRef,
				{SpRef[P], SpRef[Q] -> {a, b}}
			]},
			TestID ->
				"interface: non-invariant: {SpRef[P], SpRef[Q] -> {a, b}}"
		},
		{
			False,
			{HoldForm @ Message[
				RefInvariantQ::NonInvariantForRef,
				{SpRef[P] -> {a, b}, SpRef[Q] -> {a, b}}
			]},
			TestID -> "interface: non-invariant: \
{SpRef[P] -> {a, b}, SpRef[Q] -> {a, b}}"
		}
	}
	,
	{
		HoldForm @ Message[
			RefInvariantQ::NonInvariantForRef,
			{SpRef[P] -> {a, b}}
		],
		HoldForm @ Message[
			RefInvariantQ::NonInvariantForRef,
			{SpRef[Q] -> {a, b}}
		]
	},
	TestID -> "interface: non-invariant"
];


(* ::Subsection:: *)
(*Messages*)


TestMatch[
	RefInvariantQ[Spaa[SpRef[P], nonNum], SpRef[P] -> {a, b}],
	Spaa[a, nonNum] == Spaa[b, nonNum],
	{HoldForm @ Message[
		RefInvariantQ::NotANumber,
		{Spaa[a, nonNum], Spaa[b, nonNum]}
	]},
	TestID -> "NotANumber message"
];

TestTrue[
	RefInvariantQ[{invariantExpr, invariantExpr}, SpRef[P]],
	TestID -> "List of numbers: no NotANumber message"
];

TestTrue[
	RefInvariantQ[Spaa[p1, SpRef[P]], SpRef[Q]],
	{HoldForm[Message[RefInvariantQ::RefAbsent, SpRef[Q]]]},
	TestID -> "RefAbsent message"
];


(* ::Subsection:: *)
(*Automatic numerics declaration*)

TestFalse[
	RefInvariantQ[Spaa[SpAssoc[Q, SpRef[P]], SpRef[P]], SpRef[P]],
	TestID -> "Automatic numerics declaration: SpAssoc with tested ref"
];


(* ::Subsection:: *)
(*Non-acceptable reference spinor in explicit list*)


(*TestFalse[
	{invariantExpr, SpRef[P] -> {p1, p2, p3, a}},
	TestID -> "non-acceptable list"
];

TestFalse[
	{invariantExpr, {SpRef[P], SpRef[Q] -> {p1, p2, p3, a}}},
	TestID -> "random, non-acceptable list"
];
TestFalse[
	{invariantExpr, {SpRef[P] -> {p1, p2, p3, a}, SpRef[Q]}},
	TestID -> "non-acceptable list, random"
];
TestFalse[
	{invariantExpr, {SpRef[P] -> {p1, p3, a}, SpRef[Q] -> {p1, p2, p3, a}}},
	TestID -> "acceptable list, non-acceptable list"
];
TestFalse[
	{invariantExpr, {SpRef[P] -> {p1, p2, p3, a}, SpRef[Q] -> {p1, p2, a}}},
	TestID -> "non-acceptable list, acceptable list"
];
TestFalse[
	{invariantExpr, {SpRef[P] -> {p1, p2, p3, a}, SpRef[Q] -> {p1, p2, p3, a}}},
	TestID -> "non-acceptable list, non-acceptable list"
];*)


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
