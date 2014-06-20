(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Ref`",
	"SpinorsExtras`RefUtilities`",
	"SpinorsExtras`Composite`"
	,
	"ProtectionUtilities`" (* ProtectSyntax *)
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {p1, p2, p3, a, b, c, ref1, ref2, ref3}
];


simpleTestExpr[refArgB_, refArgA_] :=
	Spaa[p1, p2] Spbb[refArgB, p3] / Spbb[refArgB, p2] +
	Spbb[p3, p2] Spaa[p1, refArgA] / Spaa[p2, refArgA];
	
ProtectSyntax[simpleTestExpr];


complexTestExpr[refArg_] := (
	m^2 * Spab[refArg, a, p3] / Spab[p2, a, p1]
	- (
		- Spaa[p1, refArg]
		+ (
			(
				(
					m^2 * (
						-4
						+ MP[a, p1] / MP[a, p2]
						+ 2 * MP[a, p2] / MP[a, p1]
					)
				) / 2
				+ MP[p1, p2]
			) * Spaa[a, refArg]
		) / Spab[a, p2, p1]
	) * Spbb[p3, p2]
) / Spaa[p3, refArg];
	
ProtectSyntax[complexTestExpr];


SetOptions[{Test, TestCasePatternsRules},
	ApplyToInput -> RefSimplify
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Interface*)


TestCasePatternsRules[
	simpleTestExpr[ref1, ref1] + simpleTestExpr[SpRef[b], SpRef[a]],
	{
		{
			simpleTestExpr[p3, p1] + simpleTestExpr[SpRef[b], SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
ref1"
		},
		{
			simpleTestExpr[p3, p1] + simpleTestExpr[SpRef[b], SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
ref1 -> {a, p1, b, p3}"
		}
		,
		{
			simpleTestExpr[ref1, ref1] + simpleTestExpr[p3, SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
SpRef[b]"
		},
		{
			simpleTestExpr[ref1, ref1] + simpleTestExpr[ref1, SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
SpRef[b] -> Except[p3 | LvBA[p3, _]"
		}
		,
		{
			simpleTestExpr[p3, p1] + simpleTestExpr[p3, SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
ref1 | SpRef[b]"
		},
		{
			simpleTestExpr[p3, p1] + simpleTestExpr[p3, SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
ref1 | SpRef[b] -> {a, p1, b, p3}"
		}
		,
		{
			simpleTestExpr[ref1, ref1] + simpleTestExpr[p3, p1],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
no args"
		},
		{
			(* All expressions are matched, so whole expression is replaced *)
			a,
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
_ -> {a, p1, b, p3}"
		}
		,
		{
			Simplify[
				simpleTestExpr[a[ref1], a[ref1]] +
				simpleTestExpr[a[SpRef[b]], SpRef[a]]
			]
			,
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
w:(ref1 | SpRef[b]) :> {a[w], p1[w], b[w], p3[w]}"
		}
		,
		{
			simpleTestExpr[p3, p1] + simpleTestExpr[p1, SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
{ref1, SpRef[b] -> Except[p3 | LvBA[p3, _]}"
		},
		{
			simpleTestExpr[p3, p1] + simpleTestExpr[p1, SpRef[a]],
			TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
{ref1 -> {a, p1, b, p3}, SpRef[b] -> Except[p3 | LvBA[p3, _]}"
		}
	},
	"Rule1" -> (ref1 -> {a, p1, b, p3}),
	"Rule2" -> (SpRef[b] -> Except[p3 | LvBA[p3, _]]),
	TestID -> "simple with ref1 + simple with SpRef[b], SpRef[a]: \
with B p3 and A p1"
];


(* ::Subsection:: *)
(*1 Rule with List*)


(* ::Subsubsection:: *)
(*explicit composite vectors*)


SetOptions[RefSimplify, "IndependentSpinors" -> False];

Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {LvBA[b, p1], LvBA[b, a], LvBA[p3, p1], LvBA[p3, a]}
	},
	simpleTestExpr[p3, p1],
	TestID -> "1 Rule with List: explicit composite vectors: \
B with p3, A with p1: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {p1, LvBA[b, p1], LvBA[b, a], LvBA[p1, a]}
	},
	simpleTestExpr[p1, p1],
	TestID -> "1 Rule with List: explicit composite vectors: \
B with p1, A with p1: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {LvBA[b, p3], LvBA[b, a], LvBA[p1, p3], LvBA[p1, a]}
	},
	simpleTestExpr[b, p3],
	TestID -> "1 Rule with List: explicit composite vectors: \
B without p3, A without p1: simple"
];


(* ::Subsubsection:: *)
(*no explicit composite vectors*)


SetOptions[RefSimplify, "IndependentSpinors" -> True];


Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {a, p1, b}
	},
	simpleTestExpr[p1, p1],
	TestID -> "1 Rule with List: no explicit composite vectors: \
with p1: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {a, b}
	},
	simpleTestExpr[a, a],
	TestID -> "1 Rule with List: no explicit composite vectors: \
without p1: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {a, p1, b, p3}
	},
	simpleTestExpr[p3, p1],
	TestID -> "1 Rule with List: no explicit composite vectors: \
with p1, p3: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> {a, p1, b, p3},
		"IndependentSpinors" -> False
	},
	simpleTestExpr[p1, p1],
	TestID -> "1 Rule with List: no explicit composite vectors: \
with p1, p3: simple, \"IndependentSpinors\" -> False"
];

Test[
	{
		complexTestExpr[ref1],
		ref1 -> {a, p1, b}
	},
	Spaa[a, p1] * Spbb[p3, p2] / Spaa[a, p3],
	TestID -> "1 Rule with List: no explicit composite vectors: \
with p1: complex"
];


(* ::Subsection:: *)
(*1 Rule with pattern*)


Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> Except[p2]
	},
	simpleTestExpr[p3, p1],
	TestID ->
		"1 Rule with pattern: B except ref1, p2; A except ref1, p2: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> Except[p2 | p3 | p1 | LvBA[p2 | p3, _] | LvBA[_, p2 | p1]]
	},
	simpleTestExpr[p1, p3],
	TestID -> "1 Rule with pattern: \
B except ref1, p2, p3; A except ref1, p2, p1: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> Except[p1 | p2 | p3 | LvBA[p1 | p2 | p3, _] | LvBA[_, p2]],
		"ExcludeReplacedRef" -> False
	},
	simpleTestExpr[ref1, p1],
	TestID -> "1 Rule with pattern: don't ExcludeReplacedRef: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1 -> Except[p1 | LvBA[_, p1]],
		"ReplacementPattern" -> _?(SpinorQ[#] && ! MatchQ[#, p2 ]&)
	},
	simpleTestExpr[p3, p3],
	TestID ->
		"1 Rule with pattern: explicit \"ReplacementPattern\" option: simple"
];

Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1,
		"ReplacementPattern" -> Except[p2]
	},
	simpleTestExpr[p3, p1],
	TestID -> "1 Rule with pattern: except p2: simple"
];
Test[
	{
		simpleTestExpr[ref1, ref1],
		ref1,
		"ReplacementPattern" -> Except[p2 | p3]
	},
	simpleTestExpr[p1, p1],
	TestID -> "1 Rule with pattern: ref1, p2, p3: simple"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
