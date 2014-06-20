(* ::Package:: *)

BeginPackage["SpinorsExtras`Tests`TestCases`", {"Spinors`"}]


(* ::Section:: *)
(*Public*)


TestCaseSpinorQLVectorQ::usage =
"\
TestCaseSpinorQLVectorQ[input, {spinorQExpected, lVectorQExpected}] \
Performs two tests: \
compares result of SpinorQ[input] with spinorQExpected \
and result of LVectorQ[input] with lVectorQExpected,
assumes also that no messages are generated in both tests.\

TestCaseSpinorQLVectorQ[input, {spinorQExpected, lVectorQExpected}, messages] \
Performs two tests: \
compares result of SpinorQ[input] with spinorQExpected \
and result of LVectorQ[input] with lVectorQExpected,
assumes also that given messages are generated in both tests.\

TestCaseSpinorQLVectorQ[input, expected] or \
TestCaseSpinorQLVectorQ[input, expected, messages]\
uses same value expected for both tests."


TestCaseSpinorQLVectorQTrue::usage =
"\
TestCaseSpinorQLVectorQTrue[input] \
Tests that both SpinorQ[input] and LVectorQ[input] returns True and that no \
messages are generated.\

TestCaseSpinorQLVectorQTrue[input, messages] \
Tests that both SpinorQ[input] and LVectorQ[input] returns True and that \
given messages are generated."


TestCaseSpinorQLVectorQFalse::usage =
"\
TestCaseSpinorQLVectorQFalse[input] \
Tests that both SpinorQ[input] and LVectorQ[input] returns False and that no \
messages are generated.\

TestCaseSpinorQLVectorQFalse[input, messages] \
Tests that both SpinorQ[input] and LVectorQ[input] returns False and that \
given messages are generated."


TestCasePatterns::usage =
"\
TestCasePatterns[\
input, {{expected1}, {expected2}, {expectedAlternative}, {expectedNoArgs}}\
] \
Performs four tests: \
first test gets as first argument list with input and value of \"Var1\" \
option and as second argument: expected1, same with \"Var2\" option and \
expected2. Than as second element of list alternative of values of \"Var1\" \
and \"Var2\" options is given and as second argument expectedAlternative is \
given. Lastly list with only input is given as first argument and \
expectedNoArgs as second argument."


TestCasePatternsDefault::usage =
"\
TestCasePatternsDefault[input] \
Performs four tests with first argument being list with input as first \
element and second element: \
value of \"Var1\" option, \
value of \"Var2\" option, \
alternative of values of \"Var1\" and \"Var2\" options, \
no second element."


TestCaseRules::usage =
"\
TestCaseRules[\
input, {\
{expected1}, \
{expected2}, \
{expectedAlternative}, \
{expectedBlankPatt}, \
{expectedRuleDelayed}, \
{expectedList}
}] \
Performs seven tests with first argument being list with input as first \
element and second element: \

value of \"Rule1\" option (compared with expected1), \

value of \"Rule2\" option (compared with expected2), \

alternative of first arguments of values of \"Rule1\" and \"Rule2\" options \
ruling to second argument of value of \"Rule1\" option (compared with \
expectedAlternative), \

blank pattern ruling to second argument of value of \"Rule1\" option \
(compared with expectedBlankPatt), \

said alternative delayed ruling to function accepting matched pattern \
(compared with expectedRuleDelayed), \

list containing values of \"Rule1\" and \"Rule2\" options (compared with \
expectedList), \

list containing two lists first contains value of \"Rule1\" and second of \
\"Rule2\" option (compared with {expected1, expected2})."


TestCaseRulesDefault::usage =
"\
TestCaseRulesDefault[input] \
Performs seven tests with first argument being list with input as first \
element and second element: \

value of \"Rule1\" option, \

value of \"Rule2\" option, \

alternative of first arguments of values of \"Rule1\" and \"Rule2\" options \
ruling to second argument of value of \"Rule1\" option, \

blank pattern ruling to second argument of value of \"Rule1\" option, \

said alternative delayed ruling to function accepting matched pattern, \

list containing values of \"Rule1\" and \"Rule2\" options, \

list containing two lists first contains value of \"Rule1\" and second of \
\"Rule2\" option."


TestCaseRulesDouble::usage =
"\
TestCaseRulesDouble[\
input, {\
{expectedXSame}, {expectedXDifferent}, \
{expectedYSame}, {expectedYDifferent}, \
{expectedAlternativeSame}, {expectedAlternativeDifferent}, \
{expectedBlankPattSame}, {expectedBlankPattDifferent}, \
{expectedRuleDelayedSame}, {expectedRuleDelayedDifferent}, \
{expectedListSame}, {expectedListDifferent} \
}] \
Combines two TestCaseRules.
First is evaluated with odd elements of given list, \"Rule1\" option set to: \
OptionValue[\"x\"] -> OptionValue[\"r1\"] and \"Rule2\" option to: \
OptionValue[\"y\"] -> OptionValue[\"r2\"].
Second is evaluated with even elements of given list, \"Rule1\" option set \
to: \
OptionValue[\"x\"] -> {OptionValue[\"b1\"], OptionValue[\"a1\"]} \
and \"Rule2\" option to: \
OptionValue[\"y\"] -> {OptionValue[\"b2\"], OptionValue[\"a2\"]}."


TestCasePatternsRules::usage =
"\
TestCasePatternsRules[\
input, {\
{expected1}, {expected1Rule}, \
{expected2}, {expected2Rule}, \
{expectedAlternative}, {expectedAlternativeRule}, \
{expectedNoArgs}, {expectedBlankRule}, \
{expectedRuleDelayed}, \
{expectedListMixed}, {expectedListRules} \
}] \
Combines TestCasePatterns with TestCaseRules and performs additional test \
with list containing non-rule pattern and a rule. Accepts same options as \
TestCaseRules, for TestCasePatterns \"Vari\" options are set to first \
elements of corresponding \"Rulei\" options."


TestCasePatternsRulesDefault::usage =
"\
TestCasePatternsRulesDefault[input] \
Combines TestCasePatternsDefault with TestCaseRulesDefault and performs \
additional test with list containing non-rule pattern and a rule. Accepts \
same options as TestCaseRulesDefault, for TestCasePatternsDefault \"Vari\" \
options are set to first elements of corresponding \"Rulei\" options."


TestCaseDeclare::usage =
"\
TestCaseDeclare[\
{arg1, arg2, ...}, \
{symb1 -> expectedQBefore1, symb2 -> expectedQBefore2, ...}, \
{symb1 -> expectedQAfter1, symb2 -> expectedQAfter2, ...}\
] \
Tests that for each symbi value of \"QFunction\" option with symbi as \
argument evaluates to expectedQBeforei. Then evaluates value of \
\"DeclareFunction\" option with arguments arg1, arg2, ... and repeats tests \
comparing results to expectedQAfteri.\

TestCaseDeclare[{arg1, arg2, ...}] \
Assumes all expectedQBefore1i to be False and all expectedQAfteri to be True."


TestCaseUndeclare::usage =
"\
TestCaseUndeclare[\
{arg1, arg2, ...}, \
{symb1 -> expectedQBefore1, symb2 -> expectedQBefore2, ...}, \
{symb1 -> expectedQAfter1, symb2 -> expectedQAfter2, ...}\
] \
Tests that for each symbi value of \"QFunction\" option with symbi as \
argument evaluates to expectedQBeforei. Then evaluates value of \
\"UndeclareFunction\" option with arguments arg1, arg2, ... and repeats tests \
comparing results to expectedQAfteri.\

TestCaseUndeclare[{arg1, arg2, ...}] \
Assumes all expectedQBefore1i to be True and all expectedQAfteri to be False."


TestCaseSpxyLeftInsideRight::usage =
"\
TestCaseSpxyLeftInsideRight[\
input, \
{\
resultLeft, \
resultInside, \
resultRight\
, \
resultLeftInside, \
resultLeftRight, \
resultInsideRight\
, \
resultLeftInsideRight, \
resultLeftInsideDoubleRight\
}\
] \
Five options are required: \
\"ReplaceBy\", \"Left\", \"Inside\", \"Inside2\", \"Right\". \
Performs eight tests. Compares resultLeft with input evaluated with value of \
\"Left\" option replaced by value of \"ReplaceBy\" option. Analogously for \
resultInside and \"Inside\" option, resultRight and \"Right\". Other result...
arguments are compared with input with values of respective pairs of options \
replaced, triple of options and four options."


(* ::Section:: *)
(*Private*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


(* ::Subsection:: *)
(*Imports*)


Needs["MUnit`"];


Needs["MUnitExtras`"];
Needs["MUnitExtras`Package`"];


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)
Needs["OptionsUtilities`"] (* DelegateOptions, PrependToOptions *)


(* ::Subsection:: *)
(*TestCaseSpinorQLVectorQ*)


AssignTestFeatures[TestCaseSpinorQLVectorQ]


TestCaseSpinorQLVectorQ[
	input_,
	expected_,
	Shortest[messages_:{}],
	opts:OptionsPattern[]
] :=
	TestCaseSpinorQLVectorQ[input, {expected, expected}, messages, opts];


TestCaseSpinorQLVectorQ[
	input_,
	{spinorQExpected_, lVectorQExpected_},
	Shortest[messages_:{}],
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseSpinorQLVectorQ]}
		,
		Test[
			input,
			spinorQExpected,
			messages,
			InputWrapper -> SpinorQ,
			TestFailureMessage -> "SpinorQ"
		];
		Test[
			input,
			lVectorQExpected,
			messages,
			InputWrapper -> LVectorQ,
			TestFailureMessage -> "LVectorQ"
		];
		,
		"CommonOptionsFor" -> {Test}
	]


(* ::Subsection:: *)
(*TestCaseSpinorQLVectorQTrue*)


AddTestDefaultFunction[
	TestCaseSpinorQLVectorQTrue,
	True,
	TestCaseSpinorQLVectorQ
]


(* ::Subsection:: *)
(*TestCaseSpinorQLVectorQFalse*)


AddTestDefaultFunction[
	TestCaseSpinorQLVectorQFalse,
	False,
	TestCaseSpinorQLVectorQ
]


(* ::Subsection:: *)
(*TestCasePatterns*)


AssignTestFeatures[TestCasePatterns]

PrependToOptions[TestCasePatterns,
	Test -> Test
	,
	"Var1" -> RequiredOptionIsNotSet,
	"Var2" -> RequiredOptionIsNotSet
]


TestCasePatterns[
	input_,
	{
		{expected1___},
		{expected2___},
		{expectedAlternative___},
		{expectedNoArgs___}
	},
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCasePatterns]}
		,
		CheckRequiredOptions[TestCasePatterns, {opts}, {"Var1", "Var2"}];
	
		With[
			{
				testFunction = OptionValue[Test],
				var1 = OptionValue["Var1"],
				var2 = OptionValue["Var2"],
				alternative = OptionValue["Var1"] | OptionValue["Var2"]
			}
			,
			testFunction[
				{input, var1},
				expected1,
				TestFailureMessage -> "single variable: " <> ToString[var1]
			];
			testFunction[
				{input, var2},
				expected2,
				TestFailureMessage -> "single variable: " <> ToString[var2]
			];
			testFunction[
				{input, alternative},
				expectedAlternative,
				TestFailureMessage -> "alternative: " <> ToString[alternative]
			];
			testFunction[
				{input},
				expectedNoArgs,
				TestFailureMessage -> "no args"
			];
		]
		,
		"CommonOptionsFor" -> {OptionValue[Test]}
	]


(* ::Subsection:: *)
(*TestCasePatternsDefault*)


AssignTestFeatures[TestCasePatternsDefault,
	{TestCasePatterns, TestSubexpression}
]

SetOptions[TestCasePatternsDefault, Test -> TestDefault]


TestCasePatternsDefault[input_, opts:OptionsPattern[]] :=
	With[
		{
			delegatedOpts =
				DelegateOptions[
					opts, TestCasePatternsDefault, TestCasePatterns
				]
		}
		,
		TestCasePatterns[
			input,
			{{}, {}, {}, {}},
			delegatedOpts
		]
	]


(* ::Subsection:: *)
(*MapDeepestLists (Private function)*)


MapDeepestLists[f_, input_List] := MapDeepestLists[f, #]& /@ input


MapDeepestLists[f_, input_] := f[input]


(* ::Subsection:: *)
(*TestCaseRules*)


AssignTestFeatures[TestCaseRules]

PrependToOptions[TestCaseRules,
	Test -> Test,
	"RuleDelayedFunction" -> Automatic
	,
	"Rule1" -> RequiredOptionIsNotSet,
	"Rule2" -> RequiredOptionIsNotSet
]


TestCaseRules[
	input_,
	{
		{Longest[expected1:Repeated[_, {0, 1}]], expected1Rest___},
		{Longest[expected2:Repeated[_, {0, 1}]], expected2Rest___},
		{expectedAlternative___},
		{expectedBlankPatt___},
		{expectedRuleDelayed___},
		{expectedList___}
	},
	Shortest[messages_:{}],
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseRules]}
		,
		CheckRequiredOptions[TestCaseRules, {opts}, {"Rule1", "Rule2"}];
		CheckOptionsValues[
			TestCaseRules,
			{opts},
			{"Rule1" -> Rule[_, _], "Rule2" -> Rule[_, _]}
		];
	
		With[
			{
				x = First[OptionValue["Rule1"]],
				y = First[OptionValue["Rule2"]]
				,
				r1 = Last[OptionValue["Rule1"]]
				,
				rule1 = OptionValue["Rule1"],
				rule2 = OptionValue["Rule2"]
				,
				ruleDelayedFunction =
					If[OptionValue["RuleDelayedFunction"] === Automatic,
						Function[
							w,
							MapDeepestLists[#[w]&, Last[OptionValue["Rule1"]]]
						]
					(* else *),
						OptionValue["RuleDelayedFunction"]
					]
				,
				testFunction = OptionValue[Test]
			}
			,
			testFunction[
				{input, rule1},
				expected1, expected1Rest,
				TestFailureMessage -> "simple rule: " <> ToString[rule1]
			];
			testFunction[
				{input, rule2},
				expected2, expected2Rest,
				TestFailureMessage -> "simple rule: " <> ToString[rule2]
			];
			testFunction[
				{input, x | y -> r1},
				expectedAlternative,
				TestFailureMessage ->
					"rule with alternative: " <> ToString[x | y -> r1]
			];
			testFunction[
				{input, _ -> r1},
				expectedBlankPatt,
				TestFailureMessage ->
					"rule with blank pattern: " <> ToString[_ -> r1]
			];
			
			
			testFunction[
				{input, w:(x | y) :> Evaluate[ruleDelayedFunction[w]]},
				expectedRuleDelayed,
				TestFailureMessage ->
					"rule delayed: w:(" <> ToString[x | y] <> ") :> " <>
						ToString[ruleDelayedFunction["w"]]
			];
			
			testFunction[
				{input, {rule1, rule2}},
				expectedList,
				TestFailureMessage ->
					"list of rules: " <> ToString[{rule1, rule2}]
			];
			
			testFunction[
				{input, {{rule1}, {rule2}}},
				{expected1, expected2},
				messages,
				TestFailureMessage ->
					"list of lists: " <> ToString[{{rule1}, {rule2}}]
			];
		];
		,
		"CommonOptionsFor" -> {OptionValue[Test]}
	]


(* ::Subsection:: *)
(*TestCaseRulesDefault*)


AssignTestFeatures[TestCaseRulesDefault, TestCaseRules]

SetOptions[TestCaseRulesDefault, Test -> TestDefault]


TestCaseRulesDefault[input_, Shortest[messages_:{}], opts:OptionsPattern[]] :=
	With[
		{
			delegatedOpts =
				DelegateOptions[
					opts, TestCaseRulesDefault, TestCaseRules
				]
		}
		,
		TestCaseRules[
			input,
			{{}, {}, {}, {}, {}, {}},
			messages,
			delegatedOpts
		]
	]


(* ::Subsection:: *)
(*TestCaseRulesDouble*)


AssignTestFeatures[TestCaseRulesDouble]

PrependToOptions[TestCaseRulesDouble,
	Test -> Test,
	"RuleDelayedFunctionSame" -> Automatic,
	"RuleDelayedFunctionDifferent" -> Automatic
	,
	(# -> RequiredOptionIsNotSet)& /@
		{"x", "y", "r1", "r2", "b1", "b2", "a1", "a2"}
]


TestCaseRulesDouble[
	input_,
	{
		expectedXSame_List, expectedXDifferent_List,
		expectedYSame_List, expectedYDifferent_List,
		expectedAlternativeSame_List, expectedAlternativeDifferent_List,
		expectedBlankPattSame_List, expectedBlankPattDifferent_List,
		expectedRuleDelayedSame_List, expectedRuleDelayedDifferent_List,
		expectedListSame_List, expectedListDifferent_List
	},
	{messagesSame_, messagesDifferent_},
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseRulesDouble]}
		,
		CheckRequiredOptions[
			TestCaseRulesDouble,
			{opts},
			{"x", "y", "r1", "r2", "b1", "b2", "a1", "a2"}
		];
		
		TestCaseRules[
			input,
			{
				expectedXSame,
				expectedYSame,
				expectedAlternativeSame,
				expectedBlankPattSame,
				expectedRuleDelayedSame,
				expectedListSame
			},
			messagesSame,
			"Rule1" -> (OptionValue["x"] -> OptionValue["r1"]),
			"Rule2" -> (OptionValue["y"] -> OptionValue["r2"]),
			"RuleDelayedFunction" -> OptionValue["RuleDelayedFunctionSame"]
		];
		TestCaseRules[
			input,
			{
				expectedXDifferent,
				expectedYDifferent,
				expectedAlternativeDifferent,
				expectedBlankPattDifferent,
				expectedRuleDelayedDifferent,
				expectedListDifferent
			},
			messagesDifferent,
			"Rule1" ->
				(OptionValue["x"] -> {OptionValue["b1"], OptionValue["a1"]}),
			"Rule2" ->
				(OptionValue["y"] -> {OptionValue["b2"], OptionValue["a2"]}),
			"RuleDelayedFunction" ->
				OptionValue["RuleDelayedFunctionDifferent"]
		];
		,
		"CommonOptionsFor" -> {TestCaseRules}
	]
	
TestCaseRulesDouble[
	input_,
	expected:{Repeated[_List, {12}]},
	opts:OptionsPattern[]
] :=
	TestCaseRulesDouble[input, expected, {{}, {}}, opts]


(* ::Subsection:: *)
(*TestCasePatternsRules*)


AssignTestFeatures[TestCasePatternsRules, TestCaseRules];


TestCasePatternsRules[
	input_,
	{
		result1_List, result1Rule_List,
		result2_List, result2Rule_List,
		resultAlternative_List, resultAlternativeRule_List,
		resultNoArgs_List, resultBlankRule_List,
		resultRuleDelayed_List,
		{resultListMixed___}, resultListRules_List
	},
	Shortest[messages_:{}],
	opts:OptionsPattern[]
] :=
	With[
		{
			x = First[OptionValue["Rule1"]],
			y = First[OptionValue["Rule2"]]
			,
			test = OptionValue[Test]
		}
		,
		
		TestCaseEnvironment[
			{opts, Options[TestCasePatternsRules]}
			,
			
			TestCasePatterns[
				input,
				{
					result1,
					result2,
					resultAlternative,
					resultNoArgs
				},
				"Var1" -> x,
				"Var2" -> y
			];
	
			TestCaseRules[
				input,
				{
					result1Rule,
					result2Rule,
					resultAlternativeRule,
					resultBlankRule,
					resultRuleDelayed,
					resultListRules
				},
				messages
			];
			
			test[
				{input, {x, OptionValue["Rule2"]}},
				resultListMixed,
				TestFailureMessage ->
					"list mixed: " <> ToString[{x, OptionValue["Rule2"]}]
			];
			
			,
			"CommonOptionsFor" -> {TestCasePatterns, TestCaseRules, test}
		];
	];


(* ::Subsection:: *)
(*TestCasePatternsRulesDefault*)


AssignTestFeatures[TestCasePatternsRulesDefault, TestCasePatternsRules]

SetOptions[TestCasePatternsRulesDefault, Test -> TestDefault]


TestCasePatternsRulesDefault[input_, opts:OptionsPattern[]] :=
	With[
		{
			delegatedOpts =
				DelegateOptions[
					opts, TestCasePatternsRulesDefault, TestCasePatternsRules
				]
		}
		,
		TestCasePatternsRules[
			input,
			{
				{}, {},
				{}, {},
				{}, {},
				{}, {},
				{},
				{}, {}
			},
			delegatedOpts
		]
	]


(* ::Subsection:: *)
(*TestCaseDeclareUndeclare (Private function)*)


AssignTestFeatures[TestCaseDeclareUndeclare]

PrependToOptions[TestCaseDeclareUndeclare,
	"QFunction" -> RequiredOptionIsNotSet,
	"(Un)DeclareFunction" -> RequiredOptionIsNotSet
]


TestCaseDeclareUndeclare[
	{declareArgs___},
	testBeforeArgs:({__Rule} | True | False),
	testAfterArgs:({__Rule} | True | False),
	additionalDeclared_List:{},
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseDeclareUndeclare]}
		,
		CheckRequiredOptions[
			TestCaseDeclareUndeclare,
			{opts},
			{"QFunction", "(Un)DeclareFunction"}
		];
		
		With[
			{
				testBeforeArgsList =
					If[MatchQ[testBeforeArgs, True | False],
						(# -> testBeforeArgs &) /@ {declareArgs}
					(* else *),
						testBeforeArgs
					],
				testAfterArgsList = 
					If[MatchQ[testAfterArgs, True | False],
						(# -> testAfterArgs &) /@ {declareArgs}
					(* else *),
						testAfterArgs
					]
				,
				qFunction = OptionValue["QFunction"],
				qFunctionDownValues =
					DownValues[Evaluate[OptionValue["QFunction"]]],
				undeclareFunc = OptionValue["(Un)DeclareFunction"]
			}
			,
			Block[
				{
					(*
						We want all changes done to given qFunction to be
						reversed after tests are performed, so we use qFunction
						as block variable.
					*)
					qFunction
				}
				,
				DownValues[qFunction] = qFunctionDownValues;
				(qFunction[#] = True)& /@ additionalDeclared;
				
				Test[
					qFunction[#1],
					#2,
					TestFailureMessage ->
						"(" <> ToString[#1] <> ": " <> ToString[#2] <>
							") before declaration"
				]& @@@
					testBeforeArgsList;
				
				Test[
					undeclareFunc[declareArgs],
					Null,
					TestFailureMessage -> "(un)declare"
				];
				
				Test[
					qFunction[#1],
					#2,
					TestFailureMessage ->
						"(" <> ToString[#1] <> ": " <> ToString[#2] <>
							") after declaration"
				] & @@@
					testAfterArgsList;
			]
		]
		,
		"CommonOptionsFor" -> {Test}
	]


(* ::Subsection:: *)
(*TestCaseDeclare*)


AssignTestFeatures[TestCaseDeclare]

PrependToOptions[TestCaseDeclare,
	"QFunction" -> RequiredOptionIsNotSet,
	"DeclareFunction" -> RequiredOptionIsNotSet
]


TestCaseDeclare[
	declareArgsList_List,
	testBeforeArgs:({__Rule} | False):False,
	testAfterArgs:({__Rule} | True):True,
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseDeclare]}
		,
		CheckRequiredOptions[
			TestCaseDeclare,
			{opts},
			{"QFunction", "DeclareFunction"}
		];
		
		TestCaseDeclareUndeclare[
			declareArgsList,
			testBeforeArgs,
			testAfterArgs,
			"(Un)DeclareFunction" -> OptionValue["DeclareFunction"]
		]
		,
		"CommonOptionsFor" -> {TestCaseDeclareUndeclare}
	]


(* ::Subsection:: *)
(*TestCaseUndeclare*)


AssignTestFeatures[TestCaseUndeclare]

PrependToOptions[TestCaseUndeclare,
	"QFunction" -> RequiredOptionIsNotSet,
	"UndeclareFunction" -> RequiredOptionIsNotSet
]


TestCaseUndeclare[
	declareArgsList_List,
	testBeforeArgs:({__Rule} | True):True,
	testAfterArgs:({__Rule} | False):False,
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseUndeclare]}
		,
		CheckRequiredOptions[
			TestCaseUndeclare,
			{opts},
			{"QFunction", "UndeclareFunction"}
		];
		
		TestCaseDeclareUndeclare[
			declareArgsList,
			testBeforeArgs,
			testAfterArgs,
			declareArgsList,
			"(Un)DeclareFunction" -> OptionValue["UndeclareFunction"]
		]
		,
		"CommonOptionsFor" -> {TestCaseDeclareUndeclare}
	]


(* ::Subsection:: *)
(*TestCaseSpxyLeftInsideRight*)


AssignTestFeatures[TestCaseSpxyLeftInsideRight]

PrependToOptions[TestCaseSpxyLeftInsideRight,
	(# -> RequiredOptionIsNotSet)& /@
		{"ReplaceBy", "Left", "Inside", "Inside2", "Right"}
]


TestCaseSpxyLeftInsideRight[
	input_,
	{
		{expectedLeft__},
		{expectedInside__},
		{expectedRight__}
		,
		{expectedLeftInside__},
		{expectedLeftRight__},
		{expectedInsideRight__}
		,
		{expectedLeftInsideRight__},
		{expectedLeftInsideDoubleRight__}
	},
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseSpxyLeftInsideRight]}
		,
		CheckRequiredOptions[
			TestCaseSpxyLeftInsideRight,
			{opts},
			{"ReplaceBy", "Left", "Inside", "Inside2", "Right"}
		];
		
		With[
			{
				replaceBy = OptionValue["ReplaceBy"],
				left = OptionValue["Left"],
				inside = OptionValue["Inside"],
				inside2 = OptionValue["Inside2"],
				right = OptionValue["Right"]
			}
			,
			With[
				{replacedInput = input /. #2 -> replaceBy}
				,
				Test[
					{replacedInput},
					##3,
					TestFailureMessage -> #1
				];
			]& @@@ {
				{"left", left, expectedLeft},
				{"inside", inside, expectedInside},
				{"right", right, expectedRight}
				,
				{"left and inside", left | inside, expectedLeftInside},
				{"left and right", left | right, expectedLeftRight},
				{"inside and right", inside | right, expectedInsideRight}
				,
				{
					"left, inside and right",
					left | inside | right,
					expectedLeftInsideRight
				},
				{
					"left, inside double and right",
					left | inside | inside2 | right,
					expectedLeftInsideDoubleRight
				}
			};
		]
		,
		"CommonOptionsFor" -> {Test}
	]


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
