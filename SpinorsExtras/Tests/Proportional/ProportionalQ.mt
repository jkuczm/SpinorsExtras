(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Proportional`"
	,
	"MUnitExtras`Package`",
	"OptionsUtilities`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c, d, e, f},
	"LVectors" -> {P}
];


AssignTestFeatures[TestProportionalQ];
PrependToOptions[TestProportionalQ,
	"ProportionalListFor" -> All
];

TestProportionalQ[
	input_, result_,
	propBList_List, propAList_List,
	opts:OptionsPattern[]
] :=
	Block[
		{
			SpinorsExtras`Proportional`Private`$ProportionalBSpinorList =
				propBList,
			SpinorsExtras`Proportional`Private`$ProportionalASpinorList =
				propAList
		}
		,
		With[
			{delegatedOpts = DelegateOptions[opts, TestProportionalQ, Test]}
			,
			Test[input, result, delegatedOpts];
		];
	];

TestProportionalQ[input_, result_, propList_List, opts:OptionsPattern[]] :=
	CatchTestError[
		{input, result, propList, opts}
		,
		With[
			{proportionalListFor = OptionValue["ProportionalListFor"]}
			,
			Switch[proportionalListFor,
				All,
					TestProportionalQ[input, result, propList, propList, opts]
				,
				"B",
					TestProportionalQ[input, result, propList, {}, opts]
				,
				"A",
					TestProportionalQ[input, result, {}, propList, opts]
				,
				_,
					ThrowTestError @ StringForm[
						General::wrongOptionValue,
						"ProportionalListFor" -> proportionalListFor,
						All | "B" | "A"
					]
			]
		]
	];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Sequence of arguments*)


TestCaseRepeated[
	SetOptions[TestProportionalQ,
		ApplyToInput -> propQFunction,
		"ProportionalListFor" -> propListFor
	];
	
	TestProportionalQ[
		{a},
		HoldPattern[propQFunction[a]],
		{},
		TestID -> "one arg",
		EquivalenceFunction -> MatchQ
	];
	
	TestProportionalQ[
		{1, 1},
		True,
		{},
		TestID -> "two same integers"
	];
	TestProportionalQ[
		{1, Sp[1]},
		True,
		{},
		TestID -> "two args interpretable as same spinors"
	];
	TestProportionalQ[
		{a, a},
		True,
		{},
		TestID -> "two same spinors"
	];
	TestProportionalQ[
		{P, P},
		HoldPattern[propQFunction[P, P]],
		{},
		TestID -> "two same LVectors",
		EquivalenceFunction -> MatchQ
	];
	TestProportionalQ[
		{x, x},
		HoldPattern[propQFunction[x, x]],
		{},
		TestID -> "two same non LVectors",
		EquivalenceFunction -> MatchQ
	];
	
	TestProportionalQ[
		{1, 1, 1},
		True,
		{},
		TestID -> "three same integers"
	];
	TestProportionalQ[
		{1, Sp[1], 1},
		True,
		{},
		TestID -> "three args interpretable as same spinors"
	];
	TestProportionalQ[
		{a, a, a},
		True,
		{},
		TestID -> "three same spinors"
	];
	TestProportionalQ[
		{P, P, P},
		HoldPattern[propQFunction[P, P, P]],
		{},
		TestID -> "three same LVectors",
		EquivalenceFunction -> MatchQ
	];
	TestProportionalQ[
		{x, x, x},
		HoldPattern[propQFunction[x, x, x]],
		{},
		TestID -> "three same non LVectors",
		EquivalenceFunction -> MatchQ
	];
	
	TestProportionalQ[
		{a, b},
		True,
		{a|b},
		TestID -> "two proportional spinors, only pair"
	];
	TestProportionalQ[
		{b, a},
		True,
		{a|b},
		TestID -> "two proportional spinors, only pair different order"
	];
	TestProportionalQ[
		{a, b},
		True,
		{a|b, d|e},
		TestID -> "two proportional spinors, one of two pairs"
	];
	TestProportionalQ[
		{a, b},
		True,
		{a|b|e},
		TestID -> "two proportional spinors, from one triple"
	];
	TestProportionalQ[
		{a, b},
		False,
		{a|d, b|e},
		TestID ->
			"two non-proportional spinors, both existing in separate pairs"
	];
	
	TestProportionalQ[
		{Sp[1], Sp[3]},
		True,
		{Sp[_?OddQ]},
		TestID -> "two proportional spinors, matching one pattern"
	];
	TestProportionalQ[
		{Sp[1], Sp[2]},
		False,
		{Sp[_?OddQ]},
		TestID -> "two non-proportional spinors, one matching pattern"
	];
	
	TestProportionalQ[
		{a, b, c},
		True, 
		{a|b|c},
		TestID -> "three proportional spinors, only triple"
	];
	TestProportionalQ[
		{a, b, c},
		True,
		{d|e, a|b|c},
		TestID -> "three proportional spinors, two sets"
	];
	TestProportionalQ[
		{a, b, c},
		True,
		{a|b|c|e},
		TestID -> "three proportional spinors, only quadruple"
	];
	TestProportionalQ[
		{a, b, c},
		False,
		{a|b, c|d, e|f},
		TestID -> "three spinors from which only two are proportional spinors"
	];
	
	TestProportionalQ[
		{a, 1, P},
		HoldPattern[propQFunction[a, 1, P]],
		{a|P|Sp[1]},
		TestID -> "three proportional args: spinor, integer, lVector",
		EquivalenceFunction -> MatchQ
	];
	TestProportionalQ[
		{a, x, b},
		HoldPattern[propQFunction[a, x, b]],
		{a|b},
		TestID -> "three args: one non lVector",
		EquivalenceFunction -> MatchQ
	];
	TestProportionalQ[
		{Sp[1], Sp[2], a},
		True,
		{_Sp|a},
		TestID ->
			"three proportional spinors, two matching blank pattern with head"
	];
	TestProportionalQ[
		{Sp[1], Sp[2], b},
		False,
		{_Sp|a},
		TestID ->
			"three proportional spinors, two matching blank pattern with head"
	];
	
	
	(* List argument *)
	TestProportionalQ[
		{{a}},
		HoldPattern[propQFunction[a]],
		{},
		TestID -> "List argument: one arg",
		EquivalenceFunction -> MatchQ
	];
	
	TestProportionalQ[
		{{a, b}},
		True,
		{a|b},
		TestID -> "List argument: two proportional spinors, only pair"
	];
	TestProportionalQ[
		{{a, b}},
		False,
		{},
		TestID -> "List argument: two non-proportional spinors"
	];
	
	TestProportionalQ[
		{a, b, c},
		True, 
		{a|b|c},
		TestID -> "List argument: three proportional spinors, only triple"
	];
	TestProportionalQ[
		{a, b, c},
		False, 
		{a|b},
		TestID -> "List argument: three spinors, only two proportional"
	];
	,
	TestID -> "(B/A)SpinorProportionalQ",
	RepeatFor -> {
		{propQFunction -> BSpinorProportionalQ, propListFor -> "B"},
		{propQFunction -> ASpinorProportionalQ, propListFor -> "A"},
		{propQFunction -> LVectorProportionalQ, propListFor -> All}
	}
];


SetOptions[TestProportionalQ,
	ApplyToInput -> LVectorProportionalQ,
	"ProportionalListFor" -> All
];
TestProportionalQ[
	{a, b},
	False,
	{a|b},
	{a|c},
	TestID -> "two args: prop B, non-prop A"
];
TestProportionalQ[
	{b, a, c},
	False,
	{a|b, c|d},
	{a|b|c, d|e},
	TestID -> "three args: non-prop B, prop A"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
