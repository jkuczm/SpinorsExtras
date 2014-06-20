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
	"Spinors" -> {a, b, c, d, e},
	"LVectors" -> {P}
];


AssignTestFeatures[TestCaseDeclareCheckPropLists];
PrependToOptions[TestCaseDeclareCheckPropLists,
	"AssumeNotChangedProportionalList" -> None
];
TestCaseDeclareCheckPropLists[
	input_List,
	{declarationResults__},
	initialPropBList_List -> expectedPropBList_List,
	initialPropAList_List -> expectedPropAList_List,
	opts:OptionsPattern[]
] :=
	TestCaseEnvironment[
		{opts, Options[TestCaseDeclareCheckPropLists]}
		,
		Module[
			{
				notChangedList =
					OptionValue["AssumeNotChangedProportionalList"]
				,
				reallyExpectedPropBList = expectedPropBList,
				reallyExpectedPropAList = expectedPropAList
			}
			,
			Switch[notChangedList,
				None,
					Null
				,
				"B",
					reallyExpectedPropBList = initialPropBList
				,
				"A",
					reallyExpectedPropAList = initialPropAList
				,
				_,
					ThrowTestError @ StringForm[
						General::wrongOptionValue,
						"AssumeNotChangedProportionalList" -> notChangedList,
						None | "B" | "A"
					];
			];
		
			Block[
				{
					SpinorsExtras`Proportional`Private`$ProportionalBSpinorList
						= initialPropBList
					,
					SpinorsExtras`Proportional`Private`$ProportionalASpinorList
						= initialPropAList
				}
				,
				Test[
					input,
					declarationResults,
					TestFailureMessage -> "declaration"
				];
				
				Test[
					SpinorsExtras`Proportional`Private`$ProportionalBSpinorList,
					reallyExpectedPropBList,
					ApplyToInput -> None,
					TestFailureMessage ->
						"$ProportionalBSpinorList same as expected"
				];
				Test[
					SpinorsExtras`Proportional`Private`$ProportionalASpinorList,
					reallyExpectedPropAList,
					ApplyToInput -> None,
					TestFailureMessage ->
						"$ProportionalASpinorList same as expected"
				];
			];
		];
		,
		"CommonOptionsFor" -> {Test}
	];

TestCaseDeclareCheckPropLists[
	{declareArgs___},
	initialPropBList_List -> expectedPropBList_List,
	initialPropAList_List -> expectedPropAList_List,
	opts:OptionsPattern[]
] :=
	TestCaseDeclareCheckPropLists[
		{declareArgs},
		{Null},
		initialPropBList -> expectedPropBList,
		initialPropAList -> expectedPropAList,
		opts
	];

TestCaseDeclareCheckPropLists[
	{declareArgs___},
	{declarationResults__},
	initialPropList_List -> expectedPropList_List,
	opts:OptionsPattern[]
] :=
	TestCaseDeclareCheckPropLists[
		{declareArgs},
		{declarationResults},
		initialPropList -> expectedPropList,
		initialPropList -> expectedPropList,
		opts
	];

TestCaseDeclareCheckPropLists[
	{declareArgs___},
	initialPropList_List -> expectedPropList_List,
	opts:OptionsPattern[]
] :=
	TestCaseDeclareCheckPropLists[
		{declareArgs},
		{Null},
		initialPropList -> expectedPropList,
		initialPropList -> expectedPropList,
		opts
	];


(* ::Section:: *)
(*Tests*)


TestCaseRepeated[
	SetOptions[TestCaseDeclareCheckPropLists,
		ApplyToInput -> declareFunction,
		"AssumeNotChangedProportionalList" -> notChangedProp
	];
	
	TestCaseDeclareCheckPropLists[
		{a},
		{
			$Failed
			,
			{HoldForm @ Message[
				declareFunction::notEnoughSpinors,
				declareFunction,
				{a},
				2
			]}
		},
		{} -> {},
		TestID -> "one arg"
	];
	TestCaseDeclareCheckPropLists[
		{a},
		{
			$Failed
			,
			{HoldForm @ Message[
				declareFunction::notEnoughSpinors,
				declareFunction,
				{a},
				2
			]}
		},
		{d | e} -> {d | e},
		TestID -> "one arg, existing different"
	];
	TestCaseDeclareCheckPropLists[
		{a},
		{
			$Failed,
			{HoldForm @ Message[
				declareFunction::notEnoughSpinors,
				declareFunction,
				{a},
				2
			]}
		},
		{a | e} -> {a | e},
		TestID -> "one arg, existing one same"
	];
	
	TestCaseDeclareCheckPropLists[
		{a, b},
		{} -> {a | b},
		TestID -> "two args"
	];
	TestCaseDeclareCheckPropLists[
		{a, b},
		{d | e} -> {a | b, d | e},
		TestID -> "two args, existing different"
	];
	TestCaseDeclareCheckPropLists[
		{a, b},
		{a | e} -> {a | b | e},
		TestID -> "two args, existing one same"
	];
	TestCaseDeclareCheckPropLists[
		{a, b},
		{a | b} -> {a | b},
		TestID -> "two args, existing two same in one"
	];
	TestCaseDeclareCheckPropLists[
		{a, b},
		{a | d, b | e} -> {a | b | d | e},
		TestID -> "two args, existing two same in two"
	];
	
	TestCaseDeclareCheckPropLists[
		{a, 1},
		{} -> {a | Sp[1]},
		TestID -> "two args: spinor, integer"
	];
	TestCaseDeclareCheckPropLists[
		{a, P},
		{
			$Failed,
			{HoldForm @ Message[
				declareFunction::nonSpinor,
				{{2}},
				HoldForm @ declareFunction[a, P]
			]}
		},
		{} -> {},
		TestID -> "two args: spinor, lVector"
	];
	TestCaseDeclareCheckPropLists[
		{1, Sp[1]},
		{
			$Failed
			,
			{HoldForm @ Message[
				declareFunction::notEnoughSpinors,
				declareFunction,
				{Sp[1]},
				2
			]}
		},
		{} -> {},
		TestID -> "two args interpretable as same spinors"
	];
	
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{} -> {a | b | c},
		TestID -> "three args"
	];
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{d | e} -> {d | e, a | b | c},
		TestID -> "three args, existing different"
	];
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{a | e} -> {a | b | c | e},
		TestID -> "three args, existing one same"
	];
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{a | b} -> {a | b | c},
		TestID -> "three args, existing two same in one"
	];
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{a | d, b | e} -> {a | b | c | d | e},
		TestID -> "three args, existing two same in two"
	];
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{a | b, c | d, e | f} -> {e | f, a | b | c | d},
		TestID -> "three args, existing three same in two"
	];
	TestCaseDeclareCheckPropLists[
		{a, b, c},
		{a | d, b | e, b | f} -> {a | b | c | d | e | f},
		TestID -> "three args, existing three same in three"
	];
	
	TestCaseDeclareCheckPropLists[
		{a, x, b},
		{
			$Failed,
			{HoldForm @ Message[
				declareFunction::nonSpinor,
				{{2}},
				HoldForm @ declareFunction[a, x, b]
			]}
		},
		{} -> {},
		TestID -> "three args: one non lVector"
	];
	,
	TestID -> "Declare(B/A)SpinorProportional",
	RepeatFor -> {
		{declareFunction -> DeclareBSpinorProportional, notChangedProp -> "A"},
		{declareFunction -> DeclareASpinorProportional, notChangedProp -> "B"},
		{declareFunction -> DeclareLVectorProportional, notChangedProp -> None}
	}
];


SetOptions[TestCaseDeclareCheckPropLists,
	ApplyToInput -> DeclareLVectorProportional,
	"AssumeNotChangedProportionalList" -> None
];
TestCaseDeclareCheckPropLists[
	{a, b, c},
	{a | b, c | d, e | f} -> {e | f, a | b | c | d},
	{a | b} -> {a | b | c},
	TestID -> "DeclareLVectorProportional: \
three args, B: existing three same in two, A: existing two same in one"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
