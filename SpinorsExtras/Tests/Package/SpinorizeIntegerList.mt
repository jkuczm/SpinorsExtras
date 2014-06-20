(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Package`",
	"MUnitExtras`Package`"
}];
SetUpSpinorsTestEnvironment["Spinors" -> {a}, "LVectors" -> {P}];


AssignTestFeatures[TestCase];

TestCase[input_, result_, opts:OptionsPattern[]] :=
	TestCaseEnvironment[
		{opts, Options[TestCase]}
		,
		Test[input, result];
		Test[{input}, result];
	];


SetOptions[TestCase, ApplyToInput -> SpinorizeIntegerList];


(* ::Section:: *)
(*Tests*)


TestCase[{}, {}, TestID -> "list: empty"];

TestCase[{x}, {x}, TestID -> "one non-lvector"];
TestCase[{P}, {P}, TestID -> "one lvector"];
TestCase[{a}, {a}, TestID -> "one spinor"];
TestCase[{1}, {Sp[1]}, TestID -> "one integer"];

TestCase[{1, a}, {Sp[1], a}, TestID -> "two args"];

TestCase[{x, 1, a, P}, {x, Sp[1], a, P}, TestID -> "four args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
