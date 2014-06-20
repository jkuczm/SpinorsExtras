(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Package`"}];
SetUpSpinorsTestEnvironment["Spinors" -> {a}, "LVectors" -> {P}];


SetOptions[
	{Test, TestSubexpression, TestUnchanged},
	ApplyToInput -> SpinorizeInteger
];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestSubexpression[{x}, TestID -> "one non-lvector"];
TestSubexpression[{P}, TestID -> "one lvector"];
TestSubexpression[{a}, TestID -> "one spinor"];
Test[{1}, Sp[1], TestID -> "one integer"];

TestUnchanged[{1, a}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
