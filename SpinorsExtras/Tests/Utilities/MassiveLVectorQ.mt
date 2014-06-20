(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Utilities`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a},
	"LVectors" -> {P},
	"SMatrices" -> {M}
];

	
SetOptions[{TestUnchanged, TestTrue, TestFalse},
	ApplyToInput -> MassiveLVectorQ
];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestFalse[{1}, TestID -> "Integer"];
TestFalse[{a}, TestID -> "Spinor"];
TestTrue[{P}, TestID -> "LVector"];
TestFalse[{M}, TestID -> "SMatrix"];

TestUnchanged[{1, a}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
