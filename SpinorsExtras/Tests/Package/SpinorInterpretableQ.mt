(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Package`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a},
	"LVectors" -> {P},
	"SMatrices" -> {sm}
];


SetOptions[{TestTrue, TestFalse}, ApplyToInput -> SpinorInterpretableQ];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestTrue[{1}, TestID -> "Integer"];
TestTrue[{a}, TestID -> "Spinor"];

TestFalse[{P}, TestID -> "LVector"];
TestFalse[{sm}, TestID -> "SMatrix"];
TestFalse[{x}, TestID -> "arbitrary"];

TestUnchanged[{1, a}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
