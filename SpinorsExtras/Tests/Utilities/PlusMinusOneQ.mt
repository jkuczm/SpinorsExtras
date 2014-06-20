(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Utilities`"}];


SetOptions[{TestUnchanged, TestTrue, TestFalse},
	ApplyToInput -> PlusMinusOneQ
];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestTrue[{+1}, TestID -> "+1"];
TestTrue[{-1}, TestID -> "-1"];
TestTrue[{PlusMinus[1]}, TestID -> "+-1"];
TestTrue[{MinusPlus[1]}, TestID -> "-+1"];

TestFalse[{0}, TestID -> "0"];
TestFalse[{5}, TestID -> "5"];
TestFalse[{x}, TestID -> "x"];

TestUnchanged[{1, -1}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
