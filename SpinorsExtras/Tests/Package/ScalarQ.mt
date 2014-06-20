(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Package`",
	"SpinorsExtras`Massive`" (* SpAssoc *)
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a},
	"LVectors" -> {P},
	"SMatrices" -> {sm}
];


SetOptions[{TestUnchanged, TestTrue, TestFalse}, ApplyToInput -> ScalarQ];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestTrue[{1}, TestID -> "Integer"];
TestTrue[{x}, TestID -> "arbitrary"];

TestFalse[{a}, TestID -> "Spinor"];
TestFalse[{P}, TestID -> "LVector"];
TestFalse[{sm}, TestID -> "SMatrix"];
TestFalse[{Sp[1]}, TestID -> "Sp"];

TestTrue[{Spaa[a, P]}, TestID -> "Spaa"];
TestTrue[{Spbb[a, P]}, TestID -> "Spbb"];
TestTrue[{Spab[a, sm, P]}, TestID -> "Spab"];
TestTrue[{Spba[a, sm, P]}, TestID -> "Spba"];
TestTrue[{s[a, P]}, TestID -> "s"];
TestTrue[{MP[a, P]}, TestID -> "MP"];
TestTrue[{MP2[P]}, TestID -> "MP2"];

TestFalse[{x + sm}, TestID -> "sum with SMatrix"];
TestFalse[{x P}, TestID -> "product with LVector"];
TestFalse[{Sin[a]}, TestID -> "Sin with Spinor"];
TestFalse[{SpAssoc[P]}, TestID -> "SpAssoc with LVector"];

TestTrue[
	{Spba[a, sm, P] + MP2[P] / s[a, P]},
	TestID -> "combination of scalars"
];
TestFalse[
	{Spba[a, sm, P] P + MP2[P] / s[a, P]},
	TestID -> "combination with LVector"
];

TestFalse[
	{(Spba[a, sm, P] - MP2[P]) a},
	TestID -> "difference of scalars as coefficient next to Spinor"
];

TestUnchanged[{1, x}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
