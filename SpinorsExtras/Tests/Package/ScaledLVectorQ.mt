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
	"LVectors" -> {P, Q},
	"SMatrices" -> {sm}
];


SetOptions[{TestUnchanged, TestTrue, TestFalse},
	ApplyToInput -> ScaledLVectorQ
];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestTrue[{Sp[1]}, TestID -> "Sp"];
TestTrue[{a}, TestID -> "Spinor"];
TestTrue[{SpAssoc[P]}, TestID -> "SpAssoc with LVector"];
TestTrue[{P}, TestID -> "LVector"];

TestFalse[{sm}, TestID -> "SMatrix"];
TestFalse[{1}, TestID -> "Integer"];
TestFalse[{x}, TestID -> "arbitrary"];

TestTrue[{x a}, TestID -> "Spinor times one coeff"];
TestTrue[{x P}, TestID -> "LVector times one coeff"];
TestFalse[{x sm}, TestID -> "SMatrix times one coeff"];
TestFalse[{P Q}, TestID -> "LVector times LVector"];

TestTrue[{x y a}, TestID -> "Spinor times two coeff"];
TestTrue[{x y P}, TestID -> "LVector times two coeff"];
TestFalse[{x y sm}, TestID -> "SMatrix times two coeff"];

TestFalse[{Spaa[P, Q]}, TestID -> "Spaa with LVectors"];
TestTrue[{Spaa[P, Q] P}, TestID -> "LVector times Spaa with LVectors"];

TestFalse[{x + sm}, TestID -> "sum with SMatrix"];
TestFalse[{Sin[a]}, TestID -> "Sin with Spinor"];

TestFalse[
	{Spba[a, sm, P] + MP2[P] / s[a, P]},
	TestID -> "combination of scalars"
];
TestFalse[
	{x P + y Q},
	TestID -> "combination of different LVectors with coefficients"
];

TestTrue[
	{(Spba[a, sm, P] - MP2[P]) P},
	TestID -> "difference of scalars as coefficient next to LVector"
];

TestUnchanged[{Sp[1], a}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
