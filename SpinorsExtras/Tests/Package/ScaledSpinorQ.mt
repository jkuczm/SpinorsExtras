(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Package`",
	"SpinorsExtras`Massive`" (* SpAssoc *)
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P},
	"SMatrices" -> {sm}
];


SetOptions[{TestTrue, TestFalse}, ApplyToInput -> ScaledSpinorQ];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];

TestTrue[{Sp[1]}, TestID -> "Sp"];
TestTrue[{a}, TestID -> "Spinor"];
TestTrue[{SpAssoc[P]}, TestID -> "SpAssoc with LVector"];

TestFalse[{P}, TestID -> "LVector"];
TestFalse[{sm}, TestID -> "SMatrix"];
TestFalse[{1}, TestID -> "Integer"];
TestFalse[{x}, TestID -> "arbitrary"];

TestTrue[{x a}, TestID -> "Spinor times one coeff"];
TestFalse[{x P}, TestID -> "LVector times one coeff"];
TestFalse[{x sm}, TestID -> "SMatrix times one coeff"];
TestFalse[{a b}, TestID -> "Spinor times Spinor"];

TestTrue[{x y a}, TestID -> "Spinor times two coeff"];
TestFalse[{x y P}, TestID -> "LVector times two coeff"];
TestFalse[{x y sm}, TestID -> "SMatrix times two coeff"];

TestFalse[{Spaa[a, b]}, TestID -> "Spaa with spinors"];
TestTrue[{Spaa[a, b] a}, TestID -> "Spinor times Spaa with spinors"];

TestFalse[{x + sm}, TestID -> "sum with SMatrix"];
TestFalse[{Sin[a]}, TestID -> "Sin with Spinor"];

TestFalse[
	{Spba[a, sm, P] + MP2[P] / s[a, P]},
	TestID -> "combination of scalars"
];
TestFalse[
	{x a + y b},
	TestID -> "combination of different spinors with coefficients"
];

TestTrue[
	{(Spba[a, sm, P] - MP2[P]) a},
	TestID -> "difference of scalars as coefficient next to Spinor"
];

TestUnchanged[{1, a}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
