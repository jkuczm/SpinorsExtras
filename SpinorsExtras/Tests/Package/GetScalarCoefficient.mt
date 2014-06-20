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


SetOptions[{Test, TestFailed}, InputWrapper -> GetScalarCoefficient];


(* ::Section:: *)
(*Tests*)


Test[Sp[1], 1, TestID -> "Sp"];
Test[a, 1, TestID -> "Spinor"];
Test[SpAssoc[P], 1, TestID -> "SpAssoc with LVector"];
Test[P, 1, TestID -> "LVector"];

TestFailed[
	sm,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, sm]]},
	TestID -> "SMatrix"
];
TestFailed[
	1,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, 1]]},
	TestID -> "Integer"
];
TestFailed[
	x,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, x]]},
	TestID -> "arbitrary"
];

Test[x a, x, TestID -> "Spinor times one coeff"];
Test[x P, x, TestID -> "LVector times one coeff"];
TestFailed[
	x sm,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, sm x]]},
	TestID -> "SMatrix times one coeff"
];
TestFailed[
	P Q,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, P Q]]},
	TestID -> "LVector times LVector"
];

Test[x y a, x y, TestID -> "Spinor times two coeff"];
Test[x y P, x y, TestID -> "LVector times two coeff"];
TestFailed[
	x y sm,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, sm x y]]},
	TestID -> "SMatrix times two coeff"
];

TestFailed[
	Spaa[P, Q],
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, Spaa[P, Q]]]},
	TestID -> "Spaa with LVectors"
];
Test[Spaa[P, Q] P, Spaa[P, Q], TestID -> "LVector times Spaa with LVectors"];

TestFailed[
	x + sm,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, sm + x]]},
	TestID -> "sum with SMatrix"
];
TestFailed[
	Sin[a],
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, Sin[a]]]},
	TestID -> "Sin with Spinor"
];

TestFailed[
	Spba[a, sm, P] + MP2[P] / s[a, P],
	{
		HoldForm @ Message[
			GetScalarCoefficient::notScaledLVector,
			MP[P, P] / s[a, P] + Spba[a, sm, P]
		]
	},
	TestID -> "combination of scalars"
];
TestFailed[
	x P + y Q,
	{HoldForm[Message[GetScalarCoefficient::notScaledLVector, P x + Q y]]},
	TestID -> "combination of different LVectors with coefficients"
];

Test[
	(Spba[a, sm, P] - MP2[P]) P,
	(Spba[a, sm, P] - MP2[P]),
	TestID -> "difference of scalars as coefficient next to LVector"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
