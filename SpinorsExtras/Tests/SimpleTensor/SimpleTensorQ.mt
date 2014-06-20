(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`SimpleTensor`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c},
	"RandomMomentaSpinors" -> All
];


SetOptions[{TestTrue, TestFalse}, ApplyToInput -> SimpleTensorQ];


(* ::Section:: *)
(*Tests*)


TestUnchanged[
	{},
	TestID -> "no args"
];

TestTrue[
	{Sm[a]},
	TestID -> "single Sm"
];
TestTrue[
	{SmBA[a, b]},
	TestID -> "single SmBA"
];

TestTrue[
	{5 Sm[a]},
	TestID -> "multiplied Sm"
];
TestTrue[
	{X SmBA[a, b]},
	TestID -> "multiplied SmBA"
];

TestFalse[
	{Sm[a] + Sm[b]},
	TestID -> "different Sm-s"
];
TestFalse[
	{2.1 Sm[a] + Sm[b]},
	TestID -> "different Sm-s one multiplied"
];
TestFalse[
	{2 Sm[a] + 7. Sm[b]},
	TestID -> "different Sm-s both multiplied"
];

TestTrue[
	{Sm[a] + SmBA[a, b]},
	TestID -> "Sm and SmBA same B"
];
TestTrue[
	{Sm[a] + SmBA[b, a]},
	TestID -> "Sm and SmBA same A"
];
TestFalse[
	{Sm[a] + SmBA[b, c]},
	TestID -> "Sm and SmBA different"
];
TestTrue[
	{Sm[a] 3.2 + SmBA[a, b]},
	TestID -> "Sm multiplied and SmBA same B"
];
TestTrue[
	{8 Sm[a] + SmBA[b, a]},
	TestID -> "Sm multiplied and SmBA same A"
];
TestFalse[
	{12.2 Sm[a] + SmBA[b, c]},
	TestID -> "Sm multiplied and SmBA different"
];

TestTrue[
	{Sm[a] + SmBA[a, b] 3.2},
	TestID -> "Sm and SmBA multiplied same B"
];
TestTrue[
	{Sm[a] + 8 SmBA[b, a]},
	TestID -> "Sm and SmBA multiplied same A"
];
TestFalse[
	{Sm[a] + 12.2 SmBA[b, c]},
	TestID -> "Sm and SmBA multiplied different"
];

TestTrue[
	{9 Sm[a] + SmBA[a, b] 3.2},
	TestID -> "Sm multiplied and SmBA multiplied same B"
];
TestTrue[
	{5.4 Sm[a] + 8 SmBA[b, a]},
	TestID -> "Sm multiplied and SmBA multiplied same A"
];
TestFalse[
	{87 Sm[a] + 12.2 SmBA[b, c]},
	TestID -> "Sm multiplied and SmBA multiplied different"
];

TestUnchanged[
	{Sm[a], Sm[a]},
	TestID -> "two args"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
