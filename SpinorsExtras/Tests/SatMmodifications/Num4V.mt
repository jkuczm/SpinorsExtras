(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`SatMmodifications`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a},
	"LVectors" -> {P, Q, L},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All
];


SetOptions[{Test, TestUnchangedHead}, ApplyToInput -> Num4V];


(* ::Section:: *)
(*Tests*)

Test[
	{5 P},
	5 Num4V[P],
	TestID -> "integer LVector"
];
Test[
	{x P},
	x Num4V[P],
	TestID -> "symbol LVector"
];
Test[
	{x y P},
	x y Num4V[P],
	TestID -> "2 symbols LVector"
];
Test[
	{MP[Q, L] P},
	MP[Q, L] Num4V[P],
	TestID -> "MP LVector"
];
Test[
	{5 P + x y a},
	5 Num4V[P] + x y Num4V[a],
	TestID -> "sum of prducts with integers and symbols"
];
Test[
	{5 s[Q, P] P + x y MP2[L] a},
	5 s[Q, P] Num4V[P] + x y MP2[L] Num4V[a],
	TestID -> "sum of prducts with integers, symbols and scalar combinations \
of LVectors"
];


TestUnchangedHead[{Q P}, TestID -> "two LVectors"];
TestUnchangedHead[{a P}, TestID -> "spinor LVector"];
TestUnchangedHead[{x a P}, TestID -> "symbol spinor LVector"];
TestUnchangedHead[{2 Q (P + a)}, TestID -> "symbol spinor LVector"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
