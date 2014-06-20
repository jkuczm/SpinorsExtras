(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Package`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P},
	"SMatrices" -> {sm}
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*ScaledQFactory evaluation*)


SetOptions[{TestNull, TestUnchanged},
	ApplyToInput -> ScaledQFactory
];


TestUnchanged[
	{"MyFavorite"},
	TestID-> "ScaledQFactory: 1 args: string"
];

TestUnchanged[
	{x, _},
	TestID-> "ScaledQFactory: 2 args: arbitrary, pattern"
];
TestNull[
	{"MyFavorite", _MyFavorite},
	TestID-> "ScaledQFactory: 2 args: string, pattern"
];

TestUnchanged[
	{"MyFavorite", _, "str"},
	TestID-> "ScaledQFactory: 3 args: string, pattern, string"
];


(* ::Subsection:: *)
(*implemented Scaled<name>Q tests*)


SetOptions[{TestTrue, TestFalse, TestUnchanged},
	ApplyToInput -> ScaledMyFavoriteQ
];


TestUnchanged[{}, TestID -> "Scaled<name>Q: no args"];

TestTrue[{MyFavorite[]}, TestID -> "Scaled<name>Q: MyFavorite"];
TestFalse[{a}, TestID -> "Scaled<name>Q: Spinor"];
TestFalse[{P}, TestID -> "Scaled<name>Q: LVector"];
TestFalse[{sm}, TestID -> "Scaled<name>Q: SMatrix"];
TestFalse[{1}, TestID -> "Scaled<name>Q: Integer"];
TestFalse[{x}, TestID -> "Scaled<name>Q: arbitrary"];

TestTrue[
	{x MyFavorite[]},
	TestID -> "Scaled<name>Q: MyFavorite times one coeff"
];
TestFalse[
	{x a},
	TestID -> "Scaled<name>Q: Spinor times one coeff"
];
TestFalse[
	{x P},
	TestID -> "Scaled<name>Q: LVector times one coeff"
];
TestFalse[
	{x sm},
	TestID -> "Scaled<name>Q: SMatrix times one coeff"
];
TestFalse[
	{a MyFavorite[]},
	TestID -> "Scaled<name>Q: Spinor times MyFavorite"
];

TestTrue[
	{x y MyFavorite[]},
	TestID -> "Scaled<name>Q: MyFavorite times two coeff"
];
TestFalse[{x y a}, TestID -> "Scaled<name>Q: Spinor times two coeff"];
TestFalse[{x y P}, TestID -> "Scaled<name>Q: LVector times two coeff"];
TestFalse[{x y sm}, TestID -> "Scaled<name>Q: SMatrix times two coeff"];

TestFalse[
	{Spaa[a, MyFavorite[]]},
	TestID -> "Scaled<name>Q: Spaa with MyFavorite"
];
TestTrue[
	{Spaa[MyFavorite[], b] MyFavorite[x]},
	TestID -> "Scaled<name>Q: MyFavorite times Spaa with MyFavorite"
];

TestFalse[{x + MyFavorite[]}, TestID -> "Scaled<name>Q: sum with MyFavorite"];
TestFalse[{Sin[MyFavorite[]]}, TestID -> "Scaled<name>Q: Sin with MyFavorite"];

TestFalse[
	{Spba[a, sm, P] + MP2[P] / s[a, P]},
	TestID -> "Scaled<name>Q: combination of scalars"
];
TestFalse[
	{x MyFavorite[] + y MyFavorite[x]},
	TestID ->
		"Scaled<name>Q: combination of different spinors with coefficients"
];

TestTrue[
	{(Spba[a, sm, P] - MP2[P]) MyFavorite[]},
	TestID ->
		"Scaled<name>Q: difference of scalars as coefficient next to Spinor"
];

TestUnchanged[{1, a}, TestID -> "Scaled<name>Q: two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
