(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Numerics`"}];


spinors = {p1, p2, p3};


(* ::Section:: *)
(*Tests*)


TestNull[
	GenComplexMomenta[spinors],
	TestID -> "GenComplexMomenta evaluation"
];

Test[
	Total[Num4V /@ spinors],
	{0, 0, 0, 0},
	TestID -> "Momentum conservation",
	EquivalenceFunction -> Equal
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
