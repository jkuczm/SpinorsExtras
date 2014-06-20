(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`SatMmodifications`"}];


(* ::Section:: *)
(*Tests*)


Test[
	Lat[1].(coeff SmBA2[2, 3]).La[4],
	coeff Lat[1].SmBA2[2, 3].La[4],
	TestID -> "coefficient in front of SmBA2"
];

Test[
	CLa[1].(coeff CSmBA2[2,3]).CLat[4],
	coeff CLa[1].CSmBA2[2,3].CLat[4],
	TestID -> "coefficient in front of CSmBA2"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
