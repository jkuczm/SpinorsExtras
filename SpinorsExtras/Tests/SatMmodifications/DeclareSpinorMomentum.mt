(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`SatMmodifications`"}];


la = {{1}, {2}}
lat = {{3, 4}}


(* ::Section:: *)
(*Tests*)


TestNull[
	DeclareSpinorMomentum[1, la, lat],
	TestID -> "declaration with integer"
];

TestFalse[
	SpinorQ[1],
	TestID -> "Raw integer is not a spinor"
];

Test[
	Spinors`Private`NumLa[Sp[1]],
	la,
	TestID -> "Proper NumLa set"
];

Test[
	Spinors`Private`NumLat[Sp[1]],
	lat,
	TestID -> "Proper NumLat set"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
