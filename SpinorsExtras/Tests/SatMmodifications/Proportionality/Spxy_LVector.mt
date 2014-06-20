(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Proportional`",
	"SpinorsExtras`SatMmodifications`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P, Q},
	"SMatrices" -> {Sm1, Sm2},
	"PlusMinusOnes" -> {uv1, uv2}
];


DeclareLVectorProportional[a, b];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Spaa, Spbb*)


TestCaseRepeated[
	TestZero[Spxx[a, b], TestID -> "2 args: prop 1, 2"];
	
	TestZero[Spxx[a, b, x, y], TestID -> "4 args: prop 1, 2"];
	TestZero[Spxx[x, a, b, y], TestID -> "4 args: prop 2, 3"];
	TestZero[Spxx[x, y, a, b], TestID -> "4 args: prop 3, 4"];
	
	TestZero[Spxx[a, b, x, y, z, t], TestID -> "6 args: prop 1, 2"];
	TestZero[Spxx[x, a, b, y, z, t], TestID -> "6 args: prop 2, 3"];
	TestZero[Spxx[x, y, a, b, z, t], TestID -> "6 args: prop 3, 4"];
	TestZero[Spxx[x, y, z, a, b, t], TestID -> "6 args: prop 4, 5"];
	TestZero[Spxx[x, y, z, t, a, b], TestID -> "6 args: prop 5, 6"];
	,
	TestID -> "Spaa, Spbb",
	RepeatFor -> {
		Spxx -> Spaa,
		Spxx -> Spbb
	}
]


(* ::Subsection:: *)
(*Spab, Spba*)


TestCaseRepeated[
	TestZero[Spxy[a, b, x], TestID -> "3 args: prop 1, 2"];
	TestZero[Spxy[x, a, b], TestID -> "3 args: prop 2, 3"];
	
	TestZero[Spxy[a, b, x, y, z], TestID -> "3 args: prop 1, 2"];
	TestZero[Spxy[x, a, b, y, z], TestID -> "3 args: prop 2, 3"];
	TestZero[Spxy[x, y, a, b, z], TestID -> "3 args: prop 3, 4"];
	TestZero[Spxy[x, y, z, a, b], TestID -> "3 args: prop 4, 5"];
	,
	TestID -> "Spab, Spba",
	RepeatFor -> {
		Spxy -> Spab,
		Spxy -> Spba
	}
]


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
