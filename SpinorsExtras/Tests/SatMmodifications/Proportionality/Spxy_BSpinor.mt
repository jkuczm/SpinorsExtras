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


DeclareBSpinorProportional[a, b];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Spbb*)


TestZero[Spbb[a, b], TestID -> "Spbb: 2 args: prop 1, 2"];

TestZero[Spbb[a, b, x, y], TestID -> "Spbb: 4 args: prop 1, 2"];
TestUnchanged[Spbb[x, a, b, y], TestID -> "Spbb: 4 args: prop 2, 3"];
TestZero[Spbb[x, y, a, b], TestID -> "Spbb: 4 args: prop 3, 4"];

TestZero[Spbb[a, b, x, y, z, t], TestID -> "Spbb: 6 args: prop 1, 2"];
TestUnchanged[Spbb[x, a, b, y, z, t], TestID -> "Spbb: 6 args: prop 2, 3"];
TestUnchanged[Spbb[x, y, a, b, z, t], TestID -> "Spbb: 6 args: prop 3, 4"];
TestUnchanged[Spbb[x, y, z, a, b, t], TestID -> "Spbb: 6 args: prop 4, 5"];
TestZero[Spbb[x, y, z, t, a, b], TestID -> "Spbb: 6 args: prop 5, 6"];


(* ::Subsection:: *)
(*Spaa*)


TestUnchanged[Spaa[a, b], TestID -> "Spaa: 2 args: prop 1, 2"];

TestUnchanged[Spaa[a, b, x, y], TestID -> "Spaa: 4 args: prop 1, 2"];
TestUnchanged[Spaa[x, a, b, y], TestID -> "Spaa: 4 args: prop 2, 3"];
TestUnchanged[Spaa[x, y, a, b], TestID -> "Spaa: 4 args: prop 3, 4"];

TestUnchanged[Spaa[a, b, x, y, z, t], TestID -> "Spaa: 6 args: prop 1, 2"];
TestUnchanged[Spaa[x, a, b, y, z, t], TestID -> "Spaa: 6 args: prop 2, 3"];
TestUnchanged[Spaa[x, y, a, b, z, t], TestID -> "Spaa: 6 args: prop 3, 4"];
TestUnchanged[Spaa[x, y, z, a, b, t], TestID -> "Spaa: 6 args: prop 4, 5"];
TestUnchanged[Spaa[x, y, z, t, a, b], TestID -> "Spaa: 6 args: prop 5, 6"];


(* ::Subsection:: *)
(*Spab*)


TestUnchanged[Spab[a, b, x], TestID -> "Spab: 3 args: prop 1, 2"];
TestZero[Spab[x, a, b], TestID -> "Spab: 3 args: prop 2, 3"];

TestUnchanged[Spab[a, b, x, y, z], TestID -> "Spab: 3 args: prop 1, 2"];
TestUnchanged[Spab[x, a, b, y, z], TestID -> "Spab: 3 args: prop 2, 3"];
TestUnchanged[Spab[x, y, a, b, z], TestID -> "Spab: 3 args: prop 3, 4"];
TestZero[Spab[x, y, z, a, b], TestID -> "Spab: 3 args: prop 4, 5"];


(* ::Subsection:: *)
(*Spba*)


TestZero[Spba[a, b, x], TestID -> "Spba: 3 args: prop 1, 2"];
TestUnchanged[Spba[x, a, b], TestID -> "Spba: 3 args: prop 2, 3"];

TestZero[Spba[a, b, x, y, z], TestID -> "Spba: 3 args: prop 1, 2"];
TestUnchanged[Spba[x, a, b, y, z], TestID -> "Spba: 3 args: prop 2, 3"];
TestUnchanged[Spba[x, y, a, b, z], TestID -> "Spba: 3 args: prop 3, 4"];
TestUnchanged[Spba[x, y, z, a, b], TestID -> "Spba: 3 args: prop 4, 5"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
