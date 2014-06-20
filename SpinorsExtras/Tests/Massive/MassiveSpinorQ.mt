(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Massive`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b},
	"LVectors" -> {P, Q, R},
	"SMatrices" -> {M},
	"PlusMinusOnes" -> {u}
];

	
SetOptions[{TestUnchanged, TestTrue, TestFalse},
	ApplyToInput -> MassiveSpinorQ
];


(* ::Section:: *)
(*Tests*)


TestUnchanged[{}, TestID -> "no args"];


TestFalse[{x}, TestID -> "Arbitrary"];
TestFalse[{2}, TestID -> "Integer"];
TestFalse[{a}, TestID -> "Spinor"];
TestFalse[{P}, TestID -> "LVector"];
TestFalse[{M}, TestID -> "SMatrix"];
TestFalse[{u}, TestID -> "+-1"];
TestFalse[{Sp}, TestID -> "Sp"];
TestFalse[{SpM}, TestID -> "SpM"];


TestFalse[{Sp[2]}, TestID -> "Sp: Integer"];


TestFalse[{SpM[x]}, TestID -> "SpM: Arbitrary"];
TestFalse[{SpM[2]}, TestID -> "SpM: Integer"];
TestFalse[{SpM[a]}, TestID -> "SpM: Spinor"];
TestFalse[{SpM[P]}, TestID -> "SpM: LVector"];
TestFalse[{SpM[M]}, TestID -> "SpM: SMatrix"];
TestFalse[{SpM[u]}, TestID -> "SpM: +-1"];


TestFalse[{SpM[x, y]}, TestID -> "SpM: Arbitrary, Arbitrary"];
TestFalse[{SpM[x, 2]}, TestID -> "SpM: Arbitrary, Integer"];
TestFalse[{SpM[x, a]}, TestID -> "SpM: Arbitrary, Spinor"];
TestFalse[{SpM[x, P]}, TestID -> "SpM: Arbitrary, LVector"];
TestFalse[{SpM[x, M]}, TestID -> "SpM: Arbitrary, SMatrix"];
TestFalse[{SpM[x, u]}, TestID -> "SpM: Arbitrary, +-1"];

TestFalse[{SpM[2, y]}, TestID -> "SpM: Integer, Arbitrary"];
TestFalse[{SpM[2, 2]}, TestID -> "SpM: Integer, Integer"];
TestFalse[{SpM[2, a]}, TestID -> "SpM: Integer, Spinor"];
TestFalse[{SpM[2, P]}, TestID -> "SpM: Integer, LVector"];
TestFalse[{SpM[2, M]}, TestID -> "SpM: Integer, SMatrix"];
TestFalse[{SpM[2, u]}, TestID -> "SpM: Integer, +-1"];

TestFalse[{SpM[a, y]}, TestID -> "SpM: Spinor, Arbitrary"];
TestFalse[{SpM[a, 2]}, TestID -> "SpM: Spinor, Integer"];
TestFalse[{SpM[a, a]}, TestID -> "SpM: Spinor, Spinor"];
TestFalse[{SpM[a, P]}, TestID -> "SpM: Spinor, LVector"];
TestFalse[{SpM[a, M]}, TestID -> "SpM: Spinor, SMatrix"];
TestFalse[{SpM[a, u]}, TestID -> "SpM: Spinor, +-1"];

TestFalse[{SpM[P, y]}, TestID -> "SpM: LVector, Arbitrary"];
TestFalse[{SpM[P, 2]}, TestID -> "SpM: LVector, Integer"];
TestFalse[{SpM[P, a]}, TestID -> "SpM: LVector, Spinor"];
TestFalse[{SpM[P, Q]}, TestID -> "SpM: LVector, LVector"];
TestFalse[{SpM[P, M]}, TestID -> "SpM: LVector, SMatrix"];
TestTrue[{SpM[P, u]}, TestID -> "SpM: LVector, +-1"];

TestFalse[{SpM[M, y]}, TestID -> "SpM: SMatrix, Arbitrary"];
TestFalse[{SpM[M, 2]}, TestID -> "SpM: SMatrix, Integer"];
TestFalse[{SpM[M, a]}, TestID -> "SpM: SMatrix, Spinor"];
TestFalse[{SpM[M, P]}, TestID -> "SpM: SMatrix, LVector"];
TestFalse[{SpM[M, M]}, TestID -> "SpM: SMatrix, SMatrix"];
TestFalse[{SpM[M, u]}, TestID -> "SpM: SMatrix, +-u"];

TestFalse[{SpM[u, y]}, TestID -> "SpM: +-1, Arbitrary"];
TestFalse[{SpM[u, 2]}, TestID -> "SpM: +-1, Integer"];
TestFalse[{SpM[u, a]}, TestID -> "SpM: +-1, Spinor"];
TestFalse[{SpM[u, P]}, TestID -> "SpM: +-1, LVector"];
TestFalse[{SpM[u, M]}, TestID -> "SpM: +-1, SMatrix"];
TestFalse[{SpM[u, u]}, TestID -> "SpM: +-1, +-1"];


TestFalse[{SpM[a, u, b]}, TestID -> "SpM: Spinor, +-1, Spinor"];

TestFalse[{SpM[P, x, y]}, TestID -> "SpM: LVector, Arbitrary, Arbitrary"];
TestFalse[{SpM[P, x, 2]}, TestID -> "SpM: LVector, Arbitrary, Integer"];
TestFalse[{SpM[P, x, a]}, TestID -> "SpM: LVector, Arbitrary, Spinor"];
TestFalse[{SpM[P, x, Q]}, TestID -> "SpM: LVector, Arbitrary, LVector"];
TestFalse[{SpM[P, x, M]}, TestID -> "SpM: LVector, Arbitrary, SMatrix"];
TestFalse[{SpM[P, x, u]}, TestID -> "SpM: LVector, Arbitrary, +-1"];

TestFalse[{SpM[P, 2, y]}, TestID -> "SpM: LVector, Integer, Arbitrary"];
TestFalse[{SpM[P, 2, 2]}, TestID -> "SpM: LVector, Integer, Integer"];
TestFalse[{SpM[P, 2, a]}, TestID -> "SpM: LVector, Integer, Spinor"];
TestFalse[{SpM[P, 2, Q]}, TestID -> "SpM: LVector, Integer, LVector"];
TestFalse[{SpM[P, 2, M]}, TestID -> "SpM: LVector, Integer, SMatrix"];
TestFalse[{SpM[P, 2, u]}, TestID -> "SpM: LVector, Integer, +-1"];

TestFalse[{SpM[P, a, y]}, TestID -> "SpM: LVector, Spinor, Arbitrary"];
TestFalse[{SpM[P, a, 2]}, TestID -> "SpM: LVector, Spinor, Integer"];
TestFalse[{SpM[P, a, a]}, TestID -> "SpM: LVector, Spinor, Spinor"];
TestFalse[{SpM[P, a, Q]}, TestID -> "SpM: LVector, Spinor, LVector"];
TestFalse[{SpM[P, a, M]}, TestID -> "SpM: LVector, Spinor, SMatrix"];
TestFalse[{SpM[P, a, u]}, TestID -> "SpM: LVector, Spinor, +-1"];

TestFalse[{SpM[P, Q, y]}, TestID -> "SpM: LVector, LVector, Arbitrary"];
TestFalse[{SpM[P, Q, 2]}, TestID -> "SpM: LVector, LVector, Integer"];
TestFalse[{SpM[P, Q, a]}, TestID -> "SpM: LVector, LVector, Spinor"];
TestFalse[{SpM[P, Q, R]}, TestID -> "SpM: LVector, LVector, LVector"];
TestFalse[{SpM[P, Q, M]}, TestID -> "SpM: LVector, LVector, SMatrix"];
TestFalse[{SpM[P, Q, u]}, TestID -> "SpM: LVector, LVector, +-1"];

TestFalse[{SpM[P, M, y]}, TestID -> "SpM: LVector, SMatrix, Arbitrary"];
TestFalse[{SpM[P, M, 2]}, TestID -> "SpM: LVector, SMatrix, Integer"];
TestFalse[{SpM[P, M, a]}, TestID -> "SpM: LVector, SMatrix, Spinor"];
TestFalse[{SpM[P, M, Q]}, TestID -> "SpM: LVector, SMatrix, LVector"];
TestFalse[{SpM[P, M, M]}, TestID -> "SpM: LVector, SMatrix, SMatrix"];
TestFalse[{SpM[P, M, u]}, TestID -> "SpM: LVector, SMatrix, +-u"];

TestFalse[{SpM[P, u, y]}, TestID -> "SpM: LVector, +-1, Arbitrary"];
TestTrue[{SpM[P, u, 2]}, TestID -> "SpM: LVector, +-1, Integer"];
TestTrue[{SpM[P, u, a]}, TestID -> "SpM: LVector, +-1, Spinor"];
TestFalse[{SpM[P, u, Q]}, TestID -> "SpM: LVector, +-1, LVector"];
TestFalse[{SpM[P, u, M]}, TestID -> "SpM: LVector, +-1, SMatrix"];
TestFalse[{SpM[P, u, u]}, TestID -> "SpM: LVector, +-1, +-1"];


TestUnchanged[{2, a}, TestID -> "two args"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
