(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Ref`"}];
SetUpSpinorsTestEnvironment[
	"LVectors" -> {P, Q},
	"RandomMomentaLVectors" -> {P}
	(*
		Don't automatically declare momenta for SpRef since that's exactly
		what we're testing here.
	*)
];


(* ::Section:: *)
(*Tests*)


TestNull[
	DeclareSpinorMomentum[SpRef[P]],
	TestID -> "declaration"
];
TestZero[
	MP2[Num4V[SpRef[P]]],
	TestID -> "maslessnes",
	EquivalenceFunction -> Equal
];

threeMom = Num4V[P][[2;;4]]
Test[
	Num4V[SpRef[P]],
	Prepend[-threeMom, Sign[Num4V[P][[1]]] Sqrt[Total[threeMom^2]]],
	EquivalenceFunction -> Equal,
	TestID -> "numeric"
];

TestFailed[
	DeclareSpinorMomentum[SpRef[Q]],
	{HoldForm[Message[DeclareSpinorMomentum::firstDeclare, SpRef[Q], Q]]},
	TestID -> "base FV not declared"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
