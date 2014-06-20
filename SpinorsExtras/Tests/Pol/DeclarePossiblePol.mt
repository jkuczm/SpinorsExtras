(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Pol`"}];


SetOptions[TestCaseDeclare,
	"DeclareFunction" -> DeclarePossiblePol,
	"QFunction" -> PossiblePolQ
];


(* ::Section:: *)
(*Tests*)


TestCaseDeclare[
	{x},
	TestID -> "one symbol"
];

TestCaseDeclare[
	{x, y, z},
	TestID -> "three symbols"
];

TestCaseDeclare[
	{A[_, _]},
	{
		A -> False,
		A[x] -> False,
		A[x, y] -> False
	},
	{
		A -> False,
		A[x] -> False,
		A[x, y] -> True
	},
	TestID -> "pattern"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
