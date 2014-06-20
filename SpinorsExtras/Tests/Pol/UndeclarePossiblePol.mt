(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Pol`"}];


SetOptions[TestCaseUndeclare,
	"UndeclareFunction" -> UndeclarePossiblePol,
	"QFunction" -> PossiblePolQ
];


(* ::Section:: *)
(*Tests*)


TestCaseUndeclare[
	{x},
	TestID -> "one symbol"
];

TestCaseUndeclare[
	{x, y, z},
	TestID -> "three symbols"
];

TestCaseUndeclare[
	{A[_, _]},
	{
		A -> False,
		A[x] -> False,
		A[x, y] -> True
	},
	{
		A -> False,
		A[x] -> False,
		A[x, y] -> False
	},
	TestID -> "pattern"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
