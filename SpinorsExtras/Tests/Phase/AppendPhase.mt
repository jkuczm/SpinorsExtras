(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Phase`",
	"SpinorsExtras`SatMmodifications`" (* linearity of Dot *)
}];


(* ::Section:: *)
(*Tests*)


Test[
	AppendPhase[CLa[1].La[2], _La, "PhaseConvention" -> "MyPhaseConvention"],
	Phase["MyPhaseConvention", La[Sp[2]]] CLa[1].La[2],
	TestID -> "La"
];

Test[
	AppendPhase[
		CLa[1].La[2],
		_La|_CLa,
		"PhaseConvention" -> "MyPhaseConvention"
	]
	,
	Phase["MyPhaseConvention", CLa[Sp[1]]] *
	Phase["MyPhaseConvention", La[Sp[2]]] *
	CLa[1].La[2]
	,
	TestID -> "La, Cla"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
