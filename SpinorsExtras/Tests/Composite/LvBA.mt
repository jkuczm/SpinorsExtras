(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Composite`",
	"SpinorsExtras`Massive`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c, q1, q2},
	"LVectors" -> {P, Q},
	"PlusMinusOnes" -> {pm1, pm2}
];


arg1List = {x, w x, P, w P, b, w b, SpM[P, pm1], w SpM[P, pm1]};

SetOptions[TestCaseSparse,
	AllTestsArgs ->
		List /@ Join[
			Subsets[arg1List, 1] (* no args, 1 arg *)
			,
			Tuples @ {
				arg1List,
				{y, z y, Q, z Q, a, z a, SpM[Q, pm2], z SpM[Q, pm2]}
			} (* 2 args *)
			,
			{{b, a, c}} (* 3 args *)
		]
];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*SpinorQ, LVectorQ*)


TestCaseSpinorQLVectorQFalse[
	LvBA,
	TestID -> "SpinorQ, LVectorQ: Symbol evaluation"
];


TestCaseSparse[
	{
		{{b, a}, True, TestID -> "SpinorQ, LVectorQ: Spinor, Spinor"},
		{{w b, a}, True, TestID -> "SpinorQ, LVectorQ: scaled Spinor, Spinor"},
		{{b, z a}, True, TestID -> "SpinorQ, LVectorQ: Spinor, scaled Spinor"},
		{{w b, z a}, True,
			TestID -> "SpinorQ, LVectorQ: scaled Spinor, scaled Spinor"}
		,
		{{b, SpM[Q, pm2]}, True,
			TestID -> "SpinorQ, LVectorQ: Spinor, massive Spinor"},
		{{w b, SpM[Q, pm2]}, True,
			TestID ->
				"SpinorQ, LVectorQ: scaled Spinor, massive Spinor"},
		{{b, z SpM[Q, pm2]}, True,
			TestID ->
				"SpinorQ, LVectorQ: Spinor, scaled massive Spinor"},
		{{w b, z SpM[Q, pm2]}, True,
			TestID ->
				"SpinorQ, LVectorQ: scaled Spinor, scaled massive Spinor"}
		,
		{{SpM[P, pm1], a}, True,
			TestID -> "SpinorQ, LVectorQ: massive Spinor, Spinor"},
		{{w SpM[P, pm1], a}, True,
			TestID -> "SpinorQ, LVectorQ: scaled massive Spinor, Spinor"},
		{{SpM[P, pm1], z a}, True,
			TestID -> "SpinorQ, LVectorQ: massive Spinor, scaled Spinor"},
		{{w SpM[P, pm1], z a}, True,
			TestID ->
				"SpinorQ, LVectorQ: scaled massive Spinor, scaled Spinor"}
		,
		{{SpM[P, pm1], SpM[Q, pm2]}, {False, True},
			TestID -> "SpinorQ, LVectorQ: massive Spinor, massive Spinor"},
		{{w SpM[P, pm1], SpM[Q, pm2]}, {False, True},
			TestID ->
				"SpinorQ, LVectorQ: scaled massive Spinor, massive Spinor"},
		{{SpM[P, pm1], z SpM[Q, pm2]}, {False, True},
			TestID ->
				"SpinorQ, LVectorQ: massive Spinor, scaled massive Spinor"},
		{{w SpM[P, pm1], z SpM[Q, pm2]}, {False, True},
			TestID -> "SpinorQ, LVectorQ: \
scaled massive Spinor, scaled massive Spinor"}
	}
	,
	TestID -> "SpinorQ, LVectorQ",
	Test -> TestCaseSpinorQLVectorQ,
	TestDefault -> TestCaseSpinorQLVectorQFalse,
	ApplyToInput -> LvBA
];


(* ::Subsection:: *)
(*Spinorizing Integers*)


TestUnchanged[
	LvBA[1],
	TestID -> "2 args: Integer, arbitrary"
];

TestUnchanged[
	LvBA[2, y],
	TestID -> "2 args: Integer, arbitrary"
];
TestUnchanged[
	LvBA[2, y z],
	TestID -> "2 args: Integer, scaled arbitrary"
];
TestUnchanged[
	LvBA[3, Q],
	TestID -> "2 args: Integer, LVector"
];
TestUnchanged[
	LvBA[3, Q y],
	TestID -> "2 args: Integer, scaled LVector"
];
Test[
	LvBA[4, a],
	LvBA[Sp[4], a],
	TestID -> "2 args: Integer, massless Spinor"
];
Test[
	LvBA[4, y a],
	LvBA[Sp[4], y a],
	TestID -> "2 args: Integer, scaled massless Spinor"
];
Test[
	LvBA[5, SpM[Q, pm2]],
	LvBA[Sp[5], SpM[Q, pm2]],
	TestID -> "2 args: Integer, massive Spinor"
];
Test[
	LvBA[5, y SpM[Q, pm2]],
	LvBA[Sp[5], y SpM[Q, pm2]],
	TestID -> "2 args: Integer, scaled massive Spinor"
];

TestUnchanged[
	LvBA[x, 6],
	TestID -> "2 args: arbitrary, Integer"
];
TestUnchanged[
	LvBA[x z, 6],
	TestID -> "2 args: scaled arbitrary, Integer"
];
TestUnchanged[
	LvBA[P, 7],
	TestID -> "2 args: LVector, Integer"
];
TestUnchanged[
	LvBA[P x, 7],
	TestID -> "2 args: scaled LVector, Integer"
];
Test[
	LvBA[b, 8],
	LvBA[b, Sp[8]],
	TestID -> "2 args: massless Spinor, Integer"
];
Test[
	LvBA[x b, 8],
	LvBA[x b, Sp[8]],
	TestID -> "2 args: scaled massless Spinor, Integer"
];
Test[
	LvBA[SpM[P, pm1], 9],
	LvBA[SpM[P, pm1], Sp[9]],
	TestID -> "2 args: massive Spinor, Integer"
];
Test[
	LvBA[x SpM[P, pm1], 9],
	LvBA[x SpM[P, pm1], Sp[9]],
	TestID -> "2 args: scaled massive Spinor, Integer"
];

Test[
	LvBA[1, 2],
	LvBA[Sp[1], Sp[2]],
	TestID -> "2 args: Integer, Integer"
];


TestUnchanged[
	LvBA[1, a, c],
	TestID -> "3 args: Integer, massless Spinor, massless Spinor"
];


(* ::Subsection:: *)
(*Same B and A contraction*)


TestUnchanged[
	LvBA[x, x],
	TestID -> "2 args (same): arbitrary, arbitrary"
];
TestUnchanged[
	LvBA[x, x z],
	TestID -> "2 args (same): arbitrary, scaled arbitrary"
];
TestUnchanged[
	LvBA[x z, x],
	TestID -> "2 args (same): scaled arbitrary, arbitrary"
];
TestUnchanged[
	LvBA[x z, x z],
	TestID -> "2 args (same): scaled arbitrary, scaled arbitrary"
];

TestUnchanged[
	LvBA[P, P],
	TestID -> "2 args (same): LVector, LVector"
];
TestUnchanged[
	LvBA[P, P z],
	TestID -> "2 args (same LVector): LVector, scaled LVector"
];
TestUnchanged[
	LvBA[P z, P],
	TestID -> "2 args (same LVector): scaled LVector, LVector"
];
TestUnchanged[
	LvBA[P z, P z],
	TestID -> "2 args (same): scaled LVector, scaled LVector"
];

Test[
	LvBA[1, 1],
	Sp[1],
	TestID -> "2 args (same): Integer, Integer"
];

Test[
	LvBA[a, a],
	a,
	TestID -> "2 args (same): massless Spinor, massless Spinor"
];
Test[
	LvBA[a, y a],
	y a,
	TestID -> "2 args (same Spinor): massless Spinor, scaled massless Spinor"
];
Test[
	LvBA[x a, a],
	x a,
	TestID -> "2 args (same Spinor): scaled massless Spinor, massless Spinor"
];
Test[
	LvBA[x a, y a],
	x y a,
	TestID ->
		"2 args (same Spinor): scaled massless Spinor, scaled massless Spinor"
];

Test[
	LvBA[SpM[P, pm1], SpM[P, pm1]],
	P,
	TestID -> "2 args (same): massive Spinor impl ref, massive Spinor impl ref"
];
Test[
	LvBA[SpM[P, pm1], y SpM[P, pm1]],
	y P,
	TestID -> "2 args (same Spinor): \
massive Spinor impl ref, scaled massive Spinor impl ref"
];
Test[
	LvBA[x SpM[P, pm1], SpM[P, pm1]],
	x P,
	TestID -> "2 args (same Spinor): \
scaled massive Spinor impl ref, massive Spinor impl ref"
];
Test[
	LvBA[x SpM[P, pm1], y SpM[P, pm1]],
	x y P,
	TestID -> "2 args (same Spinor): \
scaled massive Spinor impl ref, scaled massive Spinor impl ref"
];

Test[
	LvBA[SpM[P, pm1, q1], SpM[P, pm1, q1]],
	P,
	TestID -> "2 args (same): massive Spinor expl ref, massive Spinor expl ref"
];
Test[
	LvBA[SpM[P, pm1, q1], y SpM[P, pm1, q1]],
	y P,
	TestID -> "2 args (same Spinor): \
massive Spinor expl ref, scaled massive Spinor expl ref"
];
Test[
	LvBA[x SpM[P, pm1, q1], SpM[P, pm1, q1]],
	x P,
	TestID -> "2 args (same Spinor): \
scaled massive Spinor expl ref, massive Spinor expl ref"
];
Test[
	LvBA[x SpM[P, pm1, q1], y SpM[P, pm1, q1]],
	x y P,
	TestID -> "2 args (same Spinor): \
scaled massive Spinor expl ref, scaled massive Spinor expl ref"
];

TestUnchanged[
	LvBA[SpM[P, pm1], SpM[P, pm2]],
	TestID ->
		"2 args (same LVector): \
massive Spinor impl ref, massive Spinor impl ref"
];
TestUnchanged[
	LvBA[SpM[P, pm1], SpM[P, pm1, q1]],
	TestID ->
		"2 args (same LVector, mass sign): \
massive Spinor impl ref, massive Spinor expl ref"
];
TestUnchanged[
	LvBA[SpM[P, pm1, q1], SpM[P, pm1, q2]],
	TestID ->
		"2 args (same LVector, mass sign): \
massive Spinor expl ref, massive Spinor expl ref"
];


(* ::Subsection:: *)
(*Nested LvBA*)


TestCaseRepeated[
	Test[
		LvBA[x, LvBA[b, a]],
		LvBA[x, a],
		TestID ->
			"2 args: arbitrary, composite (massless Spinor, massless Spinor)"
	];
	Test[
		LvBA[LvBA[b, a], y],
		LvBA[b, y],
		TestID ->
			"2 args: composite (massless Spinor, massless Spinor), arbitrary"
	];
	
	TestUnchanged[
		LvBA[x, LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		TestID ->
			"2 args: arbitrary, composite (massive Spinor, massive Spinor)"
	];
	TestUnchanged[
		LvBA[LvBA[SpM[P, pm1], SpM[Q, pm2]], y],
		TestID ->
			"2 args: composite (massive Spinor, massive Spinor), arbitrary"
	];
	,
	TestID -> "Nested LvBA test case",
	RepeatFor ->
		Subsets[{b:(b | SpM[P, pm1]) :> w b, a:(a | SpM[Q, pm2]) :> z a}]
];


(* ::Subsection:: *)
(*Automatic light cone decomposition for massive Spinor paired with massless*)


Test[
	LvBA[SpM[P, pm1], a],
	LvBA[SpAssoc[P], a],
	TestID -> "2 args: massive Spinor impl ref, massless Spinor"
];
Test[
	LvBA[SpM[P, pm1], y a],
	LvBA[SpAssoc[P], y a],
	TestID -> "2 args: massive Spinor impl ref, scaled massless Spinor"
];
Test[
	LvBA[x SpM[P, pm1], a],
	LvBA[x SpAssoc[P], a],
	TestID -> "2 args: scaled massive Spinor impl ref, massless Spinor"
];
Test[
	LvBA[x SpM[P, pm1], y a],
	LvBA[x SpAssoc[P], y a],
	TestID -> "2 args: scaled massive Spinor impl ref, scaled massless Spinor"
];

Test[
	LvBA[SpM[P, pm1, q1], a],
	LvBA[SpAssoc[P, q1], a],
	TestID -> "2 args: massive Spinor expl ref, massless Spinor"
];
Test[
	LvBA[SpM[P, pm1, q1], y a],
	LvBA[SpAssoc[P, q1], y a],
	TestID -> "2 args: massive Spinor expl ref, scaled massless Spinor"
];
Test[
	LvBA[x SpM[P, pm1, q1], a],
	LvBA[x SpAssoc[P, q1], a],
	TestID -> "2 args: scaled massive Spinor expl ref, massless Spinor"
];
Test[
	LvBA[x SpM[P, pm1, q1], y a],
	LvBA[x SpAssoc[P, q1], y a],
	TestID -> "2 args: scaled massive Spinor expl ref, scaled massless Spinor"
];

Test[
	LvBA[b, SpM[Q, pm2]],
	LvBA[b, SpAssoc[Q]],
	TestID -> "2 args: massless Spinor, massive Spinor impl ref"
];
Test[
	LvBA[b, y SpM[Q, pm2]],
	LvBA[b, y SpAssoc[Q]],
	TestID -> "2 args: massless Spinor, scaled massive Spinor impl ref"
];
Test[
	LvBA[x b, SpM[Q, pm2]],
	LvBA[x b, SpAssoc[Q]],
	TestID -> "2 args: scaled massless Spinor, massive Spinor impl ref"
];
Test[
	LvBA[x b, y SpM[Q, pm2]],
	LvBA[x b, y SpAssoc[Q]],
	TestID -> "2 args: scaled massless Spinor, scaled massive Spinor impl ref"
];

Test[
	LvBA[b, SpM[Q, pm2, q2]],
	LvBA[b, SpAssoc[Q, q2]],
	TestID -> "2 args: massless Spinor, massive Spinor expl ref"
];
Test[
	LvBA[b, y SpM[Q, pm2, q2]],
	LvBA[b, y SpAssoc[Q, q2]],
	TestID -> "2 args: massless Spinor, scaled massive Spinor expl ref"
];
Test[
	LvBA[x b, SpM[Q, pm2, q2]],
	LvBA[x b, SpAssoc[Q, q2]],
	TestID -> "2 args: scaled massless Spinor, massive Spinor expl ref"
];
Test[
	LvBA[x b, y SpM[Q, pm2, q2]],
	LvBA[x b, y SpAssoc[Q, q2]],
	TestID -> "2 args: scaled massless Spinor, scaled massive Spinor expl ref"
];

TestUnchanged[
	LvBA[SpM[P, pm1], SpM[Q, pm2]],
	TestID -> "2 args: massive Spinor impl ref, massive Spinor impl ref"
];
TestUnchanged[
	LvBA[SpM[P, pm1], SpM[Q, pm2, q2]],
	TestID -> "2 args: massive Spinor impl ref, massive Spinor expl ref"
];
TestUnchanged[
	LvBA[SpM[P, pm1, q1], SpM[Q, pm2]],
	TestID -> "2 args: massive Spinor expl ref, massive Spinor impl ref"
];
TestUnchanged[
	LvBA[SpM[P, pm1, q1], SpM[Q, pm2, q2]],
	TestID -> "2 args: massive Spinor expl ref, massive Spinor expl ref"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
