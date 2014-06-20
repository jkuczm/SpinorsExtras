(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`SatMmodifications`"}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, b, c},
	"LVectors" -> {J, L, P, Q, R},
	"RandomMomentaSpinors" -> All,
	"RandomMomentaLVectors" -> All
];


(* ::Section:: *)
(*Tests*)


(* Handling of sums. *)
Test[
	s[J, L + x P],
	s[J, L, x P],
	TestID -> "two, sum of two"
];
Test[
	s[J, x L + P + y z Q],
	s[J, x L, P, y z Q],
	TestID -> "two, sum of three"
];
Test[
	s[J, x L + P, Q],
	s[J, x L, P, Q],
	TestID -> "three, sum of two"
];
Test[
	s[J, L + P + Q, R],
	s[J, L, P, Q, R],
	TestID -> "three, sum of three"
];

TestUnchanged[
	s[J, L + P + x],
	TestID -> "non LVector in addends: Head"
];
Test[
	List @@ Evaluate[s[J, L + P + x]],
	{J, L + P + x},
	TestID -> "non LVector in addends: args"
];


(* Handling of repeated arguments. *)
(*Test[
	s[J, J],
	s[2 J],
	TestID -> "two, one double"
];
Test[
	s[J, L, L],
	s[J, 2 L],
	TestID -> "three, one double"
];
Test[
	s[J, L, L, P],
	s[J, 2 L, P],
	TestID -> "four, one double"
];
Test[
	s[J, L, P, L],
	s[J, 2 L, P],
	TestID -> "four, one double separated"
];
Test[
	s[J, L, L, L],
	s[J, 3 L],
	TestID -> "four, one tripple"
];
Test[
	s[J, w L, L, x y L],
	s[J, (w + 1 + x y) L],
	TestID -> "four, one tripple with different coefficients"
];*)


(* Handling of one argument. *)
Test[
	s[x],
	MP2[x],
	TestID -> "one arbitrary arg"
];
Test[
	s[P],
	MP2[P],
	TestID -> "one LVector arg"
];
Test[
	s[x y P],
	x^2 y^2 MP2[P],
	TestID -> "one LVector times coeff arg"
];


(* Test numerics of s with LVectors multiplied with coefficients. *)
SetOptions[Test,
	EquivalenceFunction -> Equal,
	InputWrapper -> (Simplify[N[#, 10]]&),
	ExpectedWrapper ->
		(SetAccuracy[Simplify[First[#^2] - Total[Rest[#^2]]], 10]&)
];


Test[
	s[a, x b],
	Num4V[a] + x Num4V[b],
	TestID -> "Numerics: 2 Spinors: 1 with 1 coeff"
];
Test[
	s[a, x y b],
	Num4V[a] + x y Num4V[b],
	TestID -> "Numerics: 2 Spinors: 1 with 2 coeff"
];
Test[
	s[x a, y z b],
	x Num4V[a] + y z Num4V[b],
	TestID -> "Numerics: 2 Spinors: 1 with 1 coeff, 1 with 2 coeff"
];
Test[
	s[x a, y b, w z c],
	x Num4V[a] + y Num4V[b] + w z Num4V[c],
	TestID -> "Numerics: 3 Spinors: 2 with 1 coeff, 1 with 2 coeff"
];

Test[
	s[P, x Q],
	Num4V[P] + x Num4V[Q],
	TestID -> "Numerics: 2 LVectors: 1 with 1 coeff"
];
Test[
	s[P, x y Q],
	Num4V[P] + x y Num4V[Q],
	TestID -> "Numerics: 2 LVectors: 1 with 2 coeff"
];
Test[
	s[x P, y z Q],
	x Num4V[P] + y z Num4V[Q],
	TestID -> "Numerics: 2 LVectors: 1 with 1 coeff, 1 with 2 coeff"
];
Test[
	s[x P, y Q, w z L],
	x Num4V[P] + y Num4V[Q] + w z Num4V[L],
	TestID -> "Numerics: 3 LVectors: 2 with 1 coeff, 1 with 2 coeff"
];

Test[
	s[a, x Q],
	Num4V[a] + x Num4V[Q],
	TestID -> "Numerics: 1 Spinor, 1 LVector with 1 coeff"
];
Test[
	s[P, x y b],
	Num4V[P] + x y Num4V[b],
	TestID -> "Numerics: 1 LVector, 1 Spinor with 2 coeff"
];
Test[
	s[x a, y z Q],
	x Num4V[a] + y z Num4V[Q],
	TestID -> "Numerics: 1 Spinor with 1 coeff, 1 LVector with 2 coeff"
];
Test[
	s[x P, y Q, w z c],
	x Num4V[P] + y Num4V[Q] + w z Num4V[c],
	TestID -> "Numerics: 2 LVectors with 1 coeff, 1 Spinor with 2 coeff"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
