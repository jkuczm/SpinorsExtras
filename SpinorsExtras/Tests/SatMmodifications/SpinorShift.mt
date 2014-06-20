(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`SatMmodifications`"}];
SetUpSpinorsTestEnvironment["LVectors" -> {lvL, lvR, P, Q}];


(* ::Section:: *)
(*Tests*)


SetOptions[Test, ApplyToInput -> ASpinorShift];

Test[
	{Spaa[1, 2], 1, z Sp[3]},
	Spaa[1, 2] + z Spaa[3, 2],
	TestID -> "ASpinorShift: Spaa: shift massless: 2 sp"
];
Test[
	{Spaa[P, 2], P, z Sp[3]},
	Spaa[P, 2] + z Spaa[3, 2],
	TestID -> "ASpinorShift: Spaa: shift massive: 1 sp, 1 lv"
];

Test[
	{Spab[lvL, P, lvR], P, Q}
	,
	Spaa[lvL, P] Spbb[P, lvR] + Spaa[lvL, Q] Spbb[P, lvR]
	+ Spab[lvL, P] Spab[P, lvR] + Spab[lvL, P] Spab[Q, lvR]
	- 1/2 Spab[P, P] Spab[lvL, lvR] - 1/2 Spab[Q, P] Spab[lvL, lvR]
	,
	EquivalenceFunction -> (TrueQ[Equal[#1, #2] // Simplify]&),
	TestID -> "ASpinorShift: Spab: shift massive: 3 lv"
];

Test[
	{Sm[1], 1, z Sp[3]},
	Sm[1] + z SmBA[1, 3],
	TestID -> "ASpinorShift: Sm: shift massless: 1 sp"
];
Test[
	{Sm[P], P, z Sp[3]},
	Sm[P] + z SmBA[P, 3] - z/2 Spba[P, 3],
	TestID -> "ASpinorShift: Sm: shift massive: 1 lv"
];

Test[
	{MP[P, Q], P, z Sp[3]},
	MP[P, Q] + z/2 Spab[3, Q, P],
	TestID -> "ASpinorShift: MP: shift massive: 2 lv different"
];
Test[
	{MP2[P], P, z Sp[3]},
	MP2[P] + z/2 Spab[3, P] Spab[P, P] + z^2/4 Spab[3, P]^2,
	TestID -> "ASpinorShift: MP: shift massive: 2 lv same"
];

Test[
	{s[P, Q], P, z Sp[3]},
	s[P, Q] + z Spab[3, Q, P] + z/2 Spab[3, P] Spab[P, P] + z^2/4 Spab[3, P]^2,
	TestID -> "ASpinorShift: s: shift massive: 2 lv"
];
Test[
	{s[P, Q], P, z Sp[3]},
	OptionValue[Test, ApplyToInput][s[P, Q] // ExpandSToSpinors, P, z Sp[3]],
	InputWrapper -> ExpandSToSpinors,
	ExpectedWrapper -> ExpandSToSpinors,
	TestID ->
		"ASpinorShift: s: shift massive: 2 lv, comutation with ExpandSToSpinor"
];


SetOptions[Test, ApplyToInput -> BSpinorShift];

Test[
	{Spbb[1, 2], 1, z Sp[3]},
	Spbb[1, 2] + z Spbb[3, 2],
	TestID -> "BSpinorShift: Spbb: shift massless: 2 sp"
];
Test[
	{Spbb[P, 2], P, z Sp[3]},
	Spbb[P, 2] + z Spbb[3, 2],
	TestID -> "BSpinorShift: Spaa: shift massive: 1 sp, 1 lv"
];

Test[
	{Spab[lvL, P, lvR], P, Q}
	,
	Spaa[lvL, P] Spbb[P, lvR] + Spaa[lvL, P] Spbb[Q, lvR]
	+ Spab[lvL, P] Spab[P, lvR] + Spab[lvL, Q] Spab[P, lvR]
	- 1/2 Spab[P, P] Spab[lvL, lvR] - 1/2 Spab[P, Q] Spab[lvL, lvR]
	,
	EquivalenceFunction -> (TrueQ[Equal[#1, #2] // Simplify]&),
	TestID -> "BSpinorShift: Spab: shift massive: 3 lv"
];

Test[
	{Sm[1], 1, z Sp[3]},
	Sm[1] + z SmBA[3, 1],
	TestID -> "BSpinorShift: Sm: shift massless: 1 sp"
];
Test[
	{Sm[P], P, z Sp[3]},
	Sm[P] + z SmBA[3, P] - z/2 Spba[3, P],
	TestID -> "BSpinorShift: Sm: shift massive: 1 lv"
];

Test[
	{MP[P, Q], P, z Sp[3]},
	MP[P, Q] + z/2 Spba[3, Q, P],
	TestID -> "BSpinorShift: MP: shift massive: 2 lv different"
];
Test[
	{MP2[P], P, z Sp[3]},
	MP2[P] + z/2 Spab[P, 3] Spab[P, P] + z^2/4 Spab[P, 3]^2,
	TestID -> "BSpinorShift: MP: shift massive: 2 lv same"
];

Test[
	{s[P, Q], P, z Sp[3]},
	s[P, Q] + z Spba[3, Q, P] + z/2 Spab[P, 3] Spab[P, P] + z^2/4 Spab[P, 3]^2,
	TestID -> "BSpinorShift: s: shift massive: 2 lv"
];
(*Test[
	OptionValue[Test, ApplyToInput][s[P, Q], P, z Sp[3]] // ExpandSToSpinors,
	OptionValue[Test, ApplyToInput][s[P, Q] // ExpandSToSpinors, P, z Sp[3]],
	EquivalenceFunction -> (TrueQ[Equal[#1, #2] // Simplify]&),
	TestID ->
		"BSpinorShift: s: shift massive: 2 lv, comutation with ExpandSToSpinor"
];*)


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
