(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`SatMmodifications`",
	"SpinorsExtras`Massive`"
}];
SetUpSpinorsTestEnvironment["LVectors" -> {P, Q}];


(* ::Section:: *)
(*Tests*)


SetOptions[{Test, TestSubexpression}, ApplyToInput -> ShiftBA[1, 2, z]];

Test[
	{Spaa[2, 3]},
	Spaa[2, 3] + z Spaa[1, 3],
	TestID -> "massless: A shift A"
];
Test[
	{Spbb[1, 3]},
	Spbb[1, 3] - z Spbb[2, 3],
	TestID -> "massless: B shift B"
];
TestSubexpression[
	{Spaa[1, 3]},
	TestID -> "massless: A shift B"
];
TestSubexpression[
	{Spbb[2, 3]},
	TestID -> "massless: B shift A"
];
Test[
	{Sm[1]},
	Sm[1] - z SmBA[2, 1],
	TestID -> "massless: LV B shift"
];
Test[
	{Sm[2]},
	Sm[2] + z SmBA[2, 1],
	TestID -> "massless: LV A shift"
];


(*SetOptions[{Test, TestSubexpression}, ApplyToInput -> ShiftBA[P, 1, z]];

Test[
	{Spaa[1, 3]},
	Spaa[1, 3] + z Spaa[SpAssoc[P, 1], 3],
	TestID -> "massive B, default A shift: A shift A"
];
Test[
	{Spbb[P, 3]},
	Spbb[P, 3] - z Spbb[1, 3],
	TestID -> "massive B, default A shift: B shift B"
];
TestSubexpression[
	{Spaa[P, 3]},
	TestID -> "massive B, default A shift: A shift B"
];
TestSubexpression[
	{Spbb[2, 3]},
	TestID -> "massive B, default A shift: B shift A"
];

SetOptions[{Test, TestSubexpression}, ApplyToInput -> ShiftBA[1, P, z]];

Test[
	{Spaa[P, 3]},
	Spaa[P, 3] + z Spaa[1, 3],
	TestID -> "massive A, default B shift: A shift A"
];
Test[
	{Spbb[1, 3]},
	Spbb[1, 3] - z Spbb[SpAssoc[P, 1], 3],
	TestID -> "massive A, default B shift: B shift B"
];
TestSubexpression[
	{Spaa[1, 3]},
	TestID -> "massive A, default B shift: A shift B"
];
TestSubexpression[
	{Spbb[P, 3]},
	TestID -> "massive A, default B shift: B shift A"
];*)


SetOptions[{Test, TestSubexpression}, ApplyToInput -> ShiftBA[P, Q, z]];

Test[
	{Spaa[SpM[Q, 1], 3]},
	Spaa[SpM[Q, 1], 3] + z Spaa[SpAssoc[P, Q], 3],
	TestID -> "massive B and A, default B and A shifts: A shift A"
];
Test[
	{Spbb[SpM[P, 1], 3]},
	Spbb[SpM[P, 1], 3] - z Spbb[SpAssoc[Q, P], 3],
	TestID -> "massive B and A, default B and A shifts: B shift B"
];
TestSubexpression[
	{Spaa[SpM[P, 1], 3]},
	TestID -> "massive B and A, default B and A shifts: A shift B"
];
TestSubexpression[
	{Spbb[SpM[Q, 1], 3]},
	TestID -> "massive B and A, default B and A shifts: B shift A"
];
Test[
	{Sm[P]},
	Sm[P] - z SmBA[SpAssoc[Q, P], SpAssoc[P, Q]],
	TestID -> "massive B and A, default B and A shifts: LV B shift"
];
Test[
	{Sm[Q]},
	Sm[Q] + z SmBA[SpAssoc[Q, P], SpAssoc[P, Q]],
	TestID -> "massive B and A, default B and A shifts: LV A shift"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
