(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Utilities`"}];


(* ::Section:: *)
(*Tests*)


Test[
	ExternalMomentaPartitions[{k1, k2, k3, k4}, {k1, k3}],
	{{{k1, k2}, {k3, k4}}, {{k1, k4}, {k2, k3}}},
	TestID -> "4 mom"
];
Test[
	ExternalMomentaPartitions[{k1, k2, k3, k4, k5}, {k2, k5}],
	{
		{{k1, k2}, {k3, k4, k5}}, {{k2, k3}, {k1, k4, k5}},
		{{k2, k4}, {k1, k3, k5}}, {{k1, k2, k3}, {k4, k5}},
		{{k1, k2, k4}, {k3, k5}}, {{k2, k3, k4}, {k1, k5}}
	},
	TestID -> "5 mom"
];

TestFailed[
	ExternalMomentaPartitions[{k1, k2, k3, k4}, {k1, k5}],
	{
		HoldForm @ Message[
			ExternalMomentaPartitions::momMissing,
			{k5}, {k1, k2, k3, k4}
		]
	},
	TestID -> "first singled out mom missing"
];
TestFailed[
	ExternalMomentaPartitions[{k1, k2, k3, k4}, {k5, k1}],
	{
		HoldForm @ Message[
			ExternalMomentaPartitions::momMissing,
			{k5}, {k1, k2, k3, k4}
		]
	},
	TestID -> "second singled out mom missing"
];
TestFailed[
	ExternalMomentaPartitions[{k1, k2, k3, k4}, {k5, k6}],
	{
		HoldForm @ Message[
			ExternalMomentaPartitions::momMissing,
			{k5, k6}, {k1, k2, k3, k4}
		]
	},
	TestID -> "2 singled out mom missing"
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
