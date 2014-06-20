(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{
	"SpinorsExtras`Composite`",
	"SpinorsExtras`Massive`"
}];
SetUpSpinorsTestEnvironment[
	"Spinors" -> {a, a2, b, b2, c, d, e},
	"LVectors" -> {L, P, Q, R},
	"PlusMinusOnes" -> {pm1, pm2, pm3, pm4}
];


BScaleRule = b:(b | SpM[P, pm1] | SpAssoc[P]) :> B b;
AScaleRule = a:(a | SpM[Q, pm2] | SpAssoc[Q]) :> A a;
BAScaleRules = Subsets[{BScaleRule, AScaleRule}];

SetOptions[TestCaseRepeated, RepeatFor -> BAScaleRules];


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Spab, Spba*)


TestCaseRepeated[
	TestUnchanged[
		Spxy[LvBA[P, Q], d, e],
		TestID -> "Spab, Spba: (B: LVector, A: LVector): left"
	];
	TestUnchanged[
		Spxy[c, LvBA[P, Q], e],
		TestID -> "Spab, Spba: (B: LVector, A: LVector): inside"
	];
	TestUnchanged[
		Spxy[c, d, LvBA[P, Q]],
		TestID -> "Spab, Spba: (B: LVector, A: LVector): right"
	];
	
	TestUnchanged[
		Spxy[LvBA[b, Q], d, e],
		TestID -> "Spab, Spba: (B: Massless Spinor, A: LVector): left"
	];
	TestUnchanged[
		Spxy[c, LvBA[b, Q], e],
		TestID -> "Spab, Spba: (B: Massless Spinor, A: LVector): inside"
	];
	TestUnchanged[
		Spxy[c, d, LvBA[b, Q]],
		TestID -> "Spab, Spba: (B: Massless Spinor, A: LVector): right"
	];
	
	TestUnchanged[
		Spxy[LvBA[P, a], d, e],
		TestID -> "Spab, Spba: (B: LVector, A: Massless Spinor): left"
	];
	TestUnchanged[
		Spxy[c, LvBA[P, a], e],
		TestID -> "Spab, Spba: (B: LVector, A: Massless Spinor): inside"
	];
	TestUnchanged[
		Spxy[c, d, LvBA[P, a]],
		TestID -> "Spab, Spba: (B: LVector, A: Massless Spinor): right"
	];
	,
	TestID -> "Spab, Spba: LVectors test case",
	RepeatFor -> {Spxy -> Spab, Spxy -> Spba}
];


TestCaseRepeated[
	Test[
		Spab[LvBA[b, a], x, y],
		Spab[a, x, y],
		TestID -> "Spab: (B: Massless Spinor, A: Massless Spinor): left"
	];
	Test[
		Spba[LvBA[b, a], x, y],
		Spba[b, x, y],
		TestID -> "Spba: (B: Massless Spinor, A: Massless Spinor): left"
	];
	
	Test[
		Spab[x, LvBA[b, a], y],
		Spaa[x, a] Spbb[b, y] + Spab[x, b] Spab[a, y],
		TestID -> "Spab: (B: Massless Spinor, A: Massless Spinor): inside"
	];
	Test[
		Spba[x, LvBA[b, a], y],
		Spbb[x, b] Spaa[a, y] + Spba[x, a] Spba[b, y],
		TestID -> "Spba: (B: Massless Spinor, A: Massless Spinor): inside"
	];
	
	Test[
		Spab[x, y, LvBA[b, a]],
		Spab[x, y, b],
		TestID -> "Spab: (B: Massless Spinor, A: Massless Spinor): right"
	];
	Test[
		Spba[x, y, LvBA[b, a]],
		Spba[x, y, a],
		TestID -> "Spba: (B: Massless Spinor, A: Massless Spinor): right"
	];
	
	Test[
		Spab[LvBA[SpM[P, pm1], a], x, y],
		Spab[a, x, y],
		TestID -> "Spab: (B: Massive Spinor, A: Massless Spinor): left"
	];
	Test[
		Spba[LvBA[SpM[P, pm1], a], x, y],
		Spba[SpAssoc[P], x, y],
		TestID -> "Spba: (B: Massive Spinor, A: Massless Spinor): left"
	];
	
	Test[
		Spab[x, LvBA[SpM[P, pm1], a], y],
		Spaa[x, a] Spbb[SpAssoc[P], y] + Spab[x, SpAssoc[P]] Spab[a, y],
		TestID -> "Spab: (B: Massive Spinor, A: Massless Spinor): inside"
	];
	Test[
		Spba[x, LvBA[SpM[P, pm1], a], y],
		Spbb[x, SpAssoc[P]] Spaa[a, y] + Spba[x, a] Spba[SpAssoc[P], y],
		TestID -> "Spba: (B: Massive Spinor, A: Massless Spinor): inside"
	];
	
	Test[
		Spab[x, y, LvBA[SpM[P, pm1], a]],
		Spab[x, y, SpAssoc[P]],
		TestID -> "Spab: (B: Massive Spinor, A: Massless Spinor): right"
	];
	Test[
		Spba[x, y, LvBA[SpM[P, pm1], a]],
		Spba[x, y, a],
		TestID -> "Spba: (B: Massive Spinor, A: Massless Spinor): right"
	];
	
	Test[
		Spab[LvBA[b, SpM[Q, pm2]], x, y],
		Spab[SpAssoc[Q], x, y],
		TestID -> "Spab: (B: Massless Spinor, A: Massive Spinor): left"
	];
	Test[
		Spba[LvBA[b, SpM[Q, pm2]], x, y],
		Spba[b, x, y],
		TestID -> "Spba: (B: Massless Spinor, A: Massive Spinor): left"
	];
	
	Test[
		Spab[x, LvBA[b, SpM[Q, pm2]], y],
		Spaa[x, SpAssoc[Q]] Spbb[b, y] + Spab[x, b] Spab[SpAssoc[Q], y],
		TestID -> "Spab: (B: Massless Spinor, A: Massive Spinor): inside"
	];
	Test[
		Spba[x, LvBA[b, SpM[Q, pm2]], y],
		Spbb[x, b] Spaa[SpAssoc[Q], y] + Spba[x, SpAssoc[Q]] Spba[b, y],
		TestID -> "Spba: (B: Massless Spinor, A: Massive Spinor): inside"
	];
	
	Test[
		Spab[x, y, LvBA[b, SpM[Q, pm2]]],
		Spab[x, y, b],
		TestID -> "Spab: (B: Massless Spinor, A: Massive Spinor): right"
	];
	Test[
		Spba[x, y, LvBA[b, SpM[Q, pm2]]],
		Spba[x, y, SpAssoc[Q]],
		TestID -> "Spba: (B: Massless Spinor, A: Massive Spinor): right"
	];
	
	
	TestCaseRepeated[
		TestUnchanged[
			Spxy[LvBA[SpM[P, pm1], SpM[Q, pm2]], x, y],
			TestID ->
				"Spab, Spba: (B: Massive Spinor, A: Massive Spinor): left"
		];
		TestUnchanged[
			Spxy[x, LvBA[SpM[P, pm1], SpM[Q, pm2]], y],
			TestID ->
				"Spab, Spba: (B: Massive Spinor, A: Massive Spinor): inside"
		];
		TestUnchanged[
			Spxy[x, y, LvBA[SpM[P, pm1], SpM[Q, pm2]]],
			TestID ->
				"Spab, Spba: (B: Massive Spinor, A: Massive Spinor): right"
		];
		,
		TestID -> "Spab, Spba: Massive Spinors test case",
		RepeatFor -> {Spxy -> Spab, Spxy -> Spba}
	];
	,
	TestID -> "Spab, Spba: Spinors test case"
];


(* ::Subsection:: *)
(*Spaa, Spbb*)


TestCaseRepeated[
	TestUnchanged[
		Spxx[LvBA[P, Q], c, d, e],
		TestID -> "Spaa, Spbb: LVector, LVector): left"
	];
	TestUnchanged[
		Spxx[c, LvBA[P, Q], d, e],
		TestID -> "Spaa, Spbb: LVector, LVector): inside"
	];
	TestUnchanged[
		Spxx[c, d, e, LvBA[P, Q]],
		TestID -> "Spaa, Spbb: LVector, LVector): right"
	];
	
	
	TestUnchanged[
		Spxx[LvBA[b, Q], c, d, e],
		TestID -> "Spaa, Spbb: (B: Massless Spinor, LVector): left"
	];
	TestUnchanged[
		Spxx[c, LvBA[b, Q], d, e],
		TestID -> "Spaa, Spbb: (B: Massless Spinor, LVector): inside"
	];
	TestUnchanged[
		Spxx[c, d, e, LvBA[b, Q]],
		TestID -> "Spaa, Spbb: (B: Massless Spinor, LVector): right"
	];
	
	
	TestUnchanged[
		Spxx[LvBA[P, a], c, d, e],
		TestID -> "Spaa, Spbb: LVector, A: Massless Spinor): left"
	];
	TestUnchanged[
		Spxx[c, LvBA[P, a], d, e],
		TestID -> "Spaa, Spbb: LVector, A: Massless Spinor): inside"
	];
	TestUnchanged[
		Spxx[c, d, e, LvBA[P, a]],
		TestID -> "Spaa, Spbb: LVector, A: Massless Spinor): right"
	];
	,
	TestID -> "Spaa, Spbb: LVectors test case",
	RepeatFor -> {Spxx -> Spaa, Spxx -> Spbb}
];


TestCaseRepeated[
	Test[
		Spaa[LvBA[b, a], x, y, z],
		Spaa[a, x, y, z],
		TestID -> "Spaa: (B: Massless Spinor, A: Massless Spinor): left"
	];
	Test[
		Spbb[LvBA[b, a], x, y, z],
		Spbb[b, x, y, z],
		TestID -> "Spbb: (B: Massless Spinor, A: Massless Spinor): left"
	];
	
	Test[
		Spaa[x, LvBA[b, a], y, z],
		Spaa[x, a] Spba[b, y, z] + Spab[x, b] Spaa[a, y, z],
		TestID -> "Spaa: (B: Massless Spinor, A: Massless Spinor): inside"
	];
	Test[
		Spbb[x, LvBA[b, a], y, z],
		Spbb[x, b] Spab[a, y, z] + Spba[x, a] Spbb[b, y, z],
		TestID -> "Spbb: (B: Massless Spinor, A: Massless Spinor): inside"
	];
	
	Test[
		Spaa[x, y, z, LvBA[b, a]],
		Spaa[x, y, z, a],
		TestID -> "Spaa: (B: Massless Spinor, A: Massless Spinor): right"
	];
	Test[
		Spbb[x, y, z, LvBA[b, a]],
		Spbb[x, y, z, b],
		TestID -> "Spbb: (B: Massless Spinor, A: Massless Spinor): right"
	];
	
	Test[
		Spaa[LvBA[SpM[P, pm1], a], x, y, z],
		Spaa[a, x, y, z],
		TestID -> "Spaa: (B: Massive Spinor, A: Massless Spinor): left"
	];
	Test[
		Spbb[LvBA[SpM[P, pm1], a], x, y, z],
		Spbb[SpAssoc[P], x, y, z],
		TestID -> "Spbb: (B: Massive Spinor, A: Massless Spinor): left"
	];
	
	Test[
		Spaa[x, LvBA[SpM[P, pm1], a], y, z],
		Spaa[x, a] Spba[SpAssoc[P], y, z] + Spab[x, SpAssoc[P]] Spaa[a, y, z],
		TestID -> "Spaa: (B: Massive Spinor, A: Massless Spinor): inside"
	];
	Test[
		Spbb[x, LvBA[SpM[P, pm1], a], y, z],
		Spbb[x, SpAssoc[P]] Spab[a, y, z] + Spba[x, a] Spbb[SpAssoc[P], y, z],
		TestID -> "Spbb: (B: Massive Spinor, A: Massless Spinor): inside"
	];
	
	Test[
		Spaa[x, y, z, LvBA[SpM[P, pm1], a]],
		Spaa[x, y, z, a],
		TestID -> "Spaa: (B: Massive Spinor, A: Massless Spinor): right"
	];
	Test[
		Spbb[x, y, z, LvBA[SpM[P, pm1], a]],
		Spbb[x, y, z, SpAssoc[P]],
		TestID -> "Spbb: (B: Massive Spinor, A: Massless Spinor): right"
	];
	
	Test[
		Spaa[LvBA[b, SpM[Q, pm2]], x, y, z],
		Spaa[SpAssoc[Q], x, y, z],
		TestID -> "Spaa: (B: Massless Spinor, A: Massive Spinor): left"
	];
	Test[
		Spbb[LvBA[B b, SpM[Q, pm2]], x, y, z],
		B Spbb[b, x, y, z],
		TestID -> "Spbb: (B: Massless Spinor, A: Massive Spinor): left"
	];
	
	Test[
		Spaa[x, LvBA[b, SpM[Q, pm2]], y, z],
		Spaa[x, SpAssoc[Q]] Spba[b, y, z] + Spab[x, b] Spaa[SpAssoc[Q], y, z],
		TestID -> "Spaa: (B: Massless Spinor, A: Massive Spinor): inside"
	];
	Test[
		Spbb[x, LvBA[b, SpM[Q, pm2]], y, z],
		Spbb[x, b] Spab[SpAssoc[Q], y, z] + Spba[x, SpAssoc[Q]] Spbb[b, y, z],
		TestID -> "Spbb: (B: Massless Spinor, A: Massive Spinor): inside"
	];
	
	Test[
		Spaa[x, y, z, LvBA[b, SpM[Q, pm2]]],
		Spaa[x, y, z, SpAssoc[Q]],
		TestID -> "Spaa: (B: Massless Spinor, A: Massive Spinor): right"
	];
	Test[
		Spbb[x, y, z, LvBA[b, SpM[Q, pm2]]],
		Spbb[x, y, z, b],
		TestID -> "Spbb: (B: Massless Spinor, A: Massive Spinor): right"
	];
	
	
	TestCaseRepeated[
		TestUnchanged[
			Spxx[LvBA[SpM[P, pm1], SpM[Q, pm2]], x, y, z],
			TestID ->
				"Spaa, Spbb: (B: Massive Spinor, A: Massive Spinor): left"
		];
		TestUnchanged[
			Spxx[x, LvBA[SpM[P, pm1], SpM[Q, pm2]], y, z],
			TestID ->
				"Spaa, Spbb: (B: Massive Spinor, A: Massive Spinor): inside"
		];
		TestUnchanged[
			Spxx[x, y, z, LvBA[SpM[P, pm1], SpM[Q, pm2]]],
			TestID ->
				"Spaa, Spbb: (B: Massive Spinor, A: Massive Spinor): right"
		];
		,
		TestID -> "Spaa, Spbb: Massive Spinors test case",
		RepeatFor -> {Spxx -> Spaa, Spxx -> Spbb}
	];
	,
	TestID -> "Spaa, Spbb: Spinors test case"
];


(* ::Subsection:: *)
(*2-dim and 4-dim representation of spinors*)


(* ::Subsubsection:: *)
(*A spinors*)


TestCaseRepeated[
	Test[
		SpA[LvBA[b, a]],
		SpA[a],
		TestID -> "2,4-dim A Spinor: (B: Massless Spinor, A: Massless Spinor)"
	];
	
	Test[
		SpA[LvBA[SpM[P, pm1], a]],
		SpA[a],
		TestID -> "2,4-dim A Spinor: (B: Massive Spinor, A: Massless Spinor)"
	];
	
	Test[
		SpA[LvBA[b, SpM[Q, pm2]]],
		SpA[SpAssoc[Q]],
		TestID -> "2,4-dim A Spinor: (B: Massless Spinor, A: Massless Spinor)"
	];
	
	TestUnchanged[
		SpA[LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		TestID -> "2,4-dim A Spinor: (B: Massive Spinor, A: Massive Spinor)"
	];
	,
	TestID -> "2,4-dim A Spinor test case: scaled Spinors",
	RepeatFor ->
		Flatten /@
			Tuples[{SpA -> # & /@ {La, CLa, USpa, UbarSpa}, BAScaleRules}]
];


(* ::Subsubsection:: *)
(*B spinors*)


TestCaseRepeated[
	Test[
		SpB[LvBA[b, a]],
		SpB[b],
		TestID -> "2,4-dim B Spinor: (B: Massless Spinor, A: Massless Spinor)"
	];
	
	Test[
		SpB[LvBA[SpM[P, pm1], a]],
		SpB[SpAssoc[P]],
		TestID -> "2,4-dim B Spinor: (B: Massive Spinor, A: Massless Spinor)"
	];
	
	Test[
		SpB[LvBA[b, SpM[Q, pm2]]],
		SpB[b],
		TestID -> "2,4-dim B Spinor: (B: Massless Spinor, A: Massive Spinor)"
	];
	
	TestUnchanged[
		SpB[LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		TestID -> "2,4-dim B Spinor: (B: Massive Spinor, A: Massive Spinor)"
	];
	,
	TestID -> "2,4-dim B Spinor test case",
	RepeatFor ->
		Flatten /@
			Tuples[{SpB -> # & /@ {Lat, CLat, USpb, UbarSpb}, BAScaleRules}]
];


(* ::Subsection:: *)
(*s invariant*)


TestCaseRepeated[
	Test[
		s[x, LvBA[b, a]],
		s[x] + Spab[a, x, b],
		TestID -> "s: 2 args: (B: Massless Spinor, A: Massless Spinor)"
	];
	Test[
		s[x, LvBA[SpM[P, pm1], a]],
		s[x] + Spab[a, x, SpAssoc[P]],
		TestID -> "s: 2 args: (B: Massive Spinor, A: Massless Spinor)"
	];
	Test[
		s[x, LvBA[b, SpM[Q, pm2]]],
		s[x] + Spab[SpAssoc[Q], x, b],
		TestID -> "s: 2 args: (B: Massless Spinor, A: Massive Spinor)"
	];
	Test[
		s[x, LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		s[x] + Spab[SpM[Q, pm2], x, SpM[P, pm1]],
		TestID -> "s: 2 args: (B: Massive Spinor, A: Massive Spinor)"
	];
	
	
	Test[
		s[x, y, LvBA[b, a], z],
		s[x, y, z] + Spab[a, x, b] + Spab[a, y, b] + Spab[a, z, b],
		TestID -> "s: 4 args: (B: Massless Spinor, A: Massless Spinor)"
	];
	Test[
		s[x, y, LvBA[SpM[P, pm1], a], z]
		,
		s[x, y, z] +
			Spab[a, x, SpAssoc[P]] +
			Spab[a, y, SpAssoc[P]] +
			Spab[a, z, SpAssoc[P]]
		,
		TestID -> "s: 4 args: (B: Massive Spinor, A: Massless Spinor)"
	];
	Test[
		s[x, y, LvBA[b, SpM[Q, pm2]], z]
		,
		s[x, y, z] +
			Spab[SpAssoc[Q], x, b] +
			Spab[SpAssoc[Q], y, b] +
			Spab[SpAssoc[Q], z, b]
		,
		TestID -> "s: 4 args: (B: Massless Spinor, A: Massive Spinor)"
	];
	Test[
		s[x, y, LvBA[SpM[P, pm1], SpM[Q, pm2]], z]
		,
		s[x, y, z] +
			Spab[SpM[Q, pm2], x, SpM[P, pm1]] +
			Spab[SpM[Q, pm2], y, SpM[P, pm1]] +
			Spab[SpM[Q, pm2], z, SpM[P, pm1]]
		,
		TestID -> "s: 4 args: (B: Massive Spinor, A: Massive Spinor)"
	];
	,
	TestID -> "s test case"
];


(* ::Subsection:: *)
(*MP*)


TestCaseRepeated[
	Test[
		MP[LvBA[b, a], x],
		1/2 Spab[a, x, b],
		TestID -> "MP: (B: Massless Spinor, A: Massless Spinor): left"
	];
	Test[
		MP[LvBA[SpM[P, pm1], a], x],
		1/2 Spab[a, x, SpAssoc[P]],
		TestID -> "MP: (B: Massive Spinor, A: Massless Spinor): left"
	];
	Test[
		MP[LvBA[b, SpM[Q, pm2]], x],
		1/2 Spab[SpAssoc[Q], x, b],
		TestID -> "MP: (B: Massless Spinor, A: Massive Spinor): left"
	];
	Test[
		MP[LvBA[SpM[P, pm1], SpM[Q, pm2]], x],
		1/2 Spab[SpM[Q, pm2], x, SpM[P, pm1]],
		TestID -> "MP: (B: Massive Spinor, A: Massive Spinor): left"
	];
	
	Test[
		MP[x, LvBA[b, a]],
		1/2 Spab[a, x, b],
		TestID -> "MP: (B: Massless Spinor, A: Massless Spinor): right"
	];
	Test[
		MP[x, LvBA[SpM[P, pm1], a]],
		1/2 Spab[a, x, SpAssoc[P]],
		TestID -> "MP: (B: Massive Spinor, A: Massless Spinor): right"
	];
	Test[
		MP[x, LvBA[b, SpM[Q, pm2]]],
		1/2 Spab[SpAssoc[Q], x, b],
		TestID -> "MP: (B: Massless Spinor, A: Massive Spinor): right"
	];
	Test[
		MP[x, LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		1/2 Spab[SpM[Q, pm2], x, SpM[P, pm1]],
		TestID -> "MP: (B: Massive Spinor, A: Massive Spinor): right"
	];
	
	Test[
		MP[LvBA[b, a], LvBA[b2, a2]],
		1/2 Spaa[a, a2] Spbb[b2, b],
		TestID -> "MP: (B: Massless Spinor, A: Massless Spinor): both"
	];
	Test[
		MP[LvBA[SpM[P, pm1], a], LvBA[SpM[L, pm3], a2]],
		1/2 Spaa[a2, a] Spbb[SpAssoc[P], SpAssoc[L]],
		TestID -> "MP: (B: Massive Spinor, A: Massless Spinor): both"
	];
	Test[
		MP[LvBA[b, SpM[Q, pm2]], LvBA[b2, SpM[R, pm4]]],
		1/2 Spaa[SpAssoc[Q], SpAssoc[R]] Spbb[b2, b],
		TestID -> "MP: (B: Massless Spinor, A: Massive Spinor): both"
	];
	Test[
		MP[LvBA[SpM[P, pm1], SpM[Q, pm2]], LvBA[SpM[L, pm3], SpM[R, pm4]]],
		1/2 Spab[SpM[R, pm4], LvBA[SpM[P, pm1], SpM[Q, pm2]], SpM[L, pm3]],
		TestID -> "MP: (B: Massive Spinor, A: Massive Spinor): both"
	];
	,
	TestID -> "MP test case"
];


(* ::Subsection:: *)
(*Sm*)


TestCaseRepeated[
	Test[
		SmN[LvBA[b, a]],
		SmBAN[b, a],
		TestID -> "Sm: (B: Massless Spinor, A: Massless Spinor)"
	];
	Test[
		SmN[LvBA[SpM[P, pm1], a]],
		SmBAN[SpAssoc[P], a],
		TestID -> "Sm: (B: Massive Spinor, A: Massless Spinor)"
	];
	Test[
		SmN[LvBA[b, SpM[Q, pm2]]],
		SmBAN[b, SpAssoc[Q]],
		TestID -> "Sm: (B: Massless Spinor, A: Massive Spinor)"
	];
	
	TestUnchanged[
		SmN[LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		TestID -> "Sm: (B: Massive Spinor, A: Massive Spinor)"
	];
	,
	TestID -> "Sm test case",
	RepeatFor ->
		Flatten /@
			Tuples @ {
				Transpose @ {
					SmN -> # & /@ {Sm, Sm2, CSm2, Sm4},
					SmBAN -> # & /@ {SmBA, SmBA2, CSmBA2, SmBA4}
				}
				,
				BAScaleRules
			}
];


(* ::Subsection:: *)
(*SmBA*)


TestCaseRepeated[
	Test[
		SmBAN[LvBA[b, a], y],
		SmBAN[b, y],
		TestID -> "SmBA: (B: Massless Spinor, A: Massless Spinor): B"
	];
	Test[
		SmBAN[LvBA[SpM[P, pm1], a], y],
		SmBAN[SpAssoc[P], y],
		TestID -> "SmBA: (B: Massive Spinor, A: Massless Spinor): B"
	];
	Test[
		SmBAN[LvBA[b, SpM[Q, pm2]], y],
		SmBAN[b, y],
		TestID -> "SmBA: (B: Massless Spinor, A: Massive Spinor): B"
	];
	TestUnchanged[
		SmBAN[LvBA[SpM[P, pm1], SpM[Q, pm2]], y],
		TestID -> "SmBA: (B: Massive Spinor, A: Massive Spinor): B"
	];
	
	Test[
		SmBAN[x, LvBA[b, a]],
		SmBAN[x, a],
		TestID -> "SmBA: (B: Massless Spinor, A: Massless Spinor): A"
	];
	Test[
		SmBAN[x, LvBA[SpM[P, pm1], a]],
		SmBAN[x, a],
		TestID -> "SmBA: (B: Massive Spinor, A: Massless Spinor): A"
	];
	Test[
		SmBAN[x, LvBA[b, SpM[Q, pm2]]],
		SmBAN[x, SpAssoc[Q]],
		TestID -> "SmBA: (B: Massless Spinor, A: Massive Spinor): A"
	];
	TestUnchanged[
		SmBAN[x, LvBA[SpM[P, pm1], SpM[Q, pm2]]],
		TestID -> "SmBA: (B: Massive Spinor, A: Massive Spinor): A"
	];
	
	Test[
		SmBAN[LvBA[b, a], LvBA[b2, a2]],
		SmBAN[b, a2],
		TestID -> "SmBA: (B: Massless Spinor, A: Massless Spinor): B and A"
	];
	Test[
		SmBAN[LvBA[SpM[P, pm1], a], LvBA[SpM[L, pm3], a2]],
		SmBAN[SpAssoc[P], a2],
		TestID -> "SmBA: (B: Massive Spinor, A: Massless Spinor): B and A"
	];
	Test[
		SmBAN[LvBA[b, SpM[Q, pm2]], LvBA[b2, SpM[R, pm4]]],
		SmBAN[b, SpAssoc[R]],
		TestID -> "SmBA: (B: Massless Spinor, A: Massive Spinor): B and A"
	];
	TestUnchanged[
		SmBAN[LvBA[SpM[P, pm1], SpM[Q, pm2]], LvBA[SpM[L, pm3], SpM[R, pm4]]],
		TestID -> "SmBA: (B: Massive Spinor, A: Massive Spinor): B and A"
	];
	,
	TestID -> "SmBA test case",
	RepeatFor ->
		Flatten /@
			Tuples @ {
				SmBAN -> # & /@ {SmBA, SmBA2, CSmBA2, SmBA4},
				BAScaleRules
			}
];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
