(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


<<"SpinorsExtras/Tests/init.mt";


BeginSpinorsTestEnvironment[{"SpinorsExtras`Package`"}];


SetOptions[{TestTrue, TestFalse}, InputWrapper -> MyFavoriteQ];


(* ::Section:: *)
(*Tests*)


TestNull[
	DeclareUndeclareQFactory["MyFavorite", {a, b}, "my favorite symbols"],
	TestID-> "DeclareUndeclareQFactory call returns Null"
];

Protect[MyFavoriteQ, DeclareMyFavorite, UndeclareMyFavorite];

TestTrue[a, TestID -> "MyFavoriteQ before Declare: a"];
TestTrue[b, TestID -> "MyFavoriteQ before Declare: b"];
TestFalse[c, TestID -> "MyFavoriteQ before Declare: c"];
TestFalse[d, TestID -> "MyFavoriteQ before Declare: d"];


TestNull[
	DeclareMyFavorite[c, d],
	TestID-> "DeclareMyFavorite call returns Null"
];

TestTrue[a, TestID -> "MyFavoriteQ after Declare: a"];
TestTrue[b, TestID -> "MyFavoriteQ after Declare: b"];
TestTrue[c, TestID -> "MyFavoriteQ after Declare: c"];
TestTrue[d, TestID -> "MyFavoriteQ after Declare: d"];

TestNull[
	UndeclareMyFavorite[a, c, e],
	TestID-> "UndeclareMyFavorite call returns Null"
];

TestFalse[a, TestID -> "MyFavoriteQ after Undeclare: a"];
TestTrue[b, TestID -> "MyFavoriteQ after Undeclare: b"];
TestFalse[c, TestID -> "MyFavoriteQ after Undeclare: c"];
TestTrue[d, TestID -> "MyFavoriteQ after Undeclare: d"];


(* ::Section:: *)
(*TearDown*)


EndSpinorsTestEnvironment[];
