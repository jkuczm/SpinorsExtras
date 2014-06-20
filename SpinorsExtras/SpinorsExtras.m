BeginPackage["SpinorsExtras`", {
	"Spinors`",
	"SpinorsExtras`Utilities`",
	"SpinorsExtras`Numerics`",
	"SpinorsExtras`Ref`",
	"SpinorsExtras`Massive`",
	"SpinorsExtras`MassiveUtilities`",
	"SpinorsExtras`Composite`",
	"SpinorsExtras`Phase`",
	"SpinorsExtras`Pol`",
	"SpinorsExtras`RefUtilities`",
	"SpinorsExtras`Proportional`",
	"SpinorsExtras`Decompose`",
	"SpinorsExtras`SimpleTensor`",
	"SpinorsExtras`SatMmodifications`",
	"SpinorsExtras`Notation`"
}]


Begin["`Private`"]


With[
	{pacletInfoRules = List @@ (<<SpinorsExtras`PacletInfo`)}
	,
	Print["
-------  ", (Name /. pacletInfoRules), "  -------

Version:\t", (Version /. pacletInfoRules), " (2014.06.18)
Author:\t",
	Hyperlink[
		Creator /. pacletInfoRules,
		"mailto:Jakub.Kuczmarski@fuw.edu.pl"
	], "
Documentation:\t",
	Hyperlink["Documentation Center", "paclet:SpinorsExtras"],
	"\t",
	Hyperlink[
		"Online version",
		"http://www.fuw.edu.pl/~jkuczm/SpinorsExtras/reference/guide/SpinorsExtras.html"
	], "
"
	]
];


End[]


EndPackage[]
