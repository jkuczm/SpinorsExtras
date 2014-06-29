(* ::Package:: *)

Get["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.0/BootstrapInstaller.m"]


BootstrapInstall[
	"SpinorsExtras",
	"https://github.com/jkuczm/SpinorsExtras/releases/download/v1.0.1/SpinorsExtras.zip"
	,
	{
		#1
		,
		"https://github.com/jkuczm/" <> #2 <>
		"/releases/download/v" <> #3 <> "/" <> #1 <> ".zip"
	}& @@@ {
		{"EvaluationUtilities", "MathematicaEvaluationUtilities", "0.1.0"},
		{"MessagesUtilities", "MathematicaMessagesUtilities", "0.1.0"},
		{"OptionsUtilities", "MathematicaOptionsUtilities", "0.1.0"},
		{"PatternUtilities", "MathematicaPatternUtilities", "0.1.0"},
		{"ProtectionUtilities", "MathematicaProtectionUtilities", "0.1.0"},
		{"StringUtilities", "MathematicaStringUtilities", "0.1.0"},
		{"MUnitExtras", "MUnitExtras", "0.1.1"}
	}
	,
	"AdditionalFailureMessage" -> 
		Sequence[
			"You can ", 
			Hyperlink[
				"install SpinorsExtras package manually", 
				"https://github.com/jkuczm/SpinorsExtras#manual-installation"
			],
			"."
		]
]
