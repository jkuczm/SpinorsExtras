(* ::Package:: *)

Module[
	{$PIImportResult}
	,
	
	Quiet[
		$PIImportResult = Needs["ProjectInstaller`"],
		{Get::noopen, Needs::nocont}
	];
	
	If[$PIImportResult === $Failed,
		Print["ProjectInstaller not found, installing it:"];
		Print @ Import[
			"https://raw.github.com/lshifr/ProjectInstaller/master/BootstrapInstall.m"
		];
		$PIImportResult = Needs["ProjectInstaller`"];
	];

	If[$PIImportResult === $Failed,
		Print[
			"Unable to load ProjectInstaller.\n",
			"Please ",
			Hyperlink[
				"install SpinorsExtras package manually",
				"https://github.com/jkuczm/SpinorsExtras#manual-installation"
			],
			".\n",
			"We would be grateful for ",
			Hyperlink[
				"reporting this issue",
				"https://github.com/jkuczm/SpinorsExtras/issues"
			],
			"."
		];
	(* else *),
		Print["Installing dependencies:"];
		(Print @ ProjectInstaller`ProjectInstall @ URL[
			"https://github.com/jkuczm/" <> #2 <>
			"/releases/download/v" <> #3 <> "/" <> #1 <> ".zip"
		])& @@@ {
			{"EvaluationUtilities", "MathematicaEvaluationUtilities", "0.1.0"},
			{"MessagesUtilities", "MathematicaMessagesUtilities", "0.1.0"},
			{"OptionsUtilities", "MathematicaOptionsUtilities", "0.1.0"},
			{"PatternUtilities", "MathematicaPatternUtilities", "0.1.0"},
			{"ProtectionUtilities", "MathematicaProtectionUtilities", "0.1.0"},
			{"StringUtilities", "MathematicaStringUtilities", "0.1.0"},
			{"MUnitExtras", "MUnitExtras", "0.1.1"}
		};
		
		Print["Installing SpinorsExtras:"];
		Print @ ProjectInstaller`ProjectInstall @ URL[
			"https://github.com/jkuczm/SpinorsExtras/releases/download/v1.0.1/SpinorsExtras.zip"
		];
	];
]
