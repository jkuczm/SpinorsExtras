(* ::Package:: *)

BeginPackage["SpinorsExtras`Package`", {"Spinors`"}]


(* ::Section:: *)
(*Usage messages*)


SpinorizeInteger::usage =
"\
SpinorizeInteger[x] \
for integer x returns Sp[x] otherwise returns x."


SpinorizeIntegerList::usage =
"\
SpinorizeIntegerList[list] \
returns list with all occurrences of integers replaced by Sp[i].\

SpinorizeIntegerList[a, b, c, ...] \
returns list {a, b, c} with all occurrences of integers replaced by Sp[i]."


SpinorInterpretableQ::usage =
"\
SpinorInterpretableQ[x] \
returns True if x can be interpreted as Spinor, i.e. was declared as Spinor \
or is an Integer. Returns False otherwise."


LVectorInterpretableQ::usage =
"\
LVectorInterpretableQ[x] \
returns True if x can be interpreted as LVector, i.e. was declared as Spinor, \
LVector or is an Integer. Returns False otherwise."


ScalarQ::usage =
"\
ScalarQ[x] \
returns False if x contain bare Spinor, LVector or SMatrix label i.e. label \
that is not inside some scalar quantity: Spxy, MP, MP2, s."


ScaledQFactory::usage =
"\
ScaledQFactory[\"name\", patt] \
implements Scaled<name>Q that tests whether given expression matches pattern \
patt or patt multiplied by scalars."


ScaledSpinorQ::usage =
"\
ScaledSpinorQ[x] \
returns True if x is a massless Spinor or a massless Spinor times scalar \
coefficient."


ScaledLVectorQ::usage =
"\
ScaledLVectorQ[x] \
returns True if x is a LVector or a LVector times scalar coefficient."


GetScalarCoefficient::usage =
"\
GetScalarCoefficient[x] \
for x being scaled LVector returns scalar coefficient multiplying LVector."


ChangeSecondToA::usage =
"\
ChangeSecondToA[Spxy] \
with x, y being a or b, returns spinor chain symbol with y converted to a: \
Spxa."


ChangeSecondToB::usage =
"\
ChangeSecondToB[Spxy] \
with x, y being a or b, returns spinor chain symbol with y converted to b: \
Spxb."


ChangeFirstToA::usage =
"\
ChangeFirstToA[Spxy] \
with x, y being a or b, returns spinor chain symbol with x converted to a: \
Spay."


ChangeFirstToB::usage =
"\
ChangeFirstToB[Spxy] \
with x, y being a or b, returns spinor chain symbol with x converted to b: \
Spby."


ChangeSmToSmBA::usage =
"\
ChangeSmToSmBA[sm] \
with sm being Sm, Sm4, Sm2, CSm2, returns sm symbol converted to: \
SmBA, SmBA4, SmBA2, CSmBA2 respectively."


SmHeadPattern::usage =
"\
SmPattern \
matches head of slashed matrix in any representation: Sm, Sm2, CSm2, Sm4."


SmBAHeadPattern::usage =
"\
SmPattern \
matches head of BA slashed matrix in any representation: \
SmBA, SmBA2, CSmBA2, SmBA4."


BSpHeadPattern::usage =
"\
BSpHeadPattern \
matches head of B spinor in 2-dim and 4-dim representations: \
Lat, CLat, USpb, UbarSpb."


ASpHeadPattern::usage =
"\
ASpHeadPattern \
matches head of A spinor in 2-dim and 4-dim representations: \
La, CLa, USpa, UbarSpa."


QuietSpinorPrint::usage =
"\
QuietSpinorPrint[expr] \
evaluates expr with turned off printing from Spinors` package."


DeclareUndeclareQFactory::usage =
"\
DeclareUndeclareQFactory[patternList, name, text] \
implements Declare<name>, Undeclare<name>, <name>Q functions that use \
patternList as container for declared symbols and after successful \
(un)declaration prints info ending with text."


(* ::Section:: *)
(*Implementation*)


(* Unprotect all public symbols in this context. *)
Unprotect["`*"];


Begin["`Private`"]


(* ::Subsection:: *)
(*General messages*)


General::wrongBasis =
"Spinors labeled by `1` and `2` are proportional, so they don't form a basis \
of spinor space."


General::nonSpinor =
"Arguments at positions: `1`, in `2` should be spinors."


General::notEnoughSpinors =
"`1` called with `2` spinors; at least `3` independent spinors must be given."


General::firstDeclare =
"Declaration of default value for `1` is possible after declaring values for \
`2`."


(* ::Subsection:: *)
(*Imports*)


Needs["ProtectionUtilities`"] (* ProtectContextNonVariables *)


PrependTo[$ContextPath, "Spinors`Private`"]


(* ::Subsection:: *)
(*SpinorizeInteger*)


SpinorizeInteger[x_] := Replace[x, _Integer :> Sp[x]]


(* ::Subsection:: *)
(*SpinorizeIntegerList*)


SpinorizeIntegerList[l_List] := Replace[l, i_Integer :> Sp[i], {1}]


SpinorizeIntegerList[args___] := SpinorizeIntegerList[{args}]


(* ::Subsection:: *)
(*SpinorInterpretableQ*)


SpinorInterpretableQ[_Integer] := True


SpinorInterpretableQ[x_] := SpinorQ[x]


(* ::Subsection:: *)
(*LVectorInterpretableQ*)


LVectorInterpretableQ[_Integer] := True


LVectorInterpretableQ[x_] := LVectorQ[x]


(* ::Subsection:: *)
(*ScalarQ*)


ScalarPattern = _Spaa | _Spbb | _Spab | _Spba | _s | _MP | _MP2


(*
	Replace each occurrence of scalar quantity with unique symbol to avoid
	accidental reduction of coefficients next to non-scalar.
*)
ScalarQ[x_] := FreeQ[x //. ScalarPattern :> Unique[], _?SMatrixQ]


(* ::Subsection:: *)
(*ScaledQFactory*)


ScaledQFactory[name_String, patt_] :=
	Module[
		{qFunction = Symbol["Scaled" <> name <> "Q"]}
		,
		qFunction[x_] := MatchQ[x, patt | (__?ScalarQ patt)]
	]


(* ::Subsection:: *)
(*ScaledSpinorQ*)


ScaledQFactory["Spinor", _?SpinorQ]


(* ::Subsection:: *)
(*ScaledLVectorQ*)


ScaledQFactory["LVector", _?LVectorQ]


(* ::Subsection:: *)
(*GetScalarCoefficient*)


GetScalarCoefficient::notScaledLVector =
"GetScalarCoefficient called with `1` argument which is not a scaled LVector."


GetScalarCoefficient[_?LVectorQ] = 1


GetScalarCoefficient[x__?ScalarQ _?LVectorQ] := Times[x]


GetScalarCoefficient[x_] := (
	Message[GetScalarCoefficient::notScaledLVector, x];
	$Failed
)


(* ::Subsection:: *)
(*ChangeSecondToA*)


ChangeSecondToA[Spaa] = Spaa
ChangeSecondToA[Spab] = Spaa
ChangeSecondToA[Spba] = Spba
ChangeSecondToA[Spbb] = Spba


(* ::Subsection:: *)
(*ChangeSecondToB*)


ChangeSecondToB[Spaa] = Spab
ChangeSecondToB[Spab] = Spab
ChangeSecondToB[Spba] = Spbb
ChangeSecondToB[Spbb] = Spbb


(* ::Subsection:: *)
(*ChangeFirstToA*)


ChangeFirstToA[Spaa] = Spaa
ChangeFirstToA[Spba] = Spaa
ChangeFirstToA[Spab] = Spab
ChangeFirstToA[Spbb] = Spab


(* ::Subsection:: *)
(*ChangeFirstToB*)


ChangeFirstToB[Spaa] = Spba
ChangeFirstToB[Spba] = Spba
ChangeFirstToB[Spab] = Spbb
ChangeFirstToB[Spbb] = Spbb


(* ::Subsection:: *)
(*ChangeSmToSmBA*)


ChangeSmToSmBA[Sm] = SmBA
ChangeSmToSmBA[Sm2] = SmBA2
ChangeSmToSmBA[CSm2] = CSmBA2
ChangeSmToSmBA[Sm4] = SmBA4


(* ::Subsection:: *)
(*SmHeadPattern*)


SmHeadPattern = Sm | Sm2 | CSm2 | Sm4


(* ::Subsection:: *)
(*SmBAHeadPattern*)


SmBAHeadPattern = SmBA | SmBA2 | CSmBA2 | SmBA4


(* ::Subsection:: *)
(*BSpHeadPattern*)


BSpHeadPattern = Lat | CLat | USpb | UbarSpb


(* ::Subsection:: *)
(*ASpHeadPattern*)


ASpHeadPattern = La | CLa | USpa | UbarSpa


(* ::Subsection:: *)
(*QuietSpinorPrint*)


SetAttributes[QuietSpinorPrint, HoldFirst]


QuietSpinorPrint[expr_] :=
	Block[
		{PRINT}
		,
		PRINT[__] = Null;
		expr
	]


(* ::Subsection:: *)
(*DeclareUndeclareQFactory*)


DeclareUndeclareQFactory[name_String, initialTrue_List, text_String] :=
	Module[
		{
			qFunction = Symbol[name <> "Q"]
		}
		,
		(qFunction[#] = True)& /@ initialTrue;
		
		qFunction[_] = False;
		
		
		Symbol["Declare" <> name][x__] := (
			Unprotect[Evaluate[qFunction]];
			(qFunction[#] = True)& /@ {x};
			Protect[Evaluate[qFunction]];
			
			PRINT[{x} ," added to the list of " <> text]
		);
		
		
		Symbol["Undeclare" <> name][x__] := (
			Unprotect[Evaluate[qFunction]];
			Quiet[
				(qFunction[#] =.)& /@ {x};,
				{Unset::norep}
			];
			Protect[Evaluate[qFunction]];
			
			PRINT[{x} ," removed from the list of " <> text]
		);
	];


End[]


(* ::Subsection:: *)
(*Public symbols protection*)


ProtectContextNonVariables[];


EndPackage[]
