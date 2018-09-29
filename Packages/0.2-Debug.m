(* ::Package:: *)
(* ::Title:: *)
(*Debug(Debug)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(* ::Text:: *)
(*Author:Aster*)
(*Creation Date:2018-09-19*)
(*Copyright: Mozilla Public License Version 2.0*)
(* ::Program:: *)
(*1.软件产品再发布时包含一份原始许可声明和版权声明。*)
(*2.提供快速的专利授权。*)
(*3.不得使用其原始商标。*)
(*4.如果修改了源代码，包含一份代码修改说明。*)
(**)
(* ::Text:: *)
(*这里应该填这个函数的介绍*)
(* ::Section:: *)
(*函数说明*)
StackWatch::usage = "";
StackView::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`Debug`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$Debug = "V1.0";
Updated$Debug = "2018-09-19";
(* ::Subsubsection:: *)
(*功能块 1*)
SetAttributes[StackWatch, HoldAllComplete];
StackWatch[expr_, fn_ : StackWatchStyle] := Module[
	{enter, exit, depth = 0},
	SetAttributes[{enter, exit}, HoldAllComplete];
	enter[args__] := With[
		{d = depth++, l = Length@{args}},
		fn[Hold[{True, d}, If[l > 1, HoldForm /@ {args}, {HoldForm@args}]]
		]];
	exit[args__] := With[
		{d = --depth, l = Length@{args}},
		fn[Hold[{False, d}, If[l > 1, HoldForm /@ {args}, {HoldForm@args}]]
		]];
	TraceScan[enter, expr, _, exit]
];
StackWatchStyle[Hold[att_, log__]] := Module[
	{n = ToString@Last@att, t, s, line},
	s = "\n---------------------------\n\t";
	t = If[
		First@att,
		Style[">> L_" <> n <> "\n\t", Red, Bold],
		Style["<< L_" <> n <> "\n\t", Blue, Bold]
	];
	line = If[Length@log > 1, DeleteDuplicates @@ log, First@log];
	line = If[Length@line > 1, Riffle[{line}, s], {line}];
	Print[t, Sequence @@ line];
];



(* ::Subsubsection:: *)
(*功能块 2*)
SetAttributes[StackView, HoldAllComplete]
StackView[expr_] := Module[
	{steps = {}, stack = {}, pre, post},
	pre[e_] := (stack = {steps, stack};steps = {});
	post[e_, r_] := (
		steps = First@stack ~ Join ~ {{e, steps, HoldForm[r]}};
		stack = stack[[2]]
	);
	SetAttributes[post, HoldAllComplete];
	TraceScan[pre, expr, ___, post];
	DynamicModule[
		{focus, show, subStep, enter, exit},
		focus = steps;
		subStep[{e_, {}, _}, _] := {Null, e, Style["inert", {Italic, Small}]};
		subStep[{e_, _, r_}, p_] := {
			Button[Style["show", Small], enter[p]], e,
			Style[Row[{"-> ", r}], Small]
		};
		enter[{p_}] := PrependTo[focus, focus[[1, 2, p]]];
		exit[] := focus = Drop[focus, 1];
		show[{e_, s_, r_}] := Column[{Grid[
			{
				{"Expression", Column@Reverse@focus[[All, 1]]},
				{
					Column[{"Trace",
						focus /. {{_} :> Sequence[], _ :> Button["Back", exit[], ImageSize -> Automatic]}
					}],
					Grid[MapIndexed[subStep, s], Alignment -> Left]
				},
				{"Result", Column@focus[[All, 3]]},
				{"Steps", Length@Flatten@steps}
			}, Background -> {{LightCyan}},
			Alignment -> Left, Frame -> All
		]}];
		Dynamic@show@focus[[1]]
	]
];
(* ::Subsubsection:: *)
(*功能块 2*)
SetAttributes[StackToFile, HoldFirst];
StackToFile[expr_, filename_String, timeout_ : 60] := With[
	{file = OpenWrite[filename]}, Echo[timeout, "Timeout: "];
	Echo[FileNameJoin[{Directory[], filename}], "File: "];
	Echo[ToString[Hold[expr], InputForm], "Expresion"];
	TimeConstrained[TraceScan[WriteLine[file, StringTake[ToString[#, InputForm], {10, -2}]]&, expr], timeout];
	Close[file];
	File[FileNameJoin[{Directory[], filename}]]
];

(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{StackWatch, StackView, StackToFile},
	{Protected, ReadProtected}
];
End[]
