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
ExampleFunction::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExNumber::usage = "程序包的说明,这里抄一遍";
Begin["`Debug`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$Debug = "V1.0";
Updated$Debug = "2018-09-19";
(* ::Subsubsection:: *)
(*功能块 1*)
ClearAll[StackWatch, StackWatchStyle]
SetAttributes[StackWatch, HoldAllComplete]
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
ExampleFunction[2] = "我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
]
End[]