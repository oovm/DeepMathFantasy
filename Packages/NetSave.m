(* ::Package:: *)
(* ::Title:: *)
(*NetSave(NetSave)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(* ::Text:: *)
(*Author:Aster*)
(*Creation Date:2019-02-06*)
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
NetSave::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`NetSave`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$NetSave = "V1.0";
Updated$NetSave = "2019-02-06";
(* ::Subsubsection:: *)
(*功能块 1*)
NameWriter[head_, path_] := head <> NameWriter[path]
NameWriter["."] := "Layer";
NameWriter[str_String] := If[
	StringFreeQ[str, "."],
	str,
	StringRiffle[Part[StringSplit[StringTrim[str, "."], "."], 2 ;; All ;; 2],
		"."
	]
];
link[name_, path_ -> array_] := GeneralUtilities`Scope[<|
	"Path" -> path,
	"Name" -> NameWriter[name, path],
	"Array" -> array,
	"Type" -> array["Type"],
	"Shape" -> Dimensions[array],
	"MinMax" -> ToString /@ MinMax[Flatten@Normal@array]
|>];
(* ::Subsubsection:: *)
(*MXNetJSON*)
MXNetJSON[model_NeuralNetworks`NetPlan] := GeneralUtilities`Scope[
	node = MXNetLink`MXSymbolToJSON[model["Symbol"]];
	node = MapAt[NameWriter, node, {"nodes", All, "name"}];
	json = Developer`WriteRawJSONString[node, "Compact" -> 2];
	StringReplace[json, {
		s : StringExpression["arg_nodes\":[", Shortest[___], "]"] :> StringReplace[s, Whitespace -> ""],
		s : StringExpression["node_row_ptr\":[", Shortest[___], "]"] :> StringReplace[s, Whitespace -> ""],
		"$" :> "."
	}]
];


(* ::Subsubsection:: *)
(*MXNetJSON*)
NetSave[net_, name_ : "Net"] := GeneralUtilities`Scope[
	model = NeuralNetworks`ToNetPlan[net];
	netMap = Flatten[{
		link["arg:", #]& /@ Normal@model["WeightArrays"], link["aux:", #]& /@ Normal@model["AuxilliaryArrays"]
	}];
	ndArray = <|(#Name -> MXNetLink`NDArrayCreate@#Array)& /@ netMap|>;
	map = SortBy[
		KeyDrop[#, "Array"]& /@ netMap,
		Quiet[ToExpression /@ StringSplit[#Path, "."]]&
	];
	{
		MXNetLink`NDArrayExport[name <> "-0000.params", ndArray],
		Export[name <> "-symbol.json", MXNetJSON[model], "Text"],
		Export[name <> "-map.json", map, "RawJSON", "Compact" -> 2]
	}
];



(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ NetSave },
	{ReadProtected}
]
End[]