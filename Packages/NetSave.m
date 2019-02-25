(* ::Package:: *)
(* ::Title:: *)
(*NetSave*)
(* ::Subchapter:: *)
(*Introduce*)
NetSave::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`NetSave`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
DeepMath`Settings`NetSaveRewrite = False;
(* ::Subsubsection:: *)
(*功能块 1*)
$NameRewrite = {
	"Weights" -> "Weight",
	"Biases" -> "Bias"
};
NameWriter[head_, path_] := head <> NameWriter[path];
NameWriter["."] := "Layer";
NameWriter[in_String] := GeneralUtilities`Scope[
	If[StringFreeQ[in, "."], Return@in];
	str = DeleteCases[StringSplit[in, "."], "Nodes" | "Arrays"];
	If[!DeepMath`Settings`NetSaveRewrite, Return@StringRiffle[str, "."]];
	(*UpperCase can't re-init in stupid python version*)
	str[[-1]] = str[[-1]] /. $NameRewrite;
	StringRiffle[ToLowerCase[str], "."]
];
link[name_, path_ -> array_] := GeneralUtilities`Scope[<|
	"Path" -> path,
	"Name" -> NameWriter[name, path],
	"Array" -> array,
	"Type" -> array["Type"],
	"Shape" -> Dimensions[array]
(*"MinMax" -> ToString /@ MinMax[Flatten@Normal@array]*)
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
		link["arg:", #]& /@ Normal@model["WeightArrays"],
		link["aux:", #]& /@ Normal@model["AuxilliaryArrays"]
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
];
End[]