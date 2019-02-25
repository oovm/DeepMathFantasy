(* ::Package:: *)
(* ::Title:: *)
(*NetLoad(NetLoad)*)
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
NetLoad::usage = "NetLoad[net,name]";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`NetLoad`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$NetLoad = "V1.0";
Updated$NetLoad = "2019-02-06";
(* ::Subsubsection:: *)
(*功能块 1*)
NetLoad[net_, name_] := GeneralUtilities`Scope[
	init = NeuralNetworks`NetDeinitialize[net];
	node = Import[name <> "-map.json", "RawJSON"];
	node = Association[Query[All, #Name -> StringSplit[#Path, "."]&]@node];
	params = Last@Sort@FileNames[name <> "*.params"];
	params = MXNetLink`NDArrayImport[params];
	path = Table[DeleteCases[node[key], "Nodes" | "Arrays"] -> params[key], {key, Keys@params}];
	NetReplacePart[init, path]
];
(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]