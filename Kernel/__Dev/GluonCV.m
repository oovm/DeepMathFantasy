(* ::Package:: *)
(* ::Title:: *)
(*GluonCV(GluonCV)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(* ::Text:: *)
(*Author:Aster*)
(*Creation Date:2018-10-06*)
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
GluonCVImport::usage = "导入一个 GluonCV 模型";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`GluonCV`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$GluonCV = "V1.0";
Updated$GluonCV = "2018-10-06";
(* ::Subsubsection:: *)
(*功能块 1*)
pyGluonCVImport = StringTemplate["\
import gluoncv as gv
from gluoncv.utils import export_block
net = gv.model_zoo.get_model('`name`', pretrained=True)
export_block('`path`', net, preprocess=True)\
"];
GluonCVImport[name_String, dir_String : True] := Block[
	{path, return},
	return = If[TrueQ@path,
		path = $HomeDirectory;
		ExternalEvaluate[$pySession, pyGluonCVImport[<|"name" -> name, "path" -> name|>]],
		If[!FileExistsQ@dir, CreateDirectory[dir]];
		path = StringReplace[FileNameJoin[{dir, name}], "\\" -> "/"];
		ExternalEvaluate[$pySession, pyGluonCVImport[<|"name" -> name, "path" -> path|>]]
	];
	If[FailureQ@return, return, path]
];





defFromFile[file_, sfx_String] := Block[
	{def},
	def = AssociateTo[NeuralNetworks`Private`ReadDefinitionFile[file, "GluonCV`"], "Suffix" -> sfx];
	NeuralNetworks`DefineLayer[FileBaseName[file], def]
];

$LoadingLayers := Block[
	{layers},
	Needs["NeuralNetworks`"];
	layers = FileNames["*", FileNameJoin[{$GluonCVDirectory, "Kernel", "Layers"}]];
	Quiet@Table[defFromFile[file, "Layer"], {file, layers}];
	True
];

(*TODO:Add Installer*)

(* ::Subsubsection:: *)
(*功能块 2*)
ExampleFunction[2] = "我就是个示例函数,什么功能都没有";


(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
