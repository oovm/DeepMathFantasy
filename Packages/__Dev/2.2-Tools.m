(* ::Package:: *)
(* ::Title:: *)
(*Tools(Tools)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(* ::Text:: *)
(*Author:Aster*)
(*Creation Date:2018-10-09*)
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
NetChain2Graph::usage = "这里应该填这个函数的说明,如果要换行用\"\\r\"\r就像这样";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
Begin["`Tools`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$Tools = "V1.0";
Updated$Tools = "2018-10-09";
(* ::Subsubsection:: *)
(*功能块 1*)
NetChain2Graph[other___] := other;
NetChain2Graph[net_NetChain] := Block[
	{nets = Normal@net},
	NetGraph[nets,
		Rule @@@ Partition[Range@Length@nets, 2, 1],
		"Input" -> NetExtract[net, "Input"],
		"Output" -> NetExtract[net, "Output"]
	];
]




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
