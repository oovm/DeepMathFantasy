(* ::Package:: *)
(* ::Title:: *)
(*NetFactory(NetFactory)*)
(* ::Subchapter:: *)
(*程序包介绍*)
(* ::Text:: *)
(*Mathematica Package*)
(*Created by Mathematica Plugin for IntelliJ IDEA*)
(*Establish from GalAster's template(v1.3)*)
(**)
(* ::Text:: *)
(*Author:Aster*)
(*Creation Date:2018-10-24*)
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
Begin["`Factory`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$NetFactory = "V1.0";
Updated$NetFactory = "2018-10-24";
(* ::Subsubsection:: *)
(*功能块 1*)
NetFactory[name_String, paras___] := Switch[
	StringDelete[ToLowerCase@name, {WhitespaceCharacter,"-"}],
	"resnet", resnetFactory[paras],
	"resnetv2", resnetv2Factory[paras],
	"resnext", resnextFactory[paras],
	_, NetFactoryList[]
];



VggBlock[c_Integer,u_Integer:1,m_String:""]:=Block[
	{},
	If[Or[c<1,u<1],Return@GluonCV`helper`paraErr];
	Switch[m,
		"BN",VggBasicBN[c,u],
		___,VggBasic[c,u]
	]
];


(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
]
End[]