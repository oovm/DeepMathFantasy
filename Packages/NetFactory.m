(* ::Package:: *)
(* ::Title:: *)
(*NetForge(NetForge)*)
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
NetForge::usage = "";
(* ::Section:: *)
(*程序包正体*)
(* ::Subsection::Closed:: *)
(*主设置*)
ExNumber::usage = "程序包的说明,这里抄一遍";
Begin["`Factory`"];
(* ::Subsection::Closed:: *)
(*主体代码*)
Version$NetForge = "V1.0";
Updated$NetForge = "2018-10-24";
(* ::Subsubsection:: *)
(*功能块 1*)
NetForge[name_String, paras___] := Switch[
	StringDelete[ToLowerCase@name, {WhitespaceCharacter, "-"}],
	"resnet", resnetForge[paras],
	"resnext", resnextForge[paras],
	"vgg", VggForge[paras],
	_, NetForgeList[]
];




(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]