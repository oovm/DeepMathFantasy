(* ::Package:: *)
(* ::Title:: *)
(*NetForge*)
(* ::Subchapter:: *)
(*Introduce*)
NetForge::usage = "";
(* ::Subchapter:: *)
(*Main*)
(* ::Subsection:: *)
(*Settings*)
Begin["`Factory`"];
(* ::Subsection::Closed:: *)
(*Codes*)
Version$NetForge = "V1.0";
Updated$NetForge = "2018-10-24";
(* ::Subsubsection:: *)
(*NetForge*)
NetForge[name_String, paras___] := Switch[
	StringDelete[ToLowerCase@name, {WhitespaceCharacter, "-"}],
	"res", resnetForge[paras],
	"resnet", resnetForge[paras],
	"resnext", resnextForge[paras],
	"vgg", VggForge[paras],
	"vggnet", VggForge[paras],
	_, NetForgeList[]
];




(* ::Subsection::Closed:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]