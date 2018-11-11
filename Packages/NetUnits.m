(* ::Package:: *)
(* ::Title:: *)
(*NetLayers*)
(* ::Subchapter:: *)
(*Introduce*)
VggBlock::usage = "";
(* ::Subchapter:: *)
(*Main*)
(* ::Subsection:: *)
(*Settings*)
Begin["`Factory`"];
(* ::Subsection::Closed:: *)
(*Codes*)
Version$NetUnits = "V1.0";
Updated$NetUnits = "2018-10-19";
(* ::Subsubsection:: *)
(*defFromFile*)

VggBlock[c_Integer, u_Integer : 1, m_String : ""] := Block[
	{},
	If[Or[c < 1, u < 1], Return@GluonCV`helper`paraErr];
	Switch[m,
		"BN", VggBasicBN[c, u],
		___, VggBasic[c, u]
	]
];




(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]