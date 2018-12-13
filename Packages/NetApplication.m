(* ::Package:: *)

(* ::Subsection:: *)
(*Inherit*)


NetApplication::usage = "2";


(* ::Subsection:: *)
(*Main*)


Begin["`Tools`NetApplication`"];


(* ::Subsubsection::Closed:: *)
(*Icon*)
$icon = Block[
	{
		layerCounts = {3, 2, 5, 1},
		graph, vstyle
	},
	graph = GraphUnion @@ MapThread[
		IndexGraph, {
			CompleteGraph /@ Partition[layerCounts, 2, 1],
			FoldList[Plus, 0, layerCounts[[;; -3]]]
		}];
	vstyle = Catenate[Thread /@ Thread[
		TakeList[VertexList[graph], layerCounts] -> ColorData[97] /@ Range@Length[layerCounts]
	]];
	Graph[
		graph,
		GraphLayout -> {"MultipartiteEmbedding", "VertexPartition" -> layerCounts},
		GraphStyle -> "BasicBlack",
		VertexSize -> 0.65,
		VertexStyle -> vstyle,
		ImageSize -> 60
	]
];
(* ::Subsubsection::Closed:: *)
(*Object*)

NetApplicationQ[asc_?AssociationQ] := AllTrue[{"Name","Models", "Input", "Date"}, KeyExistsQ[asc, #]&]
NetApplicationQ[_] = False;
NetApplication /: MakeBoxes[
	obj : NetApplication[asc_? NetApplicationQ],
	form : (StandardForm | TraditionalForm)
] := Module[
	{above, below},
	above = {
		{BoxForm`SummaryItem[{"Name: ", asc["Name"]}]},
		{BoxForm`SummaryItem[{"Input: ", asc["Input"]}]},
		{BoxForm`SummaryItem[{"Date: ", asc["Date"]}]}
	};
	below = {
	
	};
	BoxForm`ArrangeSummaryBox[
		DeepMath`NetApplication,
		obj,$icon, above,below,form,
		"Interpretable" -> Automatic
	]
];

(* ::Subsubsection::Closed:: *)
(*Methods*)
NetApplication[asc_?AssociationQ][Input]:=Activate@Lookup[asc,"Example"]
NetApplication[asc_?AssociationQ][Function]:=GeneralUtilities`PrintDefinitionsLocal@Lookup[asc,"Handler"]
NetApplication[asc_?AssociationQ][NetModel]:=Lookup[asc,"Models"]
NetApplication[asc_?AssociationQ][other___]:=Lookup[asc,"Handler"][asc,other]





(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
