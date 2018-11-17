(* ::Package:: *)

(* ::Subsection:: *)
(*Inherit*)


NetPlotInfo::usage = "";


(* ::Subsection:: *)
(*Main*)


Begin["`Inherit`"];


(* ::Subsubsection::Closed:: *)
(*MXNetLink`MXJSONPlot*)
Options[NetPlotInfo] = {
	"ShowTensors" -> True,
	"VertexLabels" -> Placed["ID", Above],
	"VertexOrder" -> Automatic,
	"EdgeBundling" -> True,
	"OutputTensors" -> None,
	"InternalDimensions" -> None
};
NetPlotInfo[net_NetChain, opts : OptionsPattern[]] := NetPlotInfo[MXNetLink`ToMXJSON[net]["JSON"], opts];
NetPlotInfo[net_NetGraph, opts : OptionsPattern[]] := NetPlotInfo[MXNetLink`ToMXJSON[net]["JSON"], opts];
NetPlotInfo[json_String, opts : OptionsPattern[]] := NetPlotInfo[Developer`ReadRawJSONString[json], opts];
NetPlotInfo[expr_Association, OptionsPattern[]] := Block[
	{
		showTensors, vertexLabels, vertexOrder, edgeBundling, outputTensors,
		internalDimensions, nodes, argnodes, heads, $oldids, nameStrings, typeStrings,
		edges, nodeOps, longRange, opTypes, opNames, nullType, blank, maxIndex,
		name, nodeDims, nops, opStyles, opSizes, vertexTypeData, labels,
		infoGrids, nnodes
	},
	{
		showTensors, vertexLabels, vertexOrder, edgeBundling, outputTensors, internalDimensions
	} = OptionValue @ {
		"ShowTensors", "VertexLabels", "VertexOrder", "EdgeBundling", "OutputTensors","InternalDimensions"
	};
	{nodes, argnodes, heads} = Lookup[expr, {"nodes", "arg_nodes", "heads"}];
	$oldids = If[ListQ[vertexOrder],
		GeneralUtilities`IndexOf[vertexOrder, #name] - 1& /@ nodes,
		Range[Length[nodes]] - 1
	];
	nodes = Map[Append[#, "inputs1" -> #inputs[[All, 1]] + 1]&, nodes];
	nameStrings = MXNetLink`Visualization`PackagePrivate`toVertexLabelString[#name]& /@ nodes;
	typeStrings = MXNetLink`Visualization`PackagePrivate`toVertexTypeString[#op]& /@ nodes;
	argnodes += 1; heads += 1;
	edges = Join @@ MapIndexed[
		If[#op === "null", Nothing,
			If[showTensors,
				Thread[Prepend[#2, #inputs1]],
				Thread[Prepend[#2, Complement[#inputs1, argnodes]]]
			]]&
		,
		nodes
	];
	edges = DeleteDuplicates[edges];
	nodeOps = nodes[[All, "op"]];
	If[edgeBundling && !FreeQ[nodeOps, "Concat" | "SliceChannel"],
		longRange = MXNetLink`Visualization`PackagePrivate`pickLongRangeEdges[edges, nodes],
		longRange = None;
	];
	{opTypes, opNames} = GeneralUtilities`Labelling @ nodeOps;
	(* Add head tensors *)
	nullType = GeneralUtilities`IndexOf[opNames, "null"];
	nodes = MapIndexed[Append[#, "id" -> First[#2] - 1]&, nodes];
	If[showTensors && ListQ[outputTensors],
		opTypes = Join[opTypes, ConstantArray[nullType, Length@outputTensors]];
		argnodes = Join[argnodes, Range@Length@outputTensors + Max@edges];
		nameStrings = Join[nameStrings, outputTensors];
		blank = ConstantArray["", Length[outputTensors]];
		$oldids = Join[$oldids, blank]; typeStrings = Join[typeStrings, blank];
		nodes = Join[nodes, blank];
		maxIndex = Max@edges;
		MapIndexed[AppendTo[edges, {First@#1, (First[#2] + maxIndex)}]&, heads]
	];
	(* Do plot *)
	nops = Length[opNames];
	opStyles = MXNetLink`Visualization`PackagePrivate`opColor /@ opNames;
	opSizes = ReplacePart[ConstantArray[6, nops], nullType -> 4];
	opStyles = ReplacePart[opStyles, nullType -> Gray];
	opNames = opNames /. "null" -> "Tensor";
	vertexTypeData = <|"VertexStyles" -> opStyles|>;
	If[showTensors, vertexTypeData = Join[vertexTypeData, <|"VertexSizes" -> opSizes|>]];
	labels = vertexLabels /. {"Name" :> nameStrings, "ID" :> ( $oldids + 1), "Type" :> typeStrings};
	nnodes = Length[nodes];
	{edges,
		"VertexLabels" -> labels,
		"HiddenVertices" -> If[showTensors, None, argnodes],
		"VertexTypeData" -> vertexTypeData,
		"VertexTypeLabels" -> Capitalize@opNames,
		If[showTensors, "VertexTypes" -> opTypes, "VertexStyles" -> opTypes],
		"LongRangeEdges" -> longRange
	}
];




(* ::Subsubsection::Closed:: *)
(*ClassifyAnalyze*)




(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
