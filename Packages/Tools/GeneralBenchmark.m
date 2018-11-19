(* ::Package:: *)

(* ::Subsection:: *)
(*Benchmark*)

TestReportAnalyze::usage = "";
NetAnalyze::usage = "";

(* ::Subsection:: *)
(*Main*)


Begin["`Benchmark`"];


(* ::Subsubsection::Closed:: *)
(*TestReport*)


TestReportAnalyze[obj_TestReportObject] := Block[
	{attr},
	attr = <|
		"Index" -> #TestIndex,
		"TestID" -> #TestID,
		"Result" -> #Outcome,
		"Time" -> QuantityMagnitude[#AbsoluteTimeUsed, "Seconds"],
		"MemoryChange" -> N@QuantityMagnitude[#MemoryUsed, "Megabytes"]
	|> &;
	"Test" -> attr /@ Association @@@ Values[obj["TestResults"]]
];



(* ::Subsubsection::Closed:: *)
(*NetAnalyze*)


NetAnalyze[net_] := "Net" -> <|
	"Size" -> QuantityMagnitude[NetInformation[net, "ArraysTotalSize"], "Megabytes"],
	"Parameters" -> NetInformation[net, "ArraysTotalElementCount"],
	"Nodes" -> NetInformation[net, "LayersCount"],
	"Layers" -> KeyMap[ToString, Association @@ Sort@Normal@NetInformation[net, "LayerTypeCounts"]]
|>;


(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
