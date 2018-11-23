(* ::Package:: *)

(* ::Subsection:: *)
(*Benchmark*)

TestReportAnalyze::usage = "";
NetAnalyze::usage = "";
UploadSMMS::usage = "";


(* ::Subsection:: *)
(*Main*)


Begin["`Benchmark`"];


(* ::Subsubsection::Closed:: *)
(*TestReport*)


TestReportAnalyze[obj_TestReportObject] := Block[
	{attr},
	attr = <|
		"Index" -> First[#2],
		"TestID" -> #TestID,
		"Result" -> #Outcome,
		"Time" -> QuantityMagnitude[#AbsoluteTimeUsed, "Seconds"],
		"CPUTime" -> QuantityMagnitude[#CPUTimeUsed, "Seconds"],
		"MemoryChange" -> N@QuantityMagnitude[#MemoryUsed, "Megabytes"]
	|>&;
	MapIndexed[attr, Values[obj["TestResults"]][[All, 1]]]
];


(* ::Subsubsection::Closed:: *)
(*NetAnalyze*)


NetAnalyze[net_] := "Net" -> <|
	"Size" -> QuantityMagnitude[NetInformation[net, "ArraysTotalSize"], "Megabytes"],
	"Parameters" -> NetInformation[net, "ArraysTotalElementCount"],
	"Nodes" -> NetInformation[net, "LayersCount"],
	"Layers" -> KeyMap[ToString, Association @@ Sort@Normal@NetInformation[net, "LayerTypeCounts"]]
|>;




SetAttributes[OutCoreEvaluate, HoldAll];
OutCoreEvaluate[expr_] := Module[{link, result},
	link = LinkLaunch[First@$CommandLine <> " -mathlink -noprompt"];
	LinkWrite[link, Unevaluated@EvaluatePacket@expr];
	result = LinkRead@link;
	LinkClose@link;
	Replace[result, ReturnPacket@x_ :> x]
];


UploadSMMS[path_String] := Block[
	{body = "smfile" -> <|"Content" -> File[path], "Name" -> FileNameTake[path]|>},
	URLExecute[HTTPRequest["https://sm.ms/api/upload", <|"Body" -> {body}|>], "RawJSON"]
];


(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
