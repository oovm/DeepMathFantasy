(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Benchmark*)
ClassifyTestReportAnalyze::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`Benchmark`"];
(* ::Subsubsection:: *)
(*ClassifyTestReport*)
ClassifyTestReportAnalyze[obj_TestReport] := Block[
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

(* ::Subsubsection:: *)



(* ::Subsection::Closed:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]