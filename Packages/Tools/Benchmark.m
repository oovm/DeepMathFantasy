(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Benchmark*)
ClassifyTestReportAnalyze::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`Benchmark`"];
(* ::Subsubsection:: *)
(*TestReport*)
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
ClassifyDualAnalyze[cm_ClassifierMeasurementsObject] := Block[
	{ans},
	ans = <|
		"Count" -> Total /@ cm@"ConfusionFunction",
		"TPRate" -> cm@"Recall",
		"TNRate" -> cm@"Specificity",
		"FPRate" -> cm@"FalsePositiveRate",
		"FNRate" -> cm@"FalseNegativeRate",
		"F1Score" -> cm@"F1Score"
	|>;
	tiny = Sort@Keys@TakeSmallest[ans["F1Score"], UpTo[25]]
	"Dual" -> Query[All, Key /@ tiny]@ans
];


(* ::Subsection::Closed:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]