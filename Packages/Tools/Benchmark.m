(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Benchmark*)
TestReportAnalyze::usage = "";
ClassifyDualAnalyze::usage = "";
ClassifyProbabilitiesPlot::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`Benchmark`"];
(* ::Subsubsection:: *)
(*TestReport*)
TestReportAnalyze[obj_TestReport] := Block[
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
	{ans, tiny},
	ans = <|
		"Count" -> Total /@ cm@"ConfusionFunction",
		"TPRate" -> cm@"Recall",
		"TNRate" -> cm@"Specificity",
		"FPRate" -> cm@"FalsePositiveRate",
		"FNRate" -> cm@"FalseNegativeRate",
		"F1Score" -> cm@"F1Score"
	|>;
	tiny = Sort@Keys@TakeSmallest[ans["F1Score"], UpTo[25]];
	"Dual" -> Query[All, Key /@ tiny]@ans
];

ClassifyProbabilitiesPlot[cm_ClassifierMeasurementsObject] := Block[
	{count, plot},
	count = Reverse@BinCounts[cm@"Probabilities", {0, 100 / 100, 5 / 100}];
	plot = RectangleChart[
	(*Inner[Labeled[{1,#1},#2,Below]&,count,percentage,List],*)
		Table[{5, c}, {c, count}], ImageSize -> 1200,
		ChartLabels -> {Placed[count, Above]}, ScalingFunctions -> "Log",
		BarSpacing -> 0, ChartStyle -> "CMYKColors", PlotRange -> {{0, 100}, All}
	(*ColorFunction\[Rule]Function[{x,y},ColorData["Pastel"][1-y^(1/4)]]*),
		Ticks -> {{#, Text@Style[ToString[100 - #] <> "%", Bold], {0, 0}}& /@ Range[0, 100, 5], Automatic},
		Epilog -> Text[Style["Classification Curve", "Title", 30], Offset[{-250, -20}, Scaled[{1, 1}]], {-1, 0}]
	];
	Export["Classification Curve.png", plot, Background -> None];
	count = Reverse@BinCounts[cm@"Probabilities", {95, 100, 0.1} / 100];
	plot = RectangleChart[
		Table[{2, c}, {c, count}], ImageSize -> 1200,
		ChartLabels -> {Placed[count, Above]}, ScalingFunctions -> "Log",
		BarSpacing -> 0, ChartStyle -> "CandyColors", PlotRange -> {{0, 100}, All},
		Ticks -> {{#, Text@Style[StringRiffle[Insert[IntegerDigits[1000 - # / 2], ".", -2], ""] <> "%", Bold], {0, 0}}& /@ Range[0, 100, 10], Automatic},
		Epilog -> Text[Style["High Precision Classification Curve", "Title", 30], Offset[{-420, -20}, Scaled[{1, 1}]], {-1, 0}]
	];
	Export["High Precision Classification Curve.png", plot, Background -> None];
];


(* ::Subsection::Closed:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]