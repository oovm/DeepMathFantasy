(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Benchmark*)
TestReportAnalyze::usage = "";
ClassifyDualAnalyze::usage = "";
ClassifyProbabilitiesPlot::usage = "";
ClassifyUncertaintyAnalyzeThenPlot::usage = "";
ClassifyConfusionAnalyzeThenPlot::usage = "";
ClassifyWorstPlot::usage = "";
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
	{attr, tiny},
	attr = <|
		"Count" -> Total /@ cm@"ConfusionFunction",
		"TPRate" -> cm@"Recall",
		"TNRate" -> cm@"Specificity",
		"FPRate" -> cm@"FalsePositiveRate",
		"FNRate" -> cm@"FalseNegativeRate",
		"F1Score" -> cm@"F1Score"
	|>;
	tiny = Sort@Keys@TakeSmallest[attr["F1Score"], UpTo[25]];
	"Dual" -> Query[All, Key /@ tiny]@attr
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
(*Histogram[
	cm@"Probabilities", {0.05}, "LogCount",
	ChartBaseStyle -> EdgeForm[Dotted], LabelingFunction -> Above,
	PlotLabel -> Style["Expired", "Title", 14], PlotRange -> All, PlotRangePadding -> None
]*)

ClassifyUncertaintyAnalyzeThenPlot[cm_ClassifierMeasurementsObject] := Block[
	{thresholds, accuracies, rejections, pts, plot},
	thresholds = Join[Range[0.25, 0.80, 0.05], Range[0.81, 0.99, 0.01], Range[0.991, 0.999, 0.001], Range[0.9991, 0.9999, 0.0001]];
	{accuracies, rejections} = Transpose[cm[{"Accuracy", "RejectionRate"}, IndeterminateThreshold -> #]& /@ thresholds] /. r_Real :> Round[r, 0.0001];
	pts = MapThread[
		Callout[{#1, #2},
			Column @ {
				Row @ {"Accept = ", #2},
				Row @ {"Rejecte = ", #1},
				Row @ {"Uncertainty", " = ", #3}
			}, CalloutMarker -> "CirclePoint", (*LabelStyle\[Rule]{12,Bold,Blue},*)LeaderSize -> 25, Appearance -> "CurvedLeader"
		]&,
		{rejections, accuracies, thresholds}
	];
	plot = ListLinePlot[pts,
		PlotRange -> {{0, Max@rejections}, {Min@accuracies, 1}},
		Filling -> Axis, ImageSize -> 900,
		PlotTheme -> {"Monochrome", "FullAxes"},
		FrameLabel -> Style[#, 20]& /@ {"Indeterminate Threshold", "Accuracy"},
		GridLines -> Automatic, GridLinesStyle -> Directive[GrayLevel[0.5, 0.5], AbsoluteThickness@1, AbsoluteDashing@{1, 2}],
	(*PlotMarkers\[Rule]Graphics[{EdgeForm[Directive[AbsoluteThickness[1.],RGBColor[0.34398,0.49112,0.89936]]],White,Disk[{0,0},Offset[2*{1.,1.},{0.,0.}]]}],*)
		Epilog -> {
			Text[Style["Accuracy Rejection Curve", "Title", 30, Black], Offset[{-325, + 30}, Scaled[{1, 0}]], {-1, 0}],
			{Dashed, Line[{{0, Min@accuracies}, {Max@rejections, Max@accuracies}}]}
		}
	];
	Export["Accuracy Rejection Curve.png", Show[plot, ImageSize -> 1200], Background -> None];
	"Threshold" -> <|
		"Uncertainty" -> thresholds,
		"AcceptanceRate" -> accuracies,
		"RejectionRate" -> rejections
	|>
];

ClassifyConfusionAnalyzeThenPlot[cm_ClassifierMeasurementsObject] := Block[
	{class, img},
	class = Sort@Take[Flatten[cm["TopConfusions" -> 100] /. Rule -> List] // DeleteDuplicates, UpTo[25]];
	img = Magnify[Show[cm["ConfusionMatrixPlot" -> class], ImageSize -> 600], 2];
	Export["ConfusionMatrix.png", img, Background -> None];
	"Confusion" -> <|
		"Classes" -> class,
		"ConfusionMatrix" -> Lookup[#, class]& /@ Lookup[cm["ConfusionFunction"], class]
	|>
];


ClassifyWorstPlot[cm_ClassifierMeasurementsObject] := Block[
	{exp},
	exp = Take[Flatten@cm[{"WorstClassifiedExamples", "LeastCertainExamples", "IndeterminateExamples"}], UpTo[100]];
	(*
		tags=Transpose[{First/@exp,Last/@exp,cm["ClassifierFunction"]/@First/@exp}]
		Grid[Partition[#,4]&@(Labeled[#1,Column[{"   true:"<>ToString@#2,"predict:"<>ToString@#3}],Top]&@@@tags),Frame\[Rule]All]
	*)
	Rasterize@ImageCollage[exp[[All, 1]]]
];





(* ::Subsection::Closed:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]