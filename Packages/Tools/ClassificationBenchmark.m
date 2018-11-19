(* ::Package:: *)

(* ::Subsection:: *)
(*Benchmark*)



ClassificationEvaluate::usage = "";
ClassificationIndicatorAnalyze::usage = "";
ClassificationClassAnalyze::usage = "";
ClassificationSpeed::usage = "";
ClassificationProbabilitiesPlot::usage = "";
ClassificationUncertaintyAnalyzeThenPlot::usage = "";
ClassificationConfusionAnalyzeThenPlot::usage = "";
ClassificationWorstPlot::usage = "";


(* ::Subsection:: *)
(*Main*)


Begin["`Benchmark`"];

(* ::Subsubsection::Closed:: *)
(*ClassificationEvaluate*)

ClassificationEvaluate[net_, data_] := Block[
	{classes, $now = Now, $eval},
	classes = NetExtract[net, "Output"][["Labels"]];
	$eval = net[First /@ data, "Probabilities", TargetDevice -> "GPU"];
	$now = QuantityMagnitude[Now - $now, "Seconds"];
	MachineLearning`file115ClassifierPredictor`PackagePrivate`fillClassifierMeasurementsObject[
		MachineLearning`PackageScope`NetToClassifierFunction@net,
		{Range@Length@data, Last /@ data},
		First@Keys@ReverseSort[#]& /@ $eval,
		Log[Values /@ $eval],
		classes,
		SparseArray@Array[1&, Length@data],
		$now
	]
];

(* ::Subsubsection::Closed:: *)
(*ClassificationClassAnalyze*)


ClassificationClassAnalyze[cm_ClassifierMeasurementsObject] := Block[
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

ClassificationProbabilitiesPlot[cm_ClassifierMeasurementsObject] := Block[
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

ClassificationUncertaintyAnalyzeThenPlot[cm_ClassifierMeasurementsObject] := Block[
	{thresholds, accuracies, rejections, pts, plot},
	thresholds = Join[Range[0.25, 0.80, 0.05], Range[0.81, 0.99, 0.01], Range[0.991, 0.999, 0.001], Range[0.9991, 0.9999, 0.0001]];
	{accuracies, rejections} = Transpose[cm[{"Accuracy", "RejectionRate"}, IndeterminateThreshold -> #]& /@ thresholds] /. {Indeterminate -> 1};
	pts = MapThread[
		Callout[{#1, #2},
			Column @ {
				Row @ {"Accept = ", #2},
				Row @ {"Rejecte = ", #1},
				Row @ {"Uncertainty", " = ", #3}
			}, CalloutMarker -> "CirclePoint", (*LabelStyle\[Rule]{12,Bold,Blue},*)LeaderSize -> 25, Appearance -> "CurvedLeader"
		]&,
		{rejections, accuracies, thresholds}
	] /. {r_Real :> Round[r, 0.0001]};
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

ClassificationConfusionAnalyzeThenPlot[cm_ClassifierMeasurementsObject] := Block[
	{class, img, matrix},
	class = Sort@Take[Flatten[cm["TopConfusions" -> 100] /. Rule -> List] // DeleteDuplicates, UpTo[25]];
	img = Magnify[Show[cm["ConfusionMatrixPlot" -> class], ImageSize -> 600], 2];
	matrix = Lookup[#, class]& /@ Lookup[cm["ConfusionFunction"], class];
	Export["ConfusionMatrix.png", img, Background -> None];
	"Confusion" -> <|
		"Classes" -> class,
		"ConfusionMatrix" -> matrix
	|>
];

ClassificationWorstPlot[cm_ClassifierMeasurementsObject] := Block[
	{exp},
	exp = Take[Flatten@cm[{"WorstClassifiedExamples", "LeastCertainExamples", "IndeterminateExamples"}], UpTo[100]];
	(*
		tags=Transpose[{First/@exp,Last/@exp,cm["ClassifierFunction"]/@First/@exp}]
		Grid[Partition[#,4]&@(Labeled[#1,Column[{"   true:"<>ToString@#2,"predict:"<>ToString@#3}],Top]&@@@tags),Frame\[Rule]All]
	*)
	Rasterize@ImageCollage[exp[[All, 1]]]
];

AskTopN[cm_] := Block[
	{num = Length[First[cm]["ExtendedClasses"]]},
	Which[
		num <= 5, {1, 2, 3, 4},
		num <= 10, {1, 2, 3, 5},
		num <= 100, {1, 2, 5, 10},
		num <= 1000, {1, 2, 5, 25},
		num <= 10000, {1, 5, 25, 100}
	]
];
ProbabilityLoss[cm_] := Block[
	{right, ass = First[cm]},
	right = Flatten@Position[Inner[SameQ, ass["Predictions"], ass["TestSet", "Output"], List], True];
	Mean[1 - cm["Probabilities"][[right]]]
];
ClassificationIndicatorAnalyze[cm_ClassifierMeasurementsObject] := "Indicator" -> <|
	Sequence @@ Table["Top-" <> ToString[i] -> cm["Accuracy" -> i], {i, AskTopN@cm}],
	"LogLikelihood" -> cm@"LogLikelihood",
	"CrossEntropyLoss" -> cm@"MeanCrossEntropy",
	"ProbabilityLoss" -> ProbabilityLoss[cm],
	"MeanProbability" -> Mean[cm@"Probabilities"],
	"GeometricMeanProbability" -> cm@"GeometricMeanProbability",
	"VarianceProbability" -> Variance[cm@"Probabilities"],
	"ScottPi" -> cm@"ScottPi",
	"CohenKappa" -> cm@"CohenKappa",
	"RejectionRate" -> cm@"RejectionRate"
|>;

ClassificationSpeed[cm_ClassifierMeasurementsObject] := "Speed" -> 1000 cm@"BatchEvaluationTime";


Options[doFormat] = {"Mark" -> "%", "Times" -> 100, "Digit" -> 6};
doFormat[r_, OptionsPattern[]] := Block[
	{num, dot, mark, t, digit},
	{mark, t, digit} = OptionValue@{"Mark", "Times", "Digit"};
	{num, dot} = RealDigits[r t, 10, digit];
	If[dot > 0,
		StringRiffle[Append[Insert[num, ".", dot + 1], mark], ""],
		StringRiffle[Append[Take[Insert[Join[Array[0&, 1 - dot], num], ".", 2], digit + 1], mark], ""]
	]
];



$ClassificationReportTemplate = StringTemplate["\
# `Name`
![Task](https://img.shields.io/badge/Task-Classifation-Orange.svg)
![Size](https://img.shields.io/badge/Size-`ShieldSize`-blue.svg)
![Accuracy](https://img.shields.io/badge/Accuracy-`ShieldAccuracy`-brightgreen.svg)
![Speed](https://img.shields.io/badge/Speed-`ShieldSpeed`-ff69b4.svg)

Automatically generated on `Date`

## Network structure:
- Network Size: **`NetSize` MB**
- Parameters: **`Parameters`**
- Nodes Count: **`Nodes`**
- Speed: **`Speed`/sample**
- Layers:
`NetLayers`

## Accuracy Curve
![Classification Curve.png](`img_1`)

![High Precision Classification Curve.png](`img_2`)

## Main Indicator
`Indicator`
![Accuracy Rejection Curve.png](`img_3`)

## Class Indicator
`Dual`
|-------|-------|--------|--------|--------|--------|---------|
`DualScore`

## Hard Class
![ConfusionMatrix.png](`img_4`)

## Evaluation Report
`Test`
|-------|--------|--------|------|--------------|
`TestReport`
"];
ClassificationReport[record_] := Block[
	{line, md, indicatorF, DualScoreF, line2, TestReportF, speed},
	speed = doFormat[record["Speed"], "Times" -> 1, "Digit" -> 4, "Mark" -> " ms"];
	indicatorF = indicatorF = MapAt[doFormat, Values@KeyDrop[record["Indicator"], "Speed"], List /@ {1, 2, 3, 4, 8, 9, -1}];
	line = Transpose@Join[{Keys@First@Values@record["Dual"]}, Values /@ Values@record["Dual"]];
	DualScoreF = MapAt[doFormat[#, "Times" -> 1, "Mark" -> ""]&, MapAt[doFormat, line, {All, 3 ;; 6}], {All, -1}];
	line2 = MapAt[doFormat[#, "Times" -> 1, "Mark" -> " s"]&, Values /@ record["Test"], {All, -2}];
	TestReportF = MapAt[If[# > 0, "+", "-"] <> doFormat[#, "Times" -> 1, "Mark" -> " MB"]&, line2, {All, -1}];
	md = $ClassificationReportTemplate[<|
		"ShieldSize" -> ToString[N@FromDigits@RealDigits[record["Net", "Size"], 10, 5]] <> "%20MB",
		"ShieldAccuracy" -> doFormat[record["Indicator", "Top-1"], "Digit" -> 5, "Mark" -> "%25"],
		"ShieldSpeed" -> StringReplace[speed, " " -> "%20"],
		"Name" -> record["Name"],
		"Date" -> record["Date"],
		"NetSize" -> record["Net", "Size"],
		"Parameters" -> StringRiffle[Reverse@Flatten@Riffle[Partition[Reverse@IntegerDigits@record["Net", "Parameters"], UpTo[3]], " "], ""],
		"Nodes" -> record["Net", "Nodes"],
		"Speed" -> speed,
		"NetLayers" -> Inner[StringJoin["  - ", #1, ": **", ToString[#2], "**\n"]&, Keys@record["Net", "Layers"], Values@record["Net", "Layers"], StringJoin],
		"Indicator" -> Inner[StringJoin["  - ", #1, ": **", ToString[#2], "**\n"]&, Keys@KeyDrop[record["Indicator"], "Speed"], indicatorF, StringJoin],
		"img_1" -> record["Image", "Classification Curve.png"],
		"img_2" -> record["Image", "High Precision Classification Curve.png"],
		"img_3" -> record["Image", "Accuracy Rejection Curve.png"],
		"img_4" -> record["Image", "ConfusionMatrix.png"],
		"Dual" -> StringRiffle[Prepend[Keys@record["Dual"], "Class"], {"| ", " | ", " |"}],
		"DualScore" -> StringRiffle[StringRiffle[#, {"| ", " | ", " |"}]& /@ DualScoreF, "\n"],
		"Test" -> StringRiffle[Keys@First@record["Test"], {"| ", " | ", " |"}],
		"TestReport" -> StringRiffle[StringRiffle[#, {"| ", " | ", " |"}]& /@ TestReportF, "\n"]
	|>]
];







SetAttributes[Reaper, HoldAll];
Reaper[expr_] := Block[
	{raw = Reap@Reap[expr, "Test"]},
	{raw[[1, 2, 1]], raw[[2, 2]]}
];
CheckDependency[] := Block[
	{name = "Check Dependency", var},
	var := var = {<< MachineLearning`, << NeuralNetworks`, << MXNetLink`, << DeepMath`};
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
];
CheckParallelize[] := Block[
	{name = "Check Parallelize", var},
	var := var = LaunchKernels[];
	Sow[VerificationTest[MemberQ[{List, Symbol}, Head[var]], True, TestID -> name], "Test"];
];
getData[path_String] := Block[
	{name = "Loading Data", var},
	var := var = Import@path;
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
	Return[var]
];
getModel[path_String] := Block[
	{name = "Loading Model Loading", var},
	var := var = Import@path;
	Sow[VerificationTest[Head[var], NetChain, TestID -> name], "Test"];
	Return[var]
];
getDecoder[net_] := Block[
	{name = "Loading Decoder", var},
	var := var = NetExtract[net, "Output"];
	Sow[VerificationTest[Head[var], NetDecoder, TestID -> name], "Test"];
	Return[var]
];
getLabels[net_] := Block[
	{name = "Loading Labels", var},
	var := var = NetExtract[net, "Output"][["Labels"]];
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
	Return[var]
];
getSample[data_List] := Block[
	{name = "Sampling", var},
	var := var = If[
		Head@data[[1, 1]] == File,
		Import /@ RandomSample[First /@ data, 16],
		RandomSample[First /@ data, 16]
	];
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
	Return[var]
];
CPUTiming[net_NetChain, sample_List] := Block[
	{name = "CPU Timing", var},
	var := var = {
		"CPU Warm-Up" -> First[net[sample] // Timing],
		"CPU Single" -> First[net[First@sample] // AbsoluteTiming],
		"CPU Batch" -> First[net[sample] // RepeatedTiming] / 16.0
	};
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
	Sow[name -> var]
];
GPUTiming[net_NetChain, sample_List] := Block[
	{name = "GPU Timing", var},
	var := var = {
		"GPU Warm-Up" -> First[net[sample, TargetDevice -> "GPU"] // Timing],
		"GPU Single" -> First[net[First@sample, TargetDevice -> "GPU"] // AbsoluteTiming],
		"GPU Batch" -> First[net[sample, TargetDevice -> "GPU"] // RepeatedTiming] / 16.0
	};
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
	Sow[name -> var]
];
CalculationStage[dataPath_, modelPath_] := Block[
	{data, model, labels, sample, eval, groups, dump},
	dump = Reaper[
	(*CheckDependency[];*)
		Sow[VerificationTest[True, True, TestID -> "CalculationStage"], "Test"];
		data = getData[dataPath];
		model = getModel[modelPath];
		Sow@NetAnalyze[model];
		Sow["Actual" -> data[[All, 2]]];
		Sow["Decoder" -> getDecoder[model]];
		labels = getLabels[model];
		Sow["Classes" -> labels];
		Sow["Number" -> Length@labels];
		sample = getSample[data];
		CPUTiming[model, sample];
		GPUTiming[model, sample];
		
		
		eval = NetReplacePart[model, "Output" -> Length@labels];
		
		groups = Partition[First /@ data, UpTo@Ceiling[10^6 / Length@labels]];
		
		
		
		Sow[VerificationTest[True, True, TestID -> "Stage Finish"], "Test"];
		Sow[VerificationTest[Clear[data, model, eval, groups], Null, TestID -> "Fast GC"], "Test"];
	];
	Export["CalculationStage.dump", dump, "MX"]
];







(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
