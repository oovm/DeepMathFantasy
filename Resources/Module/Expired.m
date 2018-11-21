ClassificationInformation[net_] := {
	"Input" -> NetExtract[net, 1][["Input"]],
	"ParametersCount" -> NetInformation[net, "ArraysTotalElementCount"],
	"ModelSize" -> First@UnitConvert[NetInformation[net, "ArraysTotalSize"], "Megabytes"],
	"Nodes" -> NetInformation[net, "LayersCount"]
};
Options[ClassificationBenchmark] = {};
ClassificationBenchmark[net_, data_List, top_List : {1}] := Block[
	{$now, ans, time, pLoss, layer, TCE, getTop, right},
	$now = Now;
	ans = net[Keys@data, "Probabilities", TargetDevice -> "GPU"];
	time = Now - $now;
	(*Export["cache.wxf", ans];*)
	pLoss := "ProbabilityLoss" -> Mean[1 - MapThread[#1[#2]&, {ans, Values@data}]];
	layer = CrossEntropyLossLayer["Index", "Target" -> NetEncoder@NetExtract[net, "Output"]];
	TCE := "CrossEntropyLoss" -> Tr@MapThread[layer[<|"Input" -> Values@#1, "Target" -> #2|>]&, {ans, Values@data}];
	getTop[n_] := (
		right = MapThread[MemberQ[Keys[TakeLargest[#1, n]], #2]&, {ans, Values@data}];
		"Top-" <> ToString[n] -> Total@Boole@right
	);
	<|
		"Date" -> DateString@$now,
		"Number" -> Length@data,
		"Losses" -> SortBy[Flatten[{getTop /@ top, pLoss, TCE}], First],
		"Time" -> First@UnitConvert[time, "SI"]
	|>
];








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

ClassificationUncertaintyAnalyzeThenPlot[cm_ClassifierMeasurementsObject] := Block[
	{thresholds, accuracies, rejections, pts, plot},
	thresholds = Join[Range[0.25, 0.80, 0.05], Range[0.81, 0.99, 0.01], Range[0.991, 0.999, 0.001], Range[0.9991, 0.9999, 0.0001]];
	{accuracies, rejections} = Transpose[cm[{"Accuracy", "RejectionRate"}, IndeterminateThreshold -> #]& /@ thresholds] /. {Indeterminate -> 1};
	pts = MapThread[
		Callout[{#1, #2},
			Column@{
				Row@{"Accept = ", #2},
				Row@{"Rejecte = ", #1},
				Row@{"Uncertainty", " = ", #3}
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
ProbabilityLoss[cm_ClassifierMeasurementsObject] := Block[
	{right, ass = First[cm]},
	right = Flatten@Position[Inner[SameQ, ass["Predictions"], ass["TestSet", "Output"], List], True];
	Mean[1 - cm["Probabilities"][[right]]]
];
AskTopN[cm_ClassifierMeasurementsObject] := Block[
	{num = Length[First[cm]["ExtendedClasses"]]},
	Which[
		num <= 5, {1, 2, 3, 4},
		num <= 10, {1, 2, 3, 5},
		num <= 100, {1, 2, 5, 10},
		num <= 1000, {1, 2, 5, 25},
		num <= 10000, {1, 5, 25, 100}
	]
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
ClassificationWorstPlot[cm_ClassifierMeasurementsObject] := Block[
	{exp},
	exp = Take[Flatten@cm[{"WorstClassifiedExamples", "LeastCertainExamples", "IndeterminateExamples"}], UpTo[100]];
	(*
		tags=Transpose[{First/@exp,Last/@exp,cm["ClassifierFunction"]/@First/@exp}]
		Grid[Partition[#,4]&@(Labeled[#1,Column[{"   true:"<>ToString@#2,"predict:"<>ToString@#3}],Top]&@@@tags),Frame\[Rule]All]
	*)
	Rasterize@ImageCollage[exp[[All, 1]]]
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
ClassificationProbabilitiesPlot[cm_ClassifierMeasurementsObject] := Histogram[
	cm@"Probabilities", {0.05}, "LogCount",
	ChartBaseStyle -> EdgeForm[Dotted], LabelingFunction -> Above,
	PlotLabel -> Style["Expired", "Title", 14], PlotRange -> All, PlotRangePadding -> None
]