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





(* ::Subsubsection::Closed:: *)
(*ClassificationClassAnalyze*)
SetAttributes[Reaper, HoldAll];
Reaper[expr_] := Block[
	{raw = Reap[expr]},
	{raw[[-1, 2]], raw[[-1, -1]]}
];
AskTopN[cm_] := Block[
	{num = Length[First[cm]["ExtendedClasses"]]},
	Which[
		num <= 5, {1, 2, 3, 4},
		num <= 10, {1, 2, 3, 5},
		num <= 100, {1, 2, 5, 10, 25},
		num <= 1000, {1, 2, 5, 10, 25, 100},
		num <= 10000, {1, 5, 10, 25, 100, 500}
	]
];
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




CheckDependency[] := Block[
	{name = "Check Dependency", var},
	var := var = {<< MachineLearning`, << NeuralNetworks`, << MXNetLink`, << DeepMath`};
	Sow[VerificationTest[Head[var], List, TestID -> name], "Test"];
];
CheckParallelize[] := Block[
	{name = "Check Parallelize", var},
	var := var = ParallelEvaluate[$KernelID];
	Sow[VerificationTest[ListQ[var], True, TestID -> name], "Test"];
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
doInference[net_NetChain, img_List] := Block[
	{var, expt},
	var := var = net[img, TargetDevice -> "GPU"];
	expt := expt = Export["Inferencing.dump", var, "MX"];
	Sow[VerificationTest[MatrixQ@var, True, TestID -> "Inferencing"], "Test"];
	Sow[VerificationTest[StringQ@expt, True, TestID -> "Dumping"], "Test"]
];
Options[CalculationStage] = {DumpSave -> False};
CalculationStage[dataPath_, modelPath_] := Block[
	{data, model, labels, sample, eval, groups, dump},
	dump = Reaper[
	(*CheckDependency[];*)
		Sow[VerificationTest[True, True, TestID -> "CalculationStage"], "Test"];
		data = getData[dataPath];
		model = getModel[modelPath];
		Sow@NetAnalyze[model];
		
		Sow["Decoder" -> getDecoder[model]];
		labels = getLabels[model];
		Sow["Classes" -> labels];
		Sow["Number" -> Length@labels];
		
		Sow["Actual" -> data[[All, 2]]];
		Sow["Count" -> Length@data];
		
		
		
		sample = getSample[data];
		CPUTiming[model, sample];
		GPUTiming[model, sample];
		
		
		eval = NetReplacePart[model, "Output" -> Length@labels];
		
		(*groups = Partition[First /@ data, UpTo@Ceiling[10^6 / Length@labels]];*)
		If[TrueQ@OptionValue[DumpSave],
			doInference[eval, First /@ data, Ceiling[10^6 / Length@labels]],
			doInference[eval, First /@ data]
		];
		
		
		
		
		Sow[VerificationTest[True, True, TestID -> "Stage Finish"], "Test"];
		Sow[VerificationTest[Clear[data, model, eval, groups], Null, TestID -> "Fast GC"], "Test"];
	];
	Export["CalculationStage.dump", dump, "MX"]
];





(*ProbabilitiesMatrix Related*)
getProbabilitiesMatrix[] := Block[
	{name = "Reloading", var},
	var := var = Import["Inferencing.dump"];
	Sow[VerificationTest[MatrixQ[var], True, TestID -> name], "Test"];
	Return[var]
];
evalPrediction[decoder_, pMatrix_] := decoder@pMatrix;
getPrediction[attr_Association] := Block[
	{name = "Prediction", var},
	var := var = evalPrediction @@ Lookup[attr, {"Decoder", "pMatrix"}];
	Sow[VerificationTest[ListQ[var], True, TestID -> name], "Test"];
	Return[var]
];
evalTopN[{pMatrix_, decoder_, actual_}, ks_] := Block[
	{eval},
	eval = N@Mean@Boole@MapThread[MemberQ, {decoder[pMatrix, {"TopDecisions", #}], actual}]&;
	Sow["TopN" -> Association@ParallelTable["Top-" <> ToString[k] -> eval[k], {k, ks}]];
];
sowTopN[attr_Association, ks_List] := Block[
	{name = "TopN", var},
	var := var = evalTopN[Lookup[attr, {"pMatrix", "Decoder", "Actual"}], ks];
	Sow[VerificationTest[var, Null, TestID -> name], "Test"];
];
evalProbabilities[pMatrix_, classes_, nclass_, actual_] := Block[
	{index, outputindices, logp},
	index = AssociationThread[classes -> Range[nclass]];
	outputindices = Lookup[index, actual, 0];
	MapThread[Part, {pMatrix, outputindices}]
];
getProbabilities[attr_Association] := Block[
	{name = "Probabilities", var},
	var := var = evalProbabilities @@ Lookup[attr, {"pMatrix", "Classes", "Number", "Actual"}];
	Sow[VerificationTest[ListQ[var], True, TestID -> name], "Test"];
	Return[var]
];




evalProbabilitiesPlot[pList_] := Block[
	{exporter, count, plot},
	exporter = # -> Export[#, plot, Background -> None]&;
	count = Reverse@BinCounts[pList, {0, 100 / 100, 5 / 100}];
	plot = RectangleChart[
	(*Inner[Labeled[{1,#1},#2,Below]&,count,percentage,List],*)
		Table[{5, c}, {c, count}], ImageSize -> 1200,
		ChartLabels -> {Placed[count, Above]}, ScalingFunctions -> "Log",
		BarSpacing -> 0, ChartStyle -> "CMYKColors", PlotRange -> {{0, 100}, All}
	(*ColorFunction\[Rule]Function[{x,y},ColorData["Pastel"][1-y^(1/4)]]*),
		Ticks -> {{#, Text@Style[ToString[100 - #] <> "%", Bold], {0, 0}}& /@ Range[0, 100, 5], Automatic},
		Epilog -> Text[Style["Classification Curve", "Title", 30], Offset[{-250, -20}, Scaled[{1, 1}]], {-1, 0}]
	];
	Sow[exporter@"Classification Curve.png"];
	count = Reverse@BinCounts[pList, {95, 100, 0.1} / 100];
	plot = RectangleChart[
		Table[{2, c}, {c, count}], ImageSize -> 1200,
		ChartLabels -> {Placed[count, Above]}, ScalingFunctions -> "Log",
		BarSpacing -> 0, ChartStyle -> "CandyColors", PlotRange -> {{0, 100}, All},
		Ticks -> {{#, Text@Style[StringRiffle[Insert[IntegerDigits[1000 - # / 2], ".", -2], ""] <> "%", Bold], {0, 0}}& /@ Range[0, 100, 10], Automatic},
		Epilog -> Text[Style["High Precision Classification Curve", "Title", 30], Offset[{-420, -20}, Scaled[{1, 1}]], {-1, 0}]
	];
	Sow[exporter@"High Precision Classification Curve.png"];
];
doProbabilitiesPlot[attr_Association] := Block[
	{name = "ProbabilitiesPlot", var},
	var := var = evalProbabilitiesPlot @@ Lookup[attr, {"Probabilities"}];
	Sow[VerificationTest[var, Null, TestID -> name], "Test"];
];
evalProbabilityLoss[predictions_, actual_, p_] := Block[
	{pos = Position[Inner[SameQ, predictions, actual, List], True]},
	Sow["pLoss" -> <|
		"ProbabilityLoss" -> Mean[1 - p[[Flatten@pos]]],
		"ProbabilityMean" -> Mean@p,
		"ProbabilityGeometricMean" -> GeometricMean@p,
		"ProbabilityVariance" -> Variance@p
	|>];
];
sowProbabilityLoss[attr_Association] := Block[
	{name = "ProbabilityLoss", var},
	var := var = evalProbabilityLoss @@ Lookup[attr, {"Prediction", "Actual", "Probabilities"}];
	Sow[VerificationTest[var, Null, TestID -> name], "Test"];
	Return[var]
];
evalLogLikelihoodRate[pList_, count_] := Block[
	{llr = Mean@Log@pList},
	Sow["logLike" -> <|
		"Perplexity" -> E^-llr,
		"CrossEntropyLoss" -> -llr,
		"LogLikelihood" -> count llr
	|>];
];
sowLogLikelihoodRate[attr_Association] := Block[
	{name = "LogLikelihoodRate", var},
	var := var = evalLogLikelihoodRate @@ Lookup[attr, {"Probabilities", "Count"}];
	Sow[VerificationTest[var, Null, TestID -> name], "Test"];
];




evalIndicesMatrix[predictions_, actual_, classes_, nClass_] := Block[
	{iMatrix, actual, predicted, coordinates, index},
	index = AssociationThread[classes -> Range[nClass]];
	actual = Lookup[index, actual];
	predicted = Lookup[index, predictions, nClass + 1];
	iMatrix = ConstantArray[{}, {nClass, nClass + 1}];
	coordinates = Transpose @ {actual, predicted};
	Function[Part[iMatrix, Apply[Sequence, #]] = #2] @@@ Normal[
		GroupBy[Thread[coordinates -> Range[Length[coordinates]]], First -> Last]
	];
	iMatrix
];
getIndicesMatrix[attr_Association] := Block[
	{name = "IndicesMatrix", var},
	var := var = evalIndicesMatrix @@ Lookup[attr, {"Prediction", "Actual", "Classes", "Number"}];
	Sow[VerificationTest[ListQ[var], True, TestID -> name], "Test"];
	Return[var]
];
evalConfusionMatrix[iMatrix_] := Map[Length, iMatrix, {2}];
getConfusionMatrix[attr_Association] := Block[
	{name = "ConfusionMatrix", var},
	var := var = evalConfusionMatrix @@ Lookup[attr, {"iMatrix"}];
	Sow[VerificationTest[MatrixQ[var], True, TestID -> name], "Test"];
	Return[var]
];
evalTopConfusion[counts_, classes_, nClass_] := Block[
	{pair, confusions, top},
	pair = Subsets[Range @ nClass, {2}];
	confusions = MaximalBy[pair ,
		counts[[Sequence @@ #]]&,
		Min[100, Binomial[nClass, 2]]
	];
	top = Take[Flatten@confusions // DeleteDuplicates, UpTo[25]];
	If[
		Head[First@classes] == Entity,
		SortBy[classes[[top]], CommonName],
		Sort[classes[[top]]]
	]
];
getTopConfusion[attr_Association] := Block[
	{name = "TopConfusion", var},
	var := var = evalTopConfusion @@ Lookup[attr, {"cMatrix", "Classes", "Number"}];
	Sow[VerificationTest[MatrixQ[var], True, TestID -> name], "Test"];
	Return[var]
];
ConfusionMatrixPlot[cMatrix_, classes_, top_] := Block[
	{subset, subMatrix, rowF, columnF, nClass, exporter, plot},
	exporter = # -> Export[#, plot, Background -> None]&;
	subset = Flatten@Map[Position[classes, #]&, top];
	subMatrix = Part[cMatrix, subset, subset];
	(*indeterminatecounts = Part[cMatrix, subset, -1];*)
	rowF = Part[Map[Function[N[With[{t = Total@#}, If[Greater[t, 0], # / t, #]]]], Part[cMatrix, All, 1 ;; -2]], subset];
	columnF = Part[Transpose[Map[Function[N[With[{t = Total@#}, If[Greater[t, 0], # / t, #]]]], Transpose[Part[cMatrix, All, 1 ;; -2]]]], subset];
	nClass = Length@top;
	plot = MatrixPlot[subMatrix, PlotTheme -> "Web",
		PlotRangePadding -> 0, ImageSize -> 1200,
		FrameTicksStyle -> 1.5 Min[(12 * (2 * 20)^0.25) / (nClass + 20)^0.25, 12],
		FrameLabel -> Style[#, "Title", 24]& /@ {"Ground Truth", "Predicted Lable"},
		FrameTicks -> {
			Transpose@{Range[nClass], Map[Rotate[#, 0.]&, top]},
			Transpose@{
				Range @ nClass,
				Map[Function[Rotate[#, Pi / 2]], Total @ subMatrix]
			},
			Transpose@{Range@nClass, Map[Total, subMatrix]},
			Transpose@{Range @ nClass, Map[Function[Rotate[#, Pi / 2]], top]}
		},
		Epilog -> Table[
			Inset[
				Graphics[{Opacity @ 1., Text[subMatrix[[j, i]], {0.5, 0.5}], Opacity @ 0., Rectangle[{0, 0}, {1, 1}]}],
				{i - 0.5, nClass + (-j) + 0.5}, Automatic, {1., 1.}
			],
			{i, 1, nClass},
			{j, 1, nClass}
		]
	];
	Sow[exporter["ConfusionMatrix.png"]];
];
doConfusionMatrixPlot[attr_Association] := Block[
	{name = "ConfusionMatrixPlot", var},
	var := var = ConfusionMatrixPlot @@ Lookup[attr, {"cMatrix", "Classes", "cTop"}];
	Sow[VerificationTest[var, Null, TestID -> name], "Test"];
];
evalClassIndicator[iMatrix_, classes_, count_] := Block[
	{ex, eval},
	ex = MachineLearning`file115ClassifierPredictor`PackagePrivate`iClassifierMeasurementsObject[<|"IndicesMatrix" -> iMatrix, "ExtendedClasses" -> classes, "Weights" -> Array[1&, count]|>];
	eval[sample_] := AssociationMap[First@ex[# -> sample]&, {
		"TruePositiveRate",
		"TrueNegativeRate",
		"FalsePositiveRate",
		"FalseNegativeRate",
		"TruePositiveAccuracy",
		"TrueNegativeAccuracy",
		"FalseDiscoveryRate",
		"F1Score",
		"Informedness",
		"Markedness",
		"MatthewsCorrelationCoefficient"
	}];
	Sow["ClassScore" -> eval /@ classes]
];
sowClassIndicator[attr_Association] := Block[
	{name = "ClassScore", var},
	var := var = evalClassIndicator @@ Lookup[attr, {"iMatrix", "cTop", "Count"}];
	Sow[VerificationTest[var, Null, TestID -> name], "Test"];
];
Options[AnalysisStage] = {};
AnalysisStage[OptionsPattern[]] := Block[
	{dump, $Register},
	dump = Reaper[
		Sow[VerificationTest[True, True, TestID -> "AnalysisStage"], "Test"];
		CheckParallelize[];
		$Register = Association[Last@Import["CalculationStage.dump"]];
		(*{"Net","Actual","Decoder","Classes","Number","CPU Timing","GPU Timing"}*)
		
		
		$Register["pMatrix"] = getProbabilitiesMatrix[];
		(*{"Net","Actual","Decoder","Classes","Number","CPU Timing","GPU Timing","pMatrix"}*)
		
		
		Scan[Sow[# -> Lookup[$Register, #]]&, {"Net", "Number", "CPU Timing", "GPU Timing"}];
		$Register["Prediction"] = getPrediction@$Register;
		(*{"Net","Actual","Decoder","Classes","Number","CPU Timing","GPU Timing","pMatrix","Prediction"}*)
		
		
		sowTopN[$Register, {1, 2, 5, 10, 25, 100}];
		$Register["Probabilities"] = getProbabilities@$Register;
		(*{"Net","Actual","Decoder","Classes","Number","CPU Timing","GPU Timing","pMatrix","Probabilities","Prediction"}*)
		
		
		useless = {"Net", "CPU Timing", "GPU Timing", "pMatrix", "Decoder"};
		Sow[VerificationTest[Length@KeyDropFrom[$Register, useless], 6, TestID -> "Fast GC"], "Test"];
		(*{"Classes","Number","Actual","Count","Prediction","Probabilities"}*)
		
		
		doProbabilitiesPlot[$Register];
		sowProbabilityLoss[$Register];
		sowLogLikelihoodRate[$Register];
		$Register["iMatrix"] = getIndicesMatrix@$Register;
		(*{"Classes","Number","Actual","Count","Prediction","Probabilities","iMatrix"}*)
		$Register["cMatrix"] = getConfusionMatrix@$Register;
		(*{"Classes","Number","Actual","Count","Prediction","Probabilities","iMatrix","cMatrix"}*)
		$Register["cTop"] = getTopConfusion@$Register;
		(*{"Classes","Number","Actual","Count","Prediction","Probabilities","iMatrix","cMatrix","TopConfusion"}*)
		
		
		doConfusionMatrixPlot[$Register] ;
		sowClassIndicator[$Register];
		Sow[VerificationTest[True, True, TestID -> "Stage Finish"], "Test"];
		Sow[VerificationTest[Clear[$Register], Null, TestID -> "Fast GC"], "Test"];
	];
	Export["AnalysisStage.dump", dump]
];









$ReportTemplate = StringTemplate["\
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
makeReport[record_] := Block[
	{line, md, indicatorF, DualScoreF, line2, TestReportF, speed},
	speed = doFormat[record["Speed"], "Times" -> 1, "Digit" -> 4, "Mark" -> " ms"];
	indicatorF = indicatorF = MapAt[doFormat, Values@KeyDrop[record["Indicator"], "Speed"], List /@ {1, 2, 3, 4, 8, 9, -1}];
	line = Transpose@Join[{Keys@First@Values@record["Dual"]}, Values /@ Values@record["Dual"]];
	DualScoreF = MapAt[doFormat[#, "Times" -> 1, "Mark" -> ""]&, MapAt[doFormat, line, {All, 3 ;; 6}], {All, -1}];
	line2 = MapAt[doFormat[#, "Times" -> 1, "Mark" -> " s"]&, Values /@ record["Test"], {All, -2}];
	TestReportF = MapAt[If[# > 0, "+", "-"] <> doFormat[#, "Times" -> 1, "Mark" -> " MB"]&, line2, {All, -1}];
	md = $ReportTemplate[<|
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





(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
