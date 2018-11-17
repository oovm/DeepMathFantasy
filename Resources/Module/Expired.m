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