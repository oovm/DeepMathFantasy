(* ::Package:: *)
(* ::Title:: *)
(*Tools*)
(* ::Subchapter:: *)
(*Introduce*)
NetChain2Graph::usage = "Transform a NetChain to NetGraph.";
ImageEncoder::usage = "";
LayerRemoveShape::usage = "Try to remove the shape of the layer";
LayerInformation::usage = "Try to get basic information of the layer";
PrintLine::usage = "Print Expression With Line Broken";
NetMerge::usage = "Merge net nodes";
MXNet$Bind::usage = "Import and Bind the MX-Symbol and MX-NDArray";
MXNet$Boost::usage = "A Function which call a mxnet evaluation";
ClassificationBenchmark::usage = "";
NetPlotInformation::usage = "";
(* ::Subchapter:: *)
(*Main*)
(* ::Subsection:: *)
(*Settings*)
Begin["`Tools`"];
Version$Tools = "V0.4";
Updated$Tools = "2018-10-20";
(* ::Subsection::Closed:: *)
(*Codes*)
(* ::Subsubsection:: *)
(*NetChain2Graph*)
PrintLine[expr_, s_ : N] := Block[
	{box, token},
	token = If[StringQ@s , "\"" ~~ s ~~ "\"", ToString@s];
	box = ToBoxes[expr] //. {{a___, token, ",", b___} :> {a, "\[IndentingNewLine]", b}};
	box = box //. {{a___, ",", token} :> {a, "\[IndentingNewLine]"}};
	CellPrint@Cell[BoxData@box, "Output"];
];


(* ::Subsubsection:: *)
(*NetChain2Graph*)
NetChain2Graph[other___] := other;
NetChain2Graph[net_NetChain] := Block[
	{nets = Normal@net},
	NetGraph[nets,
		Rule @@@ Partition[Range@Length@nets, 2, 1],
		"Input" -> NetExtract[net, "Input"],
		"Output" -> NetExtract[net, "Output"]
	]
];


(* ::Subsubsection:: *)
(*NetMerge*)
$opMap = <|
	Plus -> ThreadingLayer[Plus],
	Total -> TotalLayer[],
	Sum -> TotalLayer[],
	Times -> ThreadingLayer[Times],
	Product -> ThreadingLayer[Times],
	Join -> CatenateLayer[],
	Catenate -> CatenateLayer[]
|>;
netMerge[nodes_List, op_] := NetGraph[
	Append[nodes, op],
	{NetPort["Input"] -> Range@Length[nodes] -> (Length[nodes] + 1)}
];
netMergeIdentity[nodes_List, op_] := NetGraph[
	Append[nodes, op],
	Prepend[
		Table[NetPort["Input"] -> i -> Length[nodes] + 1, {i, Length[nodes]}],
		NetPort["Input"] -> Length[nodes] + 1
	]
];
Options[NetMerge] = {Identity -> False, Expand -> False};
NetMerge[nodes_, opt : OptionsPattern[]] := NetMerge[nodes, Plus, opt];
NetMerge[nodes_, opMap_, opt : OptionsPattern[]] := Block[
	{op, net, isExpand = OptionValue[Expand]},
	op = Lookup[$opMap, opMap, opMap];
	If[ListQ@nodes,
		net = If[
			TrueQ@OptionValue[Identity],
			netMergeIdentity[nodes, op],
			netMerge[nodes, op]
		],
		Return@NetMerge[{nodes}, op, Identity -> True, Expand -> isExpand]
	];
	Switch[isExpand,
		False, net,
		True , NetFlatten[net],
		_    , NetFlatten[NetChain2Graph /@ net]
	]
];

(* ::Subsubsection:: *)
(*ImageNetEncoder*)
ImageEncoder[size_ : 224, c_ : "RGB"] := NetEncoder[{
	"Image", size,
	ColorSpace -> c,
	"MeanImage" -> {.485, .456, .406},
	"VarianceImage" -> {.229, .224, .225}^2
}];


(* ::Subsubsection:: *)
(*LayerInformation*)
LayerInformation[conv_ConvolutionLayer] := <|
	"Name" -> "Convolution",
	"Array" -> If[
		NetExtract[conv, "Biases"] === None,
		{"Weights"},
		{"Weights", "Biases"}
	],
	"Color" -> RGBColor[{51, 85, 136} / 255],
	"Option" -> {
		"Kernel" -> NetExtract[conv, "KernelSize"],
		"Stride" -> NetExtract[conv, "Stride"],
		"Padding" -> NetExtract[conv, "PaddingSize"],
		"Dilation" -> NetExtract[conv, "Dilation"]
	},
	"Tensor" -> {
		"Input" -> NetExtract[conv, "Input"],
		"Output" -> NetExtract[conv, "Output"]
	}
|>;
LayerInformation[bn_BatchNormalizationLayer] := <|
	"Name" -> "BatchNorm",
	"Color" -> RGBColor[{51, 85, 68} / 255],
	"Array" -> {"Beta", "Gamma"},
	"Option" -> {
		"Momentum" -> NetExtract[bn, "Momentum"],
		"Epsilon" -> "10^" <> ToString@Round@Log10@NetExtract[bn, "Epsilon"]
	(*"MovingMean"\[Rule]toVec@NetExtract[bn,"MovingMean"],
	"MovingVariance"->toVec@NetExtract[bn,"MovingVariance"]*)
	},
	"Tensor" -> {
		"Input" -> NetExtract[bn, "Input"],
		"Output" -> NetExtract[bn, "Output"]
	}
|>;
getActivationFunction[f_] := Switch[f,
	Ramp , "ReLU",
	"RectifiedLinearUnit"[#1]&, "ReLU",
	LogisticSigmoid, "Sigmoid",
	___, "Function"
];

LayerInformation[f_ElementwiseLayer] := <|
	"Name" -> "Activation",
	"Color" -> RGBColor[{75, 27, 22} / 255],
	"Array" -> {getActivationFunction[f]},
	"Option" -> If[
		getActivationFunction[f] === "Function",
		f, {}
	],
	"Tensor" -> {
		"Input" -> NetExtract[f, "Input"],
		"Output" -> NetExtract[f, "Output"]
	}
|>;


layerGridNormal[info_Association] := Block[
	{pLen, fill, array, head, option, tensor},
	pLen = Max[StringLength /@ Flatten[{First /@ info["Tensor"], First /@ info["Option"]}]] + 2;
	fill = Row[{Style[StringPadRight[First@# <> ":", pLen], Bold], StringRiffle[Last@#, "×"]}]&;
	array = Item[Style[#, Black, 16, FontFamily -> "Comic Sans MS"], Background -> Lighter[Gray, 0.7]]&;
	head = Flatten@{
		Item[Style[info["Name"], White, 24], Background -> info["Color"]],
		array /@ info["Array"]
	};
	option = PadRight[{Column[fill /@ info["Option"]]}, Length@head, SpanFromLeft];
	tensor = PadRight[{Column[fill /@ info["Tensor"]]}, Length@head, SpanFromLeft];
	Grid[{head, option, tensor}, Frame -> All, Alignment -> Left]
];
layerGridConvolutionBiases[info_Association] := Block[
	{oFill, tFill, head, option, tensor},
	oFill = StringRiffle[# /. info["Option"], "×"]&;
	tFill = StringRiffle[# /. info["Tensor"], "×"]&;
	head = {
		Item[Style["Convolution", White, 24], Background -> RGBColor[{1 / 5, 1 / 3, 8 / 15}]],
		Item[Style["Weights", Black, 16, FontFamily -> "Comic Sans MS"], Background -> Lighter[Gray, 0.7]],
		Item[Style["Biases", Black, 16, FontFamily -> "Comic Sans MS"], Background -> Lighter[Gray, 0.7]]
	};
	option = {Column[{
		Row[{Style["Kernel:  ", Bold], oFill@"Kernel"}],
		Row[{Style["Padding: ", Bold], oFill@"Padding"}]
	}], Column[{
		Row[{Style["Stride:   ", Bold], oFill@"Stride"}],
		Row[{Style["Dilation: ", Bold], oFill@"Dilation"}]
	}], SpanFromLeft};
	tensor = {Column[{
		Row[{Style["Input:   ", Bold], tFill@"Input"}],
		Row[{Style["Output:  ", Bold], tFill@"Output"}]
	}], SpanFromLeft, SpanFromLeft};
	Grid[{head, option, tensor}, Frame -> All, Alignment -> Left]
];
layerGridConvolution[info_Association] := If[
	Length@info["Array"] == 2,
	layerGridConvolutionBiases[info],
	layerGridNormal[info]
];


(*Graph[{Property[1, VertexSize -> 0.1,VertexShape ->Rasterize[layerGridConvolutionBiases[info],ImageResolution -> 200]]}, {1 -> 1}]*)



(* ::Subsubsection:: *)
(*RemoveLayerShape*)
LayerRemoveShape[layer_ConvolutionLayer] := With[
	{
		k = NetExtract[layer, "OutputChannels"],
		kernelSize = NetExtract[layer, "KernelSize"] ,
		weights = NetExtract[layer, "Weights"],
		biases = NetExtract[layer, "Biases"],
		padding = NetExtract[layer, "PaddingSize"],
		stride = NetExtract[layer, "Stride"],
		dilation = NetExtract[layer, "Dilation"]
	},
	ConvolutionLayer[k, kernelSize,
		"Weights" -> weights, "Biases" -> biases,
		"PaddingSize" -> padding, "Stride" -> stride,
		"Dilation" -> dilation
	]
];
LayerRemoveShape[layer_PoolingLayer] := With[
	{
		f = NetExtract[layer, "Function"],
		kernelSize = NetExtract[layer, "KernelSize"] ,
		padding = NetExtract[layer, "PaddingSize"],
		stride = NetExtract[layer, "Stride"]
	},
	PoolingLayer[kernelSize, stride,
		"PaddingSize" -> padding, "Function" -> f
	]
];
LayerRemoveShape[layer_ElementwiseLayer] := With[
	{f = NetExtract[layer, "Function"]},
	ElementwiseLayer[f]
];
LayerRemoveShape[layer_SoftmaxLayer] := Nothing;
LayerRemoveShape[layer_FlattenLayer] := Nothing;


(* ::Subsubsection:: *)
(*MXNet$Bind*)
MXNet$Bind[pathJ_, pathP_] := Block[
	{symbol, params},
	symbol = MXNetLink`MXSymbolFromJSON@File[pathJ];
	params = MXNetLink`MXModelLoadParameters[pathP];
	<|
		"Framework" -> {"MXNet", Import[pathJ][[-1, -1, -1, -1, -1]]},
		"Graph" -> MXNetLink`MXSymbolToJSON@symbol,
		"Nodes" -> Length@MXNetLink`MXSymbolToJSON[symbol]["nodes"],
		"Put" -> {"Image", "Colorful", "ImageSize"},
		"Get" -> "Image",
		"<<" -> "data",
		">>" -> First@MXNetLink`MXSymbolOutputs@symbol,
		"Weight" -> MXNetLink`NDArrayGetRawArray /@ params["ArgumentArrays"],
		"Auxilliary" -> MXNetLink`NDArrayGetRawArray /@ params["AuxilliaryArrays"],
		"Fixed" -> <||>
	|>
];


(* ::Subsubsection:: *)
(*MXNet$Boost*)
Options[MXNet$Boost] = {TargetDevice -> "GPU"};
MXNet$Boost[dm_Association, OptionsPattern[]] := Block[
	{exe, device, port},
	device = NeuralNetworks`Private`ParseContext@OptionValue[TargetDevice];
	exe = NeuralNetworks`Private`ToNetExecutor[
		NeuralNetworks`NetPlan[<|
			"Symbol" -> MXNetLink`MXSymbolFromJSON@dm["Graph"],
			"WeightArrays" -> dm["Weight"],
			"FixedArrays" -> dm["Fixed"],
			"BatchedArrayDims" -> <|dm["<<"] -> {BatchSize, Sequence @@ Dimensions[#]}|>,
			"ZeroArrays" -> {},
			"AuxilliaryArrays" -> dm["Auxilliary"],
			"Inputs" -> <|"Input" -> dm["<<"]|>,
			"Outputs" -> <|"Output" -> dm[">>"]|>,
			"InputStates" -> <||>,
			"OutputStates" -> <||>,
			"Metrics" -> <||>,
			"LogicalWeights" -> <||>,
			"ReshapeTemplate" -> None,
			"NodeCount" -> dm["nodes"]
		|>],
		1, "Context" -> device, "ArrayCaching" -> True
	];
	port = ToExpression@StringDelete[ToString[exe["Arrays", "Inputs", "Input"]], {"NDArray[", "]"}];
	MXNetLink`NDArray`PackagePrivate`mxWritePackedArrayToNDArrayChecked[#, port];
	NeuralNetworks`NetExecutorForward[exe, False];
	exe["Arrays", "Outputs", "Output"] // MXNetLink`NDArrayGetFlat
]&;


(* ::Subsubsection::Closed:: *)
(*ClassificationBenchmark*)
ClassificationBenchmark[model_, data_List] := ClassificationEvaluate[model, data];
ClassificationBenchmark[cm_ClassifierMeasurementsObject, name_String : Missing[]] := Block[
	{date, cm, net, report},
	ClassifyProbabilitiesPlot@cm;
	<|
		If[MissingQ@name, "Name" -> ToString@Hash@cm, "Name" -> name],
		"Date" -> DateString[],
		"Task" -> "Classification",
		NetAnalyze[First[First[cm]["Model"]]["Model", "Net"]],
		ClassificationSpeed@cm,
		ClassifyIndicatorAnalyze@cm,
		ClassifyDualAnalyze@cm,
		ClassifyUncertaintyAnalyzeThenPlot@cm,
		ClassifyConfusionAnalyzeThenPlot@cm
	|>
];
ClassificationBenchmark[attr_Association, opts__] := AssociateTo[attr, {opts}];
ClassificationBenchmark[path_String, analyze_Association] := Block[
	{fix, text},
	fix = GeneralUtilities`TextString`PackagePrivate`fmtReal[a_, b_] :> ExportString[a, "JSON"];
	text = Quiet@Dataset`DatasetJSONString[Dataset[analyze]] /. fix;
	Export[path, StringReplace[text, {", {" -> ",\n {", "], [" -> "], \n["}], "Text"]
];


NetPlotInformation[net_NetChain, opts : OptionsPattern[]] := NetPlotInformation[MXNetLink`ToMXJSON[net]["JSON"], opts];
NetPlotInformation[net_NetGraph, opts : OptionsPattern[]] := NetPlotInformation[MXNetLink`ToMXJSON[net]["JSON"], opts];
NetPlotInformation[json_String, opts : OptionsPattern[]] := NetPlotInfo[Developer`ReadRawJSONString[json], opts];



(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]