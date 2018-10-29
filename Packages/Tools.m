(* ::Package:: *)
(* ::Title:: *)
(*Tools*)
(* ::Subchapter:: *)
(*Introduce*)
NetChain2Graph::usage = "Transform a NetChain to NetGraph.";
ImageEncoder::usage = "";
LayerRemoveShape::usage = "Try to remove the shape of the layer";
LayerInformation::usage = "Try to get basic information of the layer";
NetMerge::usage = "Merge net nodes";
MXNet$Bind::usage = "Import and Bind the MX-Symbol and MX-NDArray";
MXNet$Boost::usage = "A Function which call a mxnet evaluation";
ClassificationBenchmark::usage = "";
ClassificationInformation::usage = "";
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
NetChain2Graph[other___] := other;
NetChain2Graph[net_NetChain] := Block[
	{nets = Normal@net},
	NetGraph[nets,
		Rule @@@ Partition[Range@Length@nets, 2, 1],
		"Input" -> NetExtract[net, "Input"],
		"Output" -> NetExtract[net, "Output"]
	];
];


(* ::Subsubsection:: *)
(*NetMerge*)
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
Options[NetMerge] = {Identity -> True, Expand -> False};
NetMerge[nodes_, op_, OptionsPattern[]] := Block[
	{net},
	If[ListQ@nodes,
		net = If[
			True@OptionValue[Identity],
			netMergeIdentity[nodes, op],
			netMerge[nodes, op]
		],
		net = netMergeIdentity[{nodes}, op]
	];
	Switch[OptionValue[Expand],
		False, NetFlatten[net],
		True, NetFlatten[net],
		___, NetFlatten[NetChain2Graph /@ net]
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
	"Array" -> {"Weights", "Biases"},
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
pplot = First@# <> ": " <> StringRiffle[Last@#, "*"]&;
getActivationFunction[f_] := Switch[f,
	Ramp , "ReLU",
	"RectifiedLinearUnit"[#1]&, "ReLU",
	LogisticSigmoid, "Sigmoid",
	___, "Function"
]




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



ClassificationInformation[net_] := {
	"Input" -> NetExtract[net, 1][["Input"]],
	"ParametersCount" -> NetInformation[net, "ArraysTotalElementCount"],
	"ModelSize" -> First@UnitConvert[NetInformation[net, "ArraysTotalSize"], "Megabytes"],
	"Nodes" -> NetInformation[net, "LayersCount"]
};
Options[] = {};
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

(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]