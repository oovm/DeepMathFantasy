Servent$MXNet::usage = "";
Geass$MXNet::usage = "";

(* ::Subsubsection:: *)
(*功能块 1*)
Servent$MXNet[pathJ_, pathP_] := Block[
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
(*功能块 2*)
Options[Geass$MXNet] = {TargetDevice -> "GPU"};
Geass$MXNet[dm_Association, OptionsPattern[]] := Block[
	{exe, device, port},
	device = NeuralNetworks`Private`ParseContext @OptionValue[TargetDevice];
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


(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
