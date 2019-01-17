Input: ChannelT[$Channels, TensorT[$$InputSize]]
Output: ChannelT[$Channels, TensorT[$$OutputSize]]

Arrays:
	$Weights: TensorT[{$Channels, 1}, TensorT[$KernelSize]]
	$Biases: Nullable[VectorT[$Channels]]

Parameters:
	$Channels:			SizeT
	$KernelSize:		ArbSizeListT[$Dimensionality, SizeT, None]
	$Stride:			ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$PaddingSize:		ArbSizeListT[$Dimensionality, NaturalT,    0]
	$Dilation:			ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$Dimensionality:	Defaulting[NaturalT, 2]
	$$InputSize:		SizeListT[$Dimensionality]
	$$OutputSize:		ComputedType[
		SizeListT[2],
		MaybeDyn @ ConvolutionShape[$$InputSize, $PaddingSize, $KernelSize, $Stride, $Dilation]
	]

ReshapeParams: {$Channels, $$InputSize, $$OutputSize}

MinArgCount: 0
PosArgCount: 2

AllowDynamicDimensions: True

Constraints: SpatialConstraintGenerator[getFirstConvOut]

getFirstConvOut = Function[
	ConvolutionShape[#2, First @ #PaddingSize, First @ #KernelSize, First @ #Stride, First @ #Dilation]
];

Writer: Function[
	input = GetInput["Input"];
	path = SowNode["Convolution",
		{input, #Weights, Replace[#Biases, None -> Nothing]},
		"kernel" -> #KernelSize,
		"no_bias" -> If[#Biases === None, True, False],
		"num_filter" -> #Channels,
		"num_group" -> #Channels,
		"dilate" -> #Dilation,
		"pad" -> #PaddingSize,
		"stride" -> #Stride
	];
	SetOutput["Output", path];
]

(*
MXNet:
	Name: "Convolution"
	Parameters: 
		$Channels: "num_filter"
		$KernelSize: "kernel"
		$Dilation: "dilate"
		$PaddingSize: "pad"
		$Stride: "stride"
		$$GroupNumber: "num_group"
	Arrays:
		$Weights: "weight"
		$Biases: "bias"
	Writer: Function[{
		"no_bias" -> If[#2["Biases"] === None, "True", "False"],
		"layout" -> mxConvLayout[#1]
	}]
*)

Suffix: ""