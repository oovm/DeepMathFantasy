Inputs:
	$Input: ChannelT[$$InputChannels, TensorT[$$InputSize]]
	$InputMask: ChannelT[$$InputChannels, TensorT[$$InputSize]]

Outputs:
    $Output: ChannelT[$OutputChannels, TensorT[$$OutputSize]]
	$OutputMask: ChannelT[$OutputChannels, TensorT[$$OutputSize]]

Arrays:
	$Weights: TensorT[{$OutputChannels, $$InputChannels}, TensorT[$KernelSize]]
	$Biases: Nullable[VectorT[$OutputChannels]]

Parameters:
	$OutputChannels:	SizeT
	$KernelSize:		ArbSizeListT[$Dimensionality, SizeT, None]
	$MaskFunction:		Defaulting[EnumT[{Mean, Sum}], Mean]
	$Stride:			ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$PaddingSize:		ArbSizeListT[$Dimensionality, NaturalT,    0]
	$Dilation:			ArbSizeListT[$Dimensionality, PosIntegerT, 1]
	$Dimensionality:	NaturalT
	$$InputChannels:	SizeT
	$$InputSize:		SizeListT[$Dimensionality]
	$$OutputSize:		ComputedType[SizeListT[$Dimensionality], 
		MaybeDyn @ ConvolutionShape[$$InputSize, $PaddingSize, $KernelSize, $Stride, $Dilation]
	]

ReshapeParams: {$$InputChannels, $$InputSize, $$OutputSize}

MinArgCount: 0
PosArgCount: 2

AllowDynamicDimensions: True

Writer: Function[
	image = GetInput["Input"];
	mask = GetInput["InputMask"];
	norm = SowNode["broadcast_div", {
		SowNode["broadcast_mul", {image, mask}],
		SowNode["mean", mask, "axis" -> {2, 3}, "keepdims" -> True];
	}];
	image = SowNode["Convolution", norm,
		"num_filter" -> #OutputChannels,
		"kernel" -> #KernelSize,
		"dilate" -> #Dilation,
		"pad" -> #PaddingSize,
		"stride" -> #Stride,
		"weight" -> #Weights,
		"bias" -> #Biases
	];
	mask = SowNode["Convolution", mask,
		"num_filter" -> #OutputChannels,
		"kernel" -> #KernelSize,
		"dilate" -> #Dilation,
		"pad" -> #PaddingSize,
		"stride" -> #Stride,
		"weight" -> #Weights,
		"bias" -> None
	];
	mask = SowNode["broadcast_greater", {mask, 0}];
	SetOutput["Output", image];
	SetOutput["OutputMask", mask];
]

Suffix: "Layer"