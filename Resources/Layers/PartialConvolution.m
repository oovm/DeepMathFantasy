Inputs:
	$Input: InterleavingSwitchedT[$$Interleaving, $$InputChannels, $$InputSize]
	$InputMask: InterleavingSwitchedT[$$Interleaving, 1, $$InputSize]
	
Outputs:
	$Output: InterleavingSwitchedT[$$Interleaving, $OutputChannels, $$OutputSize]
	$OutputMask: InterleavingSwitchedT[$$Interleaving, 1, $$OutputSize]

Arrays:
	$Weights: TensorT[{$OutputChannels, $$InputChannels}, TensorT[$KernelSize]]
	$Biases: Nullable[VectorT[$OutputChannels]]

Parameters:
	$OutputChannels:	SizeT
	$KernelSize:		ArbSizeListT[$$Dimensionality, SizeT, None]
	$MaskFunction:		Defaulting[EnumT[{Mean, Sum}], Mean]
	$Stride:			ArbSizeListT[$$Dimensionality, PosIntegerT, 1]
	$PaddingSize:		ArbSizeListT[$$Dimensionality, NaturalT,	0]
	$Dilation:			ArbSizeListT[$$Dimensionality, PosIntegerT, 1]
	$$Interleaving:		Defaulting[BooleanT, False]
	$$Dimensionality:	NaturalT
	$$InputChannels:	SizeT
	$$InputSize:		SizeListT[$$Dimensionality]
	$$OutputSize:		ComputedType[SizeListT[$$Dimensionality],
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
		SowNode["mean", mask, "axis" -> {2, 3}, "keepdims" -> True]
	}];
	image = SowNode["Convolution",
		{norm, #Weights, Replace[#Biases, None -> Nothing]},
		"kernel" -> #KernelSize,
		"num_filter" -> #OutputChannels,
		"dilate" -> #Dilation,
		"pad" -> #PaddingSize,
		"stride" -> #Stride
	];
	SetOutput["Output", image];
	mask = SowNode["repeat", mask, "repeats" -> #$InputChannels, "axis" -> 1];
	mask = SowNode["Convolution",
		{mask, #Weights},
		"no_bias"->True,
		"num_filter" -> #OutputChannels,
		"kernel" -> #KernelSize,
		"dilate" -> #Dilation,
		"pad" -> #PaddingSize,
		"stride" -> #Stride
	];
	mask = SowNode["sum", mask, "axis" -> 1, "keepdims" -> True];
	mask = SowNode["broadcast_greater", {mask, SowZeroArray[{1}]}];
	SetOutput["OutputMask", mask];
]

Suffix: "Layer"