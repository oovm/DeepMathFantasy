Input: ChannelT[$$Channels, TensorT[$$InputDimensions]]
Output: ChannelT[$$Channels, TensorT[$$InputDimensions]]

Parameters:
	$Epsilon: Defaulting[ScalarT, 10^-8]
	$$Channels: SizeT
	$$InputDimensions: SizeListT[SizeT]

Writer: Function[
	input = GetInput["Input", "Batchwise"];
	path = SowNode["square", input];
	path = SowNode["mean", path, "axis" -> 1, "keepdims" -> True];
	path = SowNode["_PlusScalar", path, "scalar" -> #Epsilon];
	path = SowNode["rsqrt", path];
	output = SowNode["broadcast_mul", {input, path}];
	SetOutput["Output", output]
]

Suffix: "alizationLayer"