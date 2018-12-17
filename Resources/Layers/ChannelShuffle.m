Input: ChannelT[$$Channels, TensorT[$$Dimensions]]

Output: ChannelT[$$Channels, TensorT[$$Dimensions]]

Parameters:
	$Group: PosIntegerT
	$$Channels: SizeT
	$$Dimensions: SizeListT[2]

Writer: Function[
	input = GetInput["Input", "Batchwise"];
	index = SowNode["reshape", input, "shape" -> {0, -4, #Group, -1, -2}];
	index = SowNode["SwapAxes", index, "dim1" -> "1", "dim2" -> "2"];
	index = SowNode["reshape", index, "axes" -> {0, -3, -2}];
	SetOutput["Output", index]
]

Suffix: "Layer"