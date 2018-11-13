Inputs:
	$Input: TensorT[$$Dimensions]
	$Target: TensorT[$$Dimensions]

Outputs:
	$Loss: ScalarT

Parameters:
	$$Dimensions: SizeListT[]

AllowDynamicDimensions: True

Writer: Function[
	input = GetInput["Input"];
	target = GetInput["Target"];
	loss = SowNode["elemwise_sub", {input, target}];
	loss = SowNode["mean", SowSquare@loss];
	loss = SowNode["log10", loss];
	loss = SowNode["_mul_scalar", loss, "scalar" -> -10];
	SetOutput["Loss", loss];
]

IsLoss: True

Suffix: "Layer"