Inputs:
	$Content: ChannelT[$$Channels, TensorT[$$Dimensions]]
	$Style: ChannelT[$$Channels, TensorT[$$Dimensions]]


Outputs:
	$Output: ChannelT[$$Channels, TensorT[$$Dimensions]]

Parameters:
	$Epsilon: Defaulting[ScalarT, 10^-5]
	$$Channels: SizeT
	$$Dimensions: SizeListT[2]


AllowDynamicDimensions: True

(*
SowMeanSigma[input_, eps_] := Scope[
	mean = SowNode["mean", input, "axis" -> {2, 3}, "keepdims" -> True];
	var = SowSquare@SowBMinus[input, mean];
	var = SowNode["mean", var, "axis" -> {2, 3}, "keepdims" -> True];
	sigma = SowSqrt@SowBPlus[var, eps];
	Return@{mean, sigma}
]
*)

GetMean[input_] := SowNode["mean", input, "axis" -> {2, 3}, "keepdims" -> True];
GetSigma[input_, mean_, eps_] := Scope[
	sub = SowSquare@SowBMinus[input, mean];
	var = SowNode["mean", sub, "axis" -> {2, 3}, "keepdims" -> True];
	SowSqrt@SowPlusScalar[var, eps]
];

Writer: Function[
	ctx = GetInput["Content", "Batchwise"];
	sty = GetInput["Style", "Batchwise"];
	{sMean, cMean} = GetMean /@ {sty, ctx};
	sSigma = GetSigma[sty, sMean, #Epsilon];
	cSigma = GetSigma[ctx, cMean, #Epsilon];
	sub = SowBMinus[ctx, cMean];
	div = SowBDivide[sSigma, cSigma];
	out = SowBPlus[SowBHad[sub, div], sMean];
	SetOutput["Output", out];
]


Suffix: "Layer"