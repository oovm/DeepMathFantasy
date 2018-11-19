Input: ChannelT[$$Channels, TensorT[$$InputDimensions]]
Output: ChannelT[$$Channels, TensorT[$$InputDimensions]]

Parameters:
	$Epsilon: Defaulting[ScalarT, 10^-8]
	$Gamma: ???
	$Beta: ???
	$$Channels: SizeT
	$$InputDimensions: SizeListT[SizeT]

(*
	u = Mean[in, -1, keepdim]
	s = Mean[(in - u)^2, -1, keepdim]
	n = (in - u) RSqrt[s + $eps]
	Return[n $gamma + $beta]
*)




Writer: Function[
	input = GetInput["Input", "Batchwise"];
	(*class mxnet.gluon.nn.LayerNorm*)
	SetOutput["Output", output]
]

Suffix: "alizationLayer"