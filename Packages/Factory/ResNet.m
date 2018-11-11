(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Defines*)
ResBlockV2::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`ResNet`"];
(* ::Subsubsection:: *)
(*ResNetV2*)
BN[p__ : Nothing] := BatchNormalizationLayer["Epsilon" -> 1*^-5, p]
CN[c_, k_, p_ : 1, s_ : 1, ops : OptionsPattern[]] := ConvolutionLayer[
	c, {k, k}, "PaddingSize" -> p, "Stride" -> s, ops
];
ResResampleV2[c_Integer] := Block[
	{res},
	res = NetChain@{
		BN[], ElementwiseLayer["ReLU"],
		CN[c, 3, 1, 2, "Biases" -> None],
		BN[], ElementwiseLayer["ReLU"],
		CN[c, 3, 1, 1, "Biases" -> None]
	};
	NetMerge[{CN[c, 1, 0, 2], res}, Plus, Expand -> All]
];
ResBasicV2[c_Integer] := Block[
	{res},
	res = NetChain@{
		BN[], ElementwiseLayer["ReLU"],
		CN[c, 3, 1, 1, "Biases" -> None],
		BN[], ElementwiseLayer["ReLU"],
		CN[c, 3, 1, 1, "Biases" -> None]
	};
	NetMerge[res, Plus, Expand -> All]
];
ResBlockV2[c_Integer, n_Integer, head_ : True] := Block[
	{chain},
	chain = ConstantArray[ResBasicV2[c], n];
	If[head, PrependTo[chain, ResResampleV2[c]]];
	NetChain@chain
];

(*TODO:PRelu for SR*)

ResBasicSR[c_Integer] := Block[
	{res},
	res = NetChain@{
		CN[c, 3, 1, 1],
		ParametricRampLayer[],
		CN[c, 3, 1, 1]
	};
	NetMerge[res, Plus, Expand -> All]
];



ResResampleSR[c_Integer, s_Integer] := NetChain@{
	CN[c * s^2, 3, 1, 1],
	PixelShuffleLayer[s],
	ParametricRampLayer[]
};


(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]