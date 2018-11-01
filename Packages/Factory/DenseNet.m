(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Defines*)
denseBlock::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`DenseNet`"];
(* ::Subsubsection:: *)
(*DenseNet*)
BN[p__ : Nothing] := BatchNormalizationLayer["Epsilon" -> 1*^-5, p]
CN[c_, k_, p_ : 1, s_ : 1] := ConvolutionLayer[
	c, {k, k}, "Biases" -> None,
	"PaddingSize" -> p, "Stride" -> s
];
denseBasic[c_Integer] := Block[
	{res},
	res = NetChain@{
		BN[], ElementwiseLayer["ReLU"],
		CN[c, 1, 0, 1],
		BN[], ElementwiseLayer["ReLU"],
		CN[c, 3, 1, 1]
	};
	NetMerge[res, Join, Expand -> All]
];
denseResample[c_Integer] := NetChain[{
	BN[], ElementwiseLayer["ReLU"],
	CN[c, 1, 0, 1],
	PoolingLayer[{2, 2}, "Stride" -> 2, "Function" -> Mean]
}];
denseBlock[c_Integer, n_Integer, head_ : True] := Block[
	{chain},
	chain = ConstantArray[denseBasic[c], n];
	If[head, PrependTo[chain, denseResample[c]]];
	NetChain@chain
];

(*TODO:PRelu for SR*)
CN2[c_, k_, p_ : 1, s_ : 1] := ConvolutionLayer[
	c, {k, k}, "PaddingSize" -> p, "Stride" -> s
];
denseBasicSR[c_Integer] := Block[
	{res},
	res = NetChain@{
		ParametricRampLayer[],
		CN2[c, 1, 0, 1],
		ParametricRampLayer[],
		CN2[c, 3, 1, 1]
	};
	NetMerge[res, Join, Expand -> All]
];
denseResampleSR[c_Integer] := NetChain[{
	BN[], ElementwiseLayer["ReLU"],
	CN[c, 1, 0, 1],
	PoolingLayer[{2, 2}, "Stride" -> 2, "Function" -> Mean]
}];



(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]