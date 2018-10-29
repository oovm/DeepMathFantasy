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
CN[c_, k_, p_ : 1, s_ : 1] := ConvolutionLayer[
	c, {k, k}, "Biases" -> None,
	"PaddingSize" -> p, "Stride" -> s
];
ResSampleV2[c_Integer] := Block[
	{res},
	res = NetChain@{
		BN[], Ramp,
		CN[c, 3, 1, 2],
		BN[], Ramp,
		CN[c, 3, 1, 1]
	};
	NetMerge[{CN[c, 1, 0, 2], res}, Plus, Expand -> All]
];
ResBasicV2[c_Integer] := Block[
	{res},
	res = NetChain@{
		BN[], Ramp, CN[c, 3],
		BN[], Ramp, CN[c, 3]
	};
	NetMerge[res, Plus, Expand -> All]
];
ResBlockV2[c_Integer, n_Integer, head_ : True] := Block[
	{chain},
	chain = ConstantArray[ResBasicV2[c], n];
	If[head, PrependTo[chain, ResSampleV2[c]]];
	NetChain@chain
];


(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]