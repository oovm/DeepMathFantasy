(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Defines*)
VggBasicBN::usage = "";
VggBasic::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`VggNet`"];
VggBasic[c_Integer, u_Integer] := Block[
	{unit, pool},
	unit = {
		ConvolutionLayer[c, {3, 3}, "PaddingSize" -> 1, "Stride" -> 1],
		ElementwiseLayer["ReLU"]
	};
	pool = PoolingLayer[{2, 2}, "Stride" -> 2];
	NetChain@Flatten[{ConstantArray[unit, u], pool}]
];
VggBasicBN[c_Integer, u_Integer] := Block[
	{unit, pool},
	unit = {
		ConvolutionLayer[c, {3, 3}, "PaddingSize" -> 1, "Stride" -> 1],
		BatchNormalizationLayer["Epsilon" -> 1*^-5],
		ElementwiseLayer["ReLU"]
	};
	pool = PoolingLayer[{2, 2}, "Stride" -> 2];
	NetChain@Flatten[{ConstantArray[unit, u], pool}]
];

VggBlock[c_Integer, u_Integer : 1, m_String : ""] := Block[
	{},
	If[Or[c < 1, u < 1], Return@GluonCV`helper`paraErr];
	Switch[m,
		"BN", VggBasicBN[c, u],
		___, VggBasic[c, u]
	]
];

(*
VGG number
19: 2 4 8 12 19-3=16
16: 2 4 7 10 16-3=13
13: 2 4 6 8  13-3=10
11: 1 2 4 6  11-3=8
*)
Vgg11 := Defer[NetChain][{N,
	Defer[VggBlock][64, 1, "BN"], N,
	Defer[VggBlock][128, 1, "BN"], N,
	Defer[VggBlock][256, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	1000, N
}] // PrintLine;

Vgg13 := Defer[NetChain][{N,
	Defer[VggBlock][64, 2, "BN"], N,
	Defer[VggBlock][128, 2, "BN"], N,
	Defer[VggBlock][256, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	1000, N
}] // PrintLine;

Vgg16 := Defer[NetChain][{N,
	Defer[VggBlock][64, 2, "BN"], N,
	Defer[VggBlock][128, 2, "BN"], N,
	Defer[VggBlock][256, 4, "BN"], N,
	Defer[VggBlock][512, 4, "BN"], N,
	Defer[VggBlock][512, 4, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	1000, N
}] // PrintLine;


Vgg19 := Defer[NetChain][{N,
	Defer[VggBlock][64, 2, "BN"], N,
	Defer[VggBlock][128, 2, "BN"], N,
	Defer[VggBlock][256, 3, "BN"], N,
	Defer[VggBlock][512, 3, "BN"], N,
	Defer[VggBlock][512, 3, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	1000, N
}] // PrintLine;



(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]