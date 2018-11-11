(* ::Package:: *)
(* ::Subsection:: *)
(*Defines*)
VggBasicBN::usage = "";
VggBasic::usage = "";
VggForge::usage = "";
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

(*
VGG number
19 : {0, 2, 4, 8, 12, 19 - 3}
16 : {0, 2, 4, 7, 10, 16 - 3}
13 : {0, 2, 4, 6, 8,  13 - 3}
11 : {0, 1, 2, 4, 6,  11 - 3}
*)
(*
Vgg11 := Defer[NetChain][{N,
	Defer[VggBlock][64, 1, "BN"], N,
	Defer[VggBlock][128, 1, "BN"], N,
	Defer[VggBlock][256, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	class, N
}] // PrintLine;
Vgg13 := Defer[NetChain][{N,
	Defer[VggBlock][64, 2, "BN"], N,
	Defer[VggBlock][128, 2, "BN"], N,
	Defer[VggBlock][256, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	Defer[VggBlock][512, 2, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	class, N
}] // PrintLine;
Vgg16 := Defer[NetChain][{N,
	Defer[VggBlock][64, 2, "BN"], N,
	Defer[VggBlock][128, 3, "BN"], N,
	Defer[VggBlock][256, 3, "BN"], N,
	Defer[VggBlock][512, 3, "BN"], N,
	Defer[VggBlock][512, 3, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	class, N
}] // PrintLine;
Vgg19 := Defer[NetChain][{N,
	Defer[VggBlock][64, 2, "BN"], N,
	Defer[VggBlock][128, 2, "BN"], N,
	Defer[VggBlock][256, 4, "BN"], N,
	Defer[VggBlock][512, 4, "BN"], N,
	Defer[VggBlock][512, 4, "BN"], N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
	class, N
}] // PrintLine;
*)

Options[VggForge] = {
	"Layers" -> {1, 1, 1, 1, 1},
	"Channels" -> {64, 128, 256, 512, 512},
	"Classes" -> 1000,
	"Layout" -> "BN",
	"Name" -> "Auto"
};
VggForge[ops : OptionsPattern[]] := Module[
	{layers, layout, channels, classes},
	layers = Switch[
		ToLowerCase@OptionValue["Name"],
		"vgg11", {1, 1, 2, 2, 2},
		"vgg13", {2, 2, 2, 2, 2},
		"vgg16", {2, 2, 3, 3, 3},
		"vgg19", {2, 2, 4, 4, 4},
		"auto", OptionValue["Layers"],
		_, (*Todo:FailHelper*)
		Return@"no such model"
	];
	{layout, channels, classes} = OptionValue[{"Layout", "Channels", "Classes"}];
	(*Todo:FinalCheck*)
	
	
	
	Defer[NetChain][{N,
		Sequence @@ Flatten@Inner[{Defer[VggBlock][#1, #2, layout], N}&, channels, layers, List],
		{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
		{4096, Ramp, Defer[DropoutLayer[0.5]]}, N,
		classes, Defer[SoftmaxLayer][], N
	}] // PrintLine;
];


(* ::Subsection:: *)
(*Additional*)
SetAttributes[
	{VggBasic, VggBasicBN, VggForge},
	{Protected, ReadProtected}
];
End[]