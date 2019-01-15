Input: TensorT[$$Dimensions]
Output: TensorT[$$Dimensions]

Parameters:
	$Slope: Defaulting[RealT, 0.2]
	$$Dimensions: SizeListT[]

AllowDynamicDimensions: True

Writer: Function[
	input = GetInput["Input"];
	path = SowNode["LeakyReLU", input,"act_type" -> "leaky","slope" -> #Slope];
	SetOutput["Output", path];
]

(*
MXNet:
	Name: "LeakyReLU"
	Writer: Function[{
		"act_type" -> "leaky",
		"slope" -> #Slope
	}]
*)

Suffix: ""