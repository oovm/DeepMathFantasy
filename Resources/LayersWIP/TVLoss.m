Inputs:
	$Input: TensorT[$$Dimensions]
	$Target: TensorT[$$Dimensions]

Outputs:
	$Loss: ScalarT

Parameters:
	$$Dimensions: SizeListT[]

AllowDynamicDimensions: True

IsLoss: True

Writer: Function[
	up
	down
	
	left
	right
	
	xloss
	yloss
]

