Package["NeuralNetworks`"]

PackageScope["SowCast"]
SowCast[a_, fromType_, toType_] := Scope[
	fType = ToDataTypeCode[fromType];
	tType = ToDataTypeCode[toType];
	If[fType =!= tType,
		SowNode["cast", a, "dtype" -> ToMXNetDataTypeName[tType]],
		a
	]
]

SowCast[a_List, fromType_, toType_] := SowCast[#, fromType, toType]& /@ a

PackageScope["SowIdentity"]
SowIdentity[a_] := SowNode["identity", a];

PackageScope["SowMinScalar"]
SowMinScalar[a_, s_] := SowNode["_minimum_scalar", a, "scalar" -> N[s]];

PackageScope["SowMaxScalar"]
SowMaxScalar[a_, s_] := SowNode["_maximum_scalar", a, "scalar" -> N[s]];

PackageScope["SowMaxEps"]
SowMaxEps[a_] := SowMaxScalar[a, "0.2220446049250313e-15"];

PackageScope["SowMinus"]
SowMinus[a_] := SowNode["_RMinusScalar", a, "scalar" -> "0.0"];

PackageScope["SowOneMinus"]
SowOneMinus[a_] := SowNode["_RMinusScalar", a, "scalar" -> "1.0"];

PackageScope["SowLog"]
SowLog[a_] := SowNode["log", a];

PackageScope["SowSafeLog"]
SowSafeLog[a_] := SowNode["log", SowMaxEps[a]];

PackageScope["SowExp"]
SowExp[a_] := SowNode["exp", a];

PackageScope["SowSoftmax"]
SowSoftmax[a_, axis_ : -1] := SowNode["softmax", a, "axis" -> axis];

PackageScope["SowPlusEps"]
SowPlusEps[a_] := SowNode["_PlusScalar", a, "scalar" -> "0.2220446049250313e-15"];

PackageScope["SowMix"]
SowMix[a_, b_, scale_] := SowPlus[SowHad[a, SowOneMinus[scale]], SowHad[b, scale]];

PackageScope["SowPlusScalar"]
SowPlusScalar[a_, n_] := SowNode["_PlusScalar", a, "scalar" -> N[n]];

PackageScope["SowMinusScalar"]
SowMinusScalar[a_, n_] := SowNode["_MinusScalar", a, "scalar" -> N[n]];

PackageScope["SowTimesScalar"]
SowTimesScalar[a_, n_] := SowNode["_MulScalar", a, "scalar" -> N[n]];
SowTimesScalar[a_, 1. | 1] := a;

PackageScope["SowDropConnect"]
SowDropConnect[in_, p_] := SowHad[in, SowDropConnect[p]];
SowDropConnect[p_] := SowTimesScalar[SowNode["round",
	SowNode["random_uniform", {},
		"low" -> CDoubleString[(1 - p) / 2.],
		"high" -> CDoubleString[0.5 + (1 - p) / 2.],
		"dtype" -> $DTypeMXName]],
	1. / (1 - p)];

PackageScope["SowDropout"]
SowDropout[in_, p_] := SowDropout[in, p, {}];
SowDropout[in_, 0., _] := in;
SowDropout[in_, p_, axes_] := SowNode["Dropout", in, "p" -> N[p], "mode" -> "always", "axes" -> axes];
(* until MXNet fixes the issue behind incubator-mxnet/issues/13264, the is_train
flag must be forced-on to prevent problems when requesting grads.
also, there is a separate issue, which is that subgraphs in mxnet use needs_grad
for their value of is_train, so dropout fails to work in subgraphs unless gradients
are requested. hence we just force dropout always-on and simply don't sow these nodes
if TMode is false *)

PackageScope["SowDivideScalar"]
SowDivideScalar[a_, n_] := SowNode["_DivScalar", a, "scalar" -> N[n]];

PackageScope["SowHad"]
PackageScope["SowDivide"]
SowDivide[a_, b_] := SowNode["_Div", {a, b}];
SowHad[a_, b_] := SowNode["_Mul", {a, b}];

PackageScope["SowBHad"]
PackageScope["SowBDivide"]
SowBHad[a_, b_] := SowNode["broadcast_mul", {a, b}];
SowBDivide[a_, b_] := SowNode["broadcast_div", {a, b}];

PackageScope["SowBPlus"]
PackageScope["SowBMinus"]
SowBPlus[a_, b_] := SowNode["broadcast_plus", {a, b}];
SowBMinus[a_, b_] := SowNode["broadcast_minus", {a, b}];

PackageScope["SowFC"]
SowFC[x_, w_, b_, numHidden_] := SowNode["FullyConnected", {x, w, b}, "num_hidden" -> numHidden];
SowFC[x_, w_, None, numHidden_] := SowNode["FullyConnected", {x, w}, "num_hidden" -> numHidden, "no_bias" -> True];

PackageScope["SowMappedFC"]
SowMappedFC[x_, w_, b_, numHidden_] := SowNode["FullyConnected", {x, w, b}, "num_hidden" -> numHidden, "flatten" -> False];
SowMappedFC[x_, w_, None, numHidden_] := SowNode["FullyConnected", {x, w}, "num_hidden" -> numHidden, "no_bias" -> True, "flatten" -> False];

PackageScope["SowTanh"]
SowTanh[a_] := SowNode["tanh", a];

PackageScope["SowHardTanh"]
SowHardTanh[a_] := SowPlusScalar[SowNode["_Minus", {SowRamp @ SowPlusScalar[a, "1.0"], SowRamp @ SowPlusScalar[a, "-1.0"]}], "-1.0"];

PackageScope["SowSigmoid"]
SowSigmoid[a_] := SowNode["sigmoid", a];

PackageScope["SowRamp"]
SowRamp[a_] := SowNode["relu", a];

PackageScope["SowHardSigmoid"]
SowHardSigmoid[a_] := SowTimesScalar[SowMinus[SowRamp @ SowPlusScalar[a, "1.0"], SowRamp @ SowPlusScalar[a, "-1.0"]], "0.5"];

PackageScope["SowPlus"]
SowPlus[a_, b_] := SowNode["_Plus", {a, b}];
SowPlus[a_, b_, c_] := SowNode["ElementWiseSum", {a, b, c}, "num_args" -> 3];
SowPlus[a_, b_, c_, d_] := SowNode["ElementWiseSum", {a, b, c, d}, "num_args" -> 4];

PackageScope["SowBPlus"]
SowBPlus[a_, b_] := SowNode["broadcast_add", {a, b}];

PackageScope["SowMinus"]
SowMinus[a_, b_] := SowNode["_Minus", {a, b}];

PackageScope["SowSum"]
SowSum[a_, 0] := a;
SowSum[a_, rank_] := SowNode["sum", a, "axis" -> Range[rank], "keepdims" -> "false"];

PackageScope["SowSquare"]
SowSquare[a_] := SowNode["square", a];

PackageScope["SowSqrt"]
SowSqrt[a_] := SowNode["sqrt", a];

PackageScope["SowRSqrt"]
SowRSqrt[a_] := SowNode["rsqrt", a];

PackageScope["SowL2Normalize"]
SowL2Normalize[a_] := SowNode["L2Normalization", a];

PackageScope["SowCosineDistance"]
SowCosineDistance[a_, b_, rank_] :=
	SowOneMinus @ SowSum[SowHad[SowL2Normalize[a], SowL2Normalize[b]], rank];

PackageScope["SowEuclideanDistance"]
SowEuclideanDistance[a_, b_, rank_] :=
	SowSum[SowSquare[SowMinus[a, b]], rank];

PackageScope["SowSoftmax"]
SowSoftmax[a_, axis_ : -1] := SowNode["softmax", a, "axis" -> axis];

PackageScope["SowLogSoftmax"]
SowLogSoftmax[a_, axis_ : -1] := SowNode["log_softmax", a, "axis" -> axis];

PackageScope["SowMarginLoss"]
SowMarginLoss[a_, margin_] := SowNode["relu", SowNode["_RMinusScalar", a, "scalar" -> margin]];

PackageScope["SowReshape"]
SowReshape[in_, dims___] := SowNode["reshape", in, "shape" -> Flatten[{-1, dims}]];

PackageScope["SowUReshape"]
SowUReshape[in_, dims___] := SowNode["reshape", in, "shape" -> Flatten[{dims}]];

PackageScope["SowTranspose"]
SowTranspose[in_, axes_ : {0, 2, 1}] := SowNode["transpose", in, "axes" -> axes];

PackageScope["SowTransposeLast"]
SowTransposeLast[in_, 1] := in;
SowTransposeLast[in_, rank_] := SowTranspose[in, Join[{0, rank}, Range[rank - 1]]];

PackageScope["SowTranspose01"]
SowTranspose01[in_] := SowSwapAxis[in, 0, 1];

PackageScope["SowSwapAxis"]
SowSwapAxis[in_, d1_, d2_] := If[d1 === d2, in, SowNode["SwapAxis", in, "dim1" -> d1, "dim2" -> d2]];

PackageScope["SowSumAxis"]
SowSumAxis[a_, axes_] := SowNode["sum", a, "axis" -> axes, "keepdims" -> "false"];
SowSumAxis[a_, {}] := a; (* This is here because MXNet might be stupid *)

PackageScope["SowMeanAxis"]
SowMeanAxis[a_, axes_] := SowNode["mean", a, "axis" -> axes, "keepdims" -> "false"];
SowMeanAxis[a_, {}] := a; (* This is here because MXNet might be stupid *)

PackageScope["SowProdAxis"]
SowProdAxis[a_, axes_] := SowNode["prod", a, "axis" -> axes, "keepdims" -> "false"];
SowProdAxis[a_, {}] := a;

PackageScope["SowMaxAxis"]
SowMaxAxis[a_, axes_] := SowNode["max", a, "axis" -> axes, "keepdims" -> "false"];
SowMaxAxis[a_, {}] := a;

PackageScope["SowPack"]
SowPack[elems_List, timewise_] := Scope[
	len = Length[elems];
	axis = If[timewise, 0, 1];
	elems = SowInsertDim[#, axis]& /@ elems;
	SowNode["concat", elems, "num_args" -> len, "dim" -> axis]
];

SowPack[__] := $Unreachable;

PackageScope["SowUnpack"]
SowUnpack[node_, numOut_, axis_] := Scope[
	out = SowNode["split", node, "axis" -> axis, "num_outputs" -> numOut, "squeeze_axis" -> "true"];
	out[[2]] = Range[numOut] - 1;
	Thread[out]
]

PackageScope["SowFlatMean"]
(* sums away everything but the 0th axis, assumed to be batchwise *)
SowFlatMean[node_, keepAxis_ : 0] := SowNode["mean", node, "axis" -> keepAxis, "keepdims" -> False, "exclude" -> True];

PackageScope["SowFlatProduct"]
(* multiplies away everything but the 0th axis, assumed to be batchwise *)
SowFlatProduct[node_, keepAxis_ : 0] := SowNode["prod", node, "axis" -> keepAxis, "keepdims" -> False, "exclude" -> True];

PackageScope["SowFlatMax"]
(* maximums away everything but the 0th axis, assumed to be batchwise *)
SowFlatMax[node_, keepAxis_ : 0] := SowNode["max", node, "axis" -> keepAxis, "keepdims" -> False, "exclude" -> True];

PackageScope["SowTimewiseMean"]
(* sums away the 0th axis, assumed to be timewise *)
SowTimewiseMean[in_, lnode_MXNode] := SowDivide[SowSumAxis[SowSeqMask[in, lnode], 0], lnode];

PackageScope["SowTimewiseSum"]
(* sums away the 0th axis, assumed to be timewise *)
SowTimewiseSum[in_, lnode_MXNode] := SowSumAxis[SowSeqMask[in, lnode], 0];

PackageScope["SowTimewiseProduct"]
(* multiplies away the 0th axis, assumed to be timewise *)
SowTimewiseProduct[in_, lnode_MXNode] := SowProdAxis[SowSeqMask[in, lnode, 1], 0];

PackageScope["SowTimewiseMax"]
(* maximums away the 0th axis, assumed to be timewise *)
SowTimewiseMax[in_, lnode_MXNode] := SowMaxAxis[SowSeqMask[in, lnode], 0];

PackageScope["SowFlatVariance"]

(* TODO: take rank *)
SowFlatVariance[node_, dims_] := Scope[
	count = Times @@ dims;
	allDims = Range[Length[dims]];
	summed = SowNode["sum", in, "axis" -> allDims, "keepdims" -> True];
	mean = SowNode["_DivScalar", summed, "scalar" -> count]; (* <- replace with SowMean? *)
	diff = SowNode["broadcast_minus", {in, mean}];
	diff = SowNode["square", diff];
	var = SowNode["sum", diff, "axis" -> count, "keepdims" -> False];
	var = SowNode["_DivScalar", var, "scalar" -> (count - 1)];
	var
];

PackageScope["SowTake"]
SowTake[node_, {a_, b_}, axis_] := SowNode["slice_axis", node, "axis" -> axis, "begin" -> a, "end" -> b];
SowTake[node_, ind_, axis_] := SowNode["take", {node, ind}, "axis" -> axis];

PackageScope["SowSeqMask"]
PackageScope["SowSeqMaskBatchwise"]

(* see the comment LayerTests.m about testing of layers that employ SowSeqMask *)
SowSeqMask[twise_, lnode_, mask_ : "0.", axis_ : 0] := SowNode["SequenceMask", {twise, lnode}, "use_sequence_length" -> "true", "value" -> mask, "axis" -> axis];
SowSeqMaskBatchwise[bwise_, lnode_, mask_ : "0."] := SowSeqMask[bwise, lnode, mask, 1];
(* ^ TODO: rename SeqSeqMask to Timewise, and make SeqSeqMaskBatchwise into SeqSeqMask *)

PackageScope["SowSeqReverse"]
SowSeqReverse[in_, lnode_] := SowNode["SequenceReverse", {in, lnode}, "use_sequence_length" -> "true"];

PackageScope["SowInsertDim"]
PackageScope["SowInsertDimList"]
SowInsertDim[in_, axis_] := SowNode["expand_dims", in, "axis" -> axis];
SowInsertDimList[in_, axes_List] := Fold[SowInsertDim, in, axes];

PackageScope["SowSqueeze"]
SowSqueeze[in_, axis_] := SowNode["squeeze", in, "axis" -> ToList[axis]];

PackageScope["SowUniformRandom"]

SowUniformRandom[shape_List, precision_] :=
	SowSourceFixup[
		SowNode["random_uniform", {}, "low" -> "0", "high" -> "1", "dtype" -> ToMXNetDataTypeName @ ToDataTypeCode[precision]],
		shape
	];

(* Debugging ops *)

PackageScope["KillNode"]
PackageScope["ForceNode"]
KillNode[a_] := SowTimesScalar[a, "0.0"];
ForceNode[r_][a_] := SowPlusScalar[SowTimesScalar[a, "0.0"], r];

PackageScope["SowSwitch"]
SowSwitch[a_, thresh_, b_, c_] := Scope[
	above = SowTimesScalar[SowPlusScalar[SowNode["sign", SowMinusScalar[a, thresh]], 1], 0.5];
	below = SowOneMinus[above];
	SowPlus[SowHad[b, below], SowHad[c, above]]
]

PackageScope["SowJoin"]

SowJoin[args___, n_Integer] := SowNode["concat", {args}, "num_args" -> Length[{args}], "dim" -> n]

PackageScope["NthOutput"]
NthOutput[mx_MXNode, n_] := ReplacePart[mx, 2 -> n];

PackageScope["DropInitialUnitDim"]

DropInitialUnitDim[in_] :=
	SowNode["reshape", in, "shape" -> {-3, -2}];

PackageScope["SowAppend"]
SowAppend[in_, append_] := SowJoin[in, SowInsertDim[append, 1], 1];

PackageScope["SowPrepend"]
SowPrepend[in_, prepend_] := SowJoin[SowInsertDim[prepend, 1], in, 1];

PackageScope["SowTransposedConvolutionOrPooling"]

(* used by Convolution, Pooling, Deconvolution *)

SowTransposedConvolutionOrPooling[1, input_, other___] :=
	SowSwapAxis[SowCurrentNode[{SowSwapAxis[input, 1, 2], other}], 1, 2];

SowTransposedConvolutionOrPooling[2, input_, other___] :=
	SowTranspose[SowCurrentNode[{SowTranspose[input, {0, 3, 1, 2}], other}], {0, 2, 3, 1}];

SowTransposedConvolutionOrPooling[3, input_, other___] :=
	SowTranspose[SowCurrentNode[{SowTranspose[input, {0, 4, 1, 2, 3}], other}], {0, 2, 3, 4, 1}];

PackageScope["SowBlockGrad"]

SowBlockGrad[node_] := SowNode["BlockGrad", node];

PackageScope["SowRNNNode"]

SowRNNNode[type_String, {len_, stateSize_Integer}, timewise_, gluedArray_, initialState_] := Scope[
	res = SowNode[
		"RNN",
		ToList[timewise, gluedArray, initialState /. s_MXNode :> SowInsertDim[s, 0]],
		"state_size" -> stateSize,
		"num_layers" -> 1,
		"mode" -> type,
		"state_outputs" -> ListQ[initialState] (* <- for lstm, emit cellstate *)
	];
	out = ToMetaNode[res, len, True];
	finalState = SowMetaLast[out];
	If[ListQ[initialState],
		(* Here happens https://bugs.wolfram.com/show?number=348910
			We lost the intermediate cell states
		*)
		cellState = NthOutput[res, 2];
		cellState = SowSqueeze[cellState, 0];
		{out, finalState, cellState}
		,
		{out, finalState}
	]
];

PackageScope["SowBroadcastAgainst"]

SetUsage @ "
SowBroadcastAgainst[input$, {d$1,d$2,$$}, ref$] adds extra dimensions to input$ at positions d$i, copying \
them from the corresponding positions in the shape of ref$."

SowBroadcastAgainst[in_, {}, _] := in;

SowBroadcastAgainst[in_, brank_, reference_] :=
	SowNode["broadcast_like", {
		SowInsertDimList[in, CTable[Min @ brank, Length[brank]]],
		reference},
		"lhs_axes" -> brank,
		"rhs_axes" -> brank
	];

PackageScope["SowNodeLength"]

SetUsage @ "
SowNodeLength[node$, precision$] obtains the run-time length of a node as another node of dimensions (1), with a given precision."

SowNodeLength[node_, precision_] := Scope[
	dims = SowNode["shape_array", node];
	len = SowNode["slice", dims, "begin" -> "(1,)", "end" -> "(2,)"];
	(* TODO: we should check that the output type of slice (int32/int64) is not already of the requested precision *)
	SowNode["cast", len, "dtype" -> ToMXNetDataTypeName @ ToDataTypeCode[precision]]
];

PackageScope["SowAnd"]
SowAnd[a_, b_] := SowNode["broadcast_logical_and", {a, b}];

PackageScope["SowOr"]
SowOr[a_, b_] := SowNode["broadcast_logical_or", {a, b}];

PackageScope["SowNot"]
SowNot[a_] := SowNode["logical_not", {a}];

PackageScope["SowL1Norm"]
PackageScope["SowL2Norm"]

SowL1Norm[in_] := SowNode["norm", SowFlatten @ in, "ord" -> 1, "axis" -> 1];
SowL2Norm[in_] := SowNode["norm", SowFlatten @ in, "ord" -> 2, "axis" -> 1];

PackageScope["SowOneHot"]
SowOneHot[in_, size_] := SowNode["one_hot", in, "depth" -> size, "dtype" -> $DTypeMXName];

PackageScope["SowGEMM2"]
SowGEMM2[a_, b_, transA_ : False, transB_ : False, alpha_ : 1] := If[
	$TMode,
	(* We disable linalg_gemm2 for training because it shows weird convergence behavior
		TODO. check this in the future (Test/Training/CopyNet)
		https://bugs.wolfram.com/show?number=369862
	*)
	SowNode["batch_dot", {a, b}, "transpose_a" -> transA, "transpose_b" -> transB],
	SowNode["linalg_gemm2", {a, b}, "transpose_a" -> transA, "transpose_b" -> transB, "alpha" -> alpha]
];
