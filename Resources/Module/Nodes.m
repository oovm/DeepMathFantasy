Package["NeuralNetworks`"]

PackageScope["SowIdentity"]
SowIdentity[a_] := SowNode["identity", a];

PackageScope["SowMaxEps"]
SowMaxEps[a_] := SowNode["_maximum_scalar", a, "scalar" -> "0.2220446049250313e-15"];

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

PackageScope["SowDot"]
SowDot[x_, w_] := SowNode["FullyConnected", {x, w}, "num_hidden" -> getLen[w], "no_bias" -> True];

PackageScope["SowFC"]
SowFC[x_, w_, b_] := SowNode["FullyConnected", {x, w, b}, "num_hidden" -> getLen[w]];

getLen[id_] := First @ NodeDimensions[id];

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
SowReshape[in_, dims___] := SowNode["Reshape", in, "shape" -> Flatten[{-1, dims}]];

PackageScope["SowUReshape"]
SowUReshape[in_, dims___] := SowNode["Reshape", in, "shape" -> Flatten[{dims}]];

PackageScope["SowTranspose"]
SowTranspose[in_, axes_ : {0, 2, 1}] := SowNode["transpose", in, "axes" -> axes];

PackageScope["SowTransposeLast"]
SowTransposeLast[in_, 1] := in;
SowTransposeLast[in_, rank_] := SowTranspose[in, Join[{0, rank}, Range[rank - 1]]];

PackageScope["SowTranspose01"]
SowTranspose01[in_] := SowNode["SwapAxis", in, "dim1" -> "0", "dim2" -> "1"];

PackageScope["SowSwapAxis"]
SowSwapAxis[in_, d1_, d2_] := If[d1 === d2, in, SowNode["SwapAxis", in, "dim1" -> d1, "dim2" -> d2]];

PackageScope["SowSumAxis"]
SowSumAxis[a_, axes_] := SowNode["sum", a, "axis" -> axes, "keepdims" -> "false"];

PackageScope["SowFlatten1"]
SowFlatten1[in_] := SowNode["Reshape", in, "shape" -> {-3, -2}];

PackageScope["SowUnflatten1"]
SowUnflatten1[in_, n_Integer] :=
	SowNode["Reshape", in, "shape" -> {-4, n, -1, -2}];

SowUnflatten1[in_, orig_MXNode] :=
	SowNode["split_like", {in, orig}];

PackageScope["SowPack"]

SowPack[elems_List, timewise_, dims_] := Scope[
	len = Length[elems];
	(* %MX stupidity *)
	If[dims === {}, elems = Map[SowInsertDim[#, 1]&, elems]];
	shape = Join[If[timewise, {len, -1}, {-1, len}], dims];
	cat = SowNode["Concat", elems, "num_args" -> len, "dim" -> If[timewise, 0, 1]];
	SowNode["Reshape", cat, "shape" -> writeIntList[shape]]
];

SowPack[__] := $Unreachable;

PackageScope["SowUnpack"]
SowUnpack[node_, numOut_, axis_] := Scope[
	out = SowNode["SliceChannel", node, "axis" -> axis, "num_outputs" -> numOut, "squeeze_axis" -> "true"];
	out[[2]] = Range[numOut] - 1;
	Thread[out]
]

PackageScope["SowFlatMean"]
(* sums away everything but the 0th axis, assumed to be batchwise *)
SowFlatMean[node_, keepAxis_ : 0] := SowNode["mean", node, "axis" -> keepAxis, "keepdims" -> False, "exclude" -> True];


PackageScope["SowTimewiseMean"]
(* sums away the 0th axis, assumed to be timewise *)
SowTimewiseMean[in_, lnode_MXNode, _] := SowDivide[SowSumAxis[SowSeqMask[in, lnode], 0], lnode];
SowTimewiseMean[in_, None, maxlen_] := SowDivideScalar[SowSumAxis[in, 0], maxlen];


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

PackageScope["SowSeqMask"]
PackageScope["SowSeqMaskBatchwise"]

(* see the comment LayerTests.m about testing of layers that employ SowSeqMask *)
SowSeqMask[twise_, lnode_, mask_ : "0."] := SowNode["SequenceMask", {twise, lnode}, "use_sequence_length" -> "true", "value" -> mask];
SowSeqMaskBatchwise[bwise_, lnode_, mask_ : "0."] := SowNode["SequenceMask", {bwise, lnode}, "use_sequence_length" -> "true", "value" -> mask, "axis" -> 1];


PackageScope["SowSeqReverse"]
SowSeqReverse[in_, lnode_] := SowNode["SequenceReverse", {in, lnode}, "use_sequence_length" -> "true"];


PackageScope["SowInsertDim"]
SowInsertDim[in_, axis_] := SowNode["expand_dims", in, "axis" -> axis];

PackageScope["FCGate"]
FCGate[dropfn_, weight_, bias_] := Function[fcin, SowFC[dropfn[fcin], weight, bias]];

PackageScope["SowBroadcastAt"]
SowBroadcastAt[in_, axis_, len_] :=
	SowNode[
		"broadcast_axis",
		SowNode["expand_dims", in, "axis" -> axis],
		"axis" -> axis, "size" -> len
	];

PackageScope["SowBroadcastUp"]
SowBroadcastUp[in_, odim_, idim_] := Scope[
	newrank = Length[odim] - Length[idim];
	If[newrank === 0, Return[in]];
	newdims = Take[odim, newrank];
	expanded = SowNode["Reshape", in, "shape" -> Join[{0}, 1 + newdims * 0, idim]];
	SowNode["broadcast_axis", expanded, "size" -> newdims, "axis" -> Range[newrank]]
];

(*
(* this is how this should be implemented, unfortunately this fails inference
because who-knows *)

SowBroadcastUp[in_, 0] := in;
SowBroadcastUp[in_, newrank_] := Scope[
	Do[in = SowNode["expand_dims", in, "axis" -> "1"], newrank];
	SowNode["broadcast_axis", in]
];
*)

PackageScope["SowUniformRandom"]

SowUniformRandom[shape_List] :=
	SowSourceFixup[SowNode["uniform", {}, "low" -> "0", "high" -> "1"], shape];

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
	SowNode["Reshape", in, "shape" -> {-3, -2}];


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


PackageScope["SowPad1"]

(* pads on the first dim, working around crappy limitation of pad layer in 10001 *)
SowPad1[input_, dims_, l_, r_] := Scope[
	raised = SowInsertDim[input, 1];
	padspec = Join[{0, 0, 0, 0, l, r}, Table[0, (dims - 1) * 2]];
	padded = SowNode["pad", raised, "pad_width" -> padspec, "mode" -> "constant", "constant_value" -> "0"];
	SowNode["Reshape", padded, "shape" -> {0, -3, -2}]
];


PackageScope["SowRNNNode"]

SowRNNNode[type_String, {len_, stateSize_Integer}, timewise_, gluedArray_, initialState_] := Scope[
	If[$GPUMode,
		res = SowNode[
			"RNN",
			ToList[timewise, gluedArray, initialState /. s_MXNode :> SowInsertDim[s, 0]],
			"state_size" -> stateSize,
			"num_layers" -> 1,
			"mode" -> type,
			"state_outputs" -> ListQ[initialState] (* <- for lstm, emit cellstate *)
		];
		out = ToMetaNode[res, {len, stateSize}, True];
		finalState = SowMetaLast[out];
		If[ListQ[initialState], finalState = {finalState, NthOutput[res, 2]}];
		{out, finalState}
		,
	(* the code below is nonsense, but allows us to have a working plan on CPU via the
	$ForceDummyRNNNode flag. This is for debugging things other than outputs, e.g. checking
	that array gluing is working well with other features *)
		summedArrays = Apply[SowPlus, SowNode["mean", #]& /@ ToList[gluedArray, initialState]];
		(* ^ we need the output to depend somehow on the initial states and the glued arrays*)
		res = SowNode["mean", timewise, "axis" -> {2}, "keepdims" -> True];
		(* ^ sum away the input width *)
		res = SowNode["broadcast_mul", {res, summedArrays}];
		(* ^ mix in the scalar derived from initial states and glued array *)
		res = SowNode["broadcast_axis", SowTanh[res], "axis" -> {2}, "size" -> {stateSize}];
		(* ^ make the output the right shape *)
		out = ToMetaNode[res, {len, stateSize}, True];
		finalState = SowMetaLast[out];
		If[ListQ[initialState], finalState = {finalState, finalState}];
		{out, finalState}
	]
];
