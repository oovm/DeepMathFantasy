



# Resnet trained on ImageNet

 3\*224\*224 Image

```Mathematica
NetChain[{
	BatchNormalizationLayer["Epsilon" -> 1*^-5], 
	ConvolutionLayer[64, {7, 7}, "PaddingSize" -> 3, "Stride" -> 2],
    ElementwiseLayer["ReLU"], 
    PoolingLayer[{3, 3}, "Function" -> Mean, "Stride" -> 2],
	ResBlockV2[64, 2, False],
	ResBlockV2[128, 4, True],
	ResBlockV2[256, 8, True],
	BatchNormalizationLayer["Epsilon" -> 1*^-5], 
	ElementwiseLayer["ReLU"],
    PoolingLayer[{7, 7}, "PaddingSize" -> 0, "Stride" -> 7],
    FlattenLayer[], 
	1000, 
	SoftmaxLayer[]
}]
```