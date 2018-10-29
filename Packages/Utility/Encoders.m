(* ::Package:: *)
(* ::Subsection::Closed:: *)
(*Defines*)
$DatasetShift::usage = "";
(* ::Subsection::Closed:: *)
(*Main*)
Begin["`Encoder`"];
(* ::Subsubsection:: *)
(*resnet*)

(*Root Mean Square*)
$DatasetShift = <|
	"Normal" -> <|
		"Size" -> Automatic,
		"Mean" -> {0.5, 0.5, 0.5},
		"RMS" -> {0, 0, 0}
	|>,
	"MNIST" -> <|
		"Size" -> {1, 28, 28},
		"Mean" -> 0.869,
		"RMS" -> 0.305
	|>,
	"Fashion MNIST" -> <|
		"Size" -> {1, 28, 28},
		"Mean" -> 0.714,
		"RMS" -> 0.330
	|>,
	"CIFAR10" -> <|
		"Size" -> {3, 32, 32},
		"Mean" -> {0.491, 0.482, 0.447},
		"RMS" -> {0.211, 0.209, 0.212}
	|>,
	"CIFAR100" -> <|
		"Size" -> {3, 32, 32},
		"Mean" -> {0.507, 0.487, 0.441},
		"RMS" -> {0.211, 0.208, 0.216}
	|>,
	"ImageNet" -> <|
		"Size" -> {3, 224, 224},
		"Mean" -> {0.485, 0.456, 0.406},
		"RMS" -> {0.229, 0.224, 0.225}
	|>,
	"Urban100" -> <|
		"Size" -> {3, Automatic, Automatic},
		"PixelCount" -> 774313.6`,
		"AspectRatio" -> 1.2886904158701082`,
		"Mean" -> {0.4581507484907432`, 0.44752828827793933`, 0.4412621560325149`},
		"STD" -> {0.2603759362744304`, 0.2528318425579182`, 0.26072206551107546`},
		"RMS" -> {0.2657178044188541`, 0.25769640703204005`, 0.266250802944032`}
	|>,
	"Manga109" -> <|
		"Size" -> {3, Automatic, Automatic},
		"PixelCount" -> 966010.495412844`,
		"AspectRatio" -> 0.706702988234552`,
		"Mean" -> {0.6711883699049864`, 0.5944666208402735`, 0.5576886247603288`},
		"STD" -> {0.30059826153919184`, 0.289881773898409`, 0.28189032007849696`},
		"RMS" -> {0.30674296252548366`, 0.29441755256317403`, 0.2870821411941492`}
	|>
|>



scan[path_] := Block[
	{img},
	img = First[Image`ImportExportDump`ImageReadPNG[path]];
	{Mean@img, Variance@img}
];
ScanImageDataset[path_] := Mean[ParallelMap[scan, FileNames["*", path]]];

(*

DIV2K:
Mean {0.4488, 0.4371, 0.4040}


*)

(* ::Subsection::Closed:: *)
(*附加设置*)
SetAttributes[
	{ },
	{Protected, ReadProtected}
]
End[]