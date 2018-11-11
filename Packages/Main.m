DeepMath::usage = "";
$DeepMathDirectory::usage = "";
$DeepMathData::usage = "";
Begin["`Private`"];
$DMRoot = DirectoryName[FindFile["DeepMath`"], 2];
$NNRoot = DirectoryName[FindFile["NeuralNetworks`"], 2];
$DMData = If[
	MissingQ@PersistentValue["DeepMath", "Local"],
(*PersistentValue["DeepMath", "Local"] ="D:\\NeuralNetworks"*)
	PersistentValue["DeepMath", "Local"] = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "NeuralNetworks"}];
	Return@PersistentValue["DeepMath", "Local"],
	Return@PersistentValue["DeepMath", "Local"]
];

$DeepMathInitialize = <|
	"Helper" -> TrueQ@DeepMath`helper
(*"Layers"->TrueQ@$LoadingLayers*)
|>;
End[]