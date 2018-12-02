DeepMath::usage = "";
$Root::usage = "";
$Data::usage = "";
$RootNN::usage = "";
Begin["`Private`"];
$Root = File@DirectoryName[FindFile["DeepMath`"], 2];
$Data = File@If[
	MissingQ@PersistentValue["DeepMath", "Local"],
	(*PersistentValue["DeepMath", "Local"] ="D:\\NeuralNetworks"*)
	PersistentValue["DeepMath", "Local"] = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "NeuralNetworks"}];
	Return@PersistentValue["DeepMath", "Local"],
	Return@PersistentValue["DeepMath", "Local"]
];
$RootNN = File@DirectoryName[FindFile["NeuralNetworks`"], 2];

(*Extension*)
PackageExtendContextPath[{
	"GeneralUtilities`"
}];

$DeepMathInitialize = <|
	"Helper" -> TrueQ@DeepMath`helper
	(*"Layers"->TrueQ@$LoadingLayers*)
|>;
End[]