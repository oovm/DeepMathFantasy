DeepMath::usage = "";
$DeepMathDirectory::usage = "";
$DeepMathData::usage = "";
Begin["`Private`"];
$DeepMathDirectory = DirectoryName[FindFile["DeepMath`Kernel`"], 2];
$DeepMathData = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "DeepMath"}];


DeepMath = <|
	"Helper"->TrueQ@DeepMath`helper
	(*"Layers"->TrueQ@$LoadingLayers*)
|>;
End[]