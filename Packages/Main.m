DeepMathFantasy::usage = "";
$pyPATH::usage = "";
$pySession::usage = "";
Begin["`Private`"];
$pyPATH = FileNameJoin[{$HomeDirectory, "Anaconda3", "python.exe"}];
$pySession := $pySession = StartExternalSession[<|
	"System" -> "Python",
	"Executable" -> $pyPATH,
	"ReturnType" -> "Expression"
|>];
(*PackageLoadPacletDependency["NeuralNetworks`"];*)
(*PackageLoadPacletDependency["MXNetLink`"];*)
(*PackageExtendContextPath[*)
(*{*)
(*"Developer`",*)
(*"MXNetLink`",*)
(*"NeuralNetworks`",*)
(*"GeneralUtilities`"*)
(*}*)
(*];*)
(*SetAttributes[*)
(*{ },*)
(*{Protected, ReadProtected}*)
(*];*)
End[]
