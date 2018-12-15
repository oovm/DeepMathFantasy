(* ::Package:: *)

(* ::Subsection:: *)
(*Defines*)


LayersRegister::usage = "Add new custom layers";


(* ::Subsection:: *)
(*Main*)


Begin["`LayersRegister`"];


(* ::Subsubsection:: *)

LayersRegister[] := Scope[
	PacletUpdate["NeuralNetworks"];
	local = FileNameJoin[{DirectoryName[FindFile["NeuralNetworks`"], 2], "Layers", "DeepMath"}];
	If[FileExistsQ@local, DeleteDirectory[local, DeleteContents -> True]];
	layers = URLExecute["https://api.github.com/repos/Moe-Net/DeepMathFantasy/contents/Resources/Layers?ref=master", "RawJSON"];
	URLDownloadSubmit[#["download_url"], CreateFile@FileNameJoin[{local, #["name"]}]]& /@ layers
];



(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{LayersRegister},
	{ReadProtected}
];
End[]
