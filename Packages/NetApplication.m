(* ::Package:: *)

(* ::Subsection:: *)
(*Inherit*)


NetApplication::usage = "2";
MakeNetApplication::usage = "Makes an object";
PackNetApplication::usage = "Makes an object";

(* ::Subsection:: *)
(*Main*)


Begin["`Tools`NetApplication`"];


(* ::Subsubsection:: *)
(*Constructor*)


(*Seem as Symbol*)
(*
NetApplication ~ SetAttributes ~ HoldFirst;
SetNetApplication[data_] := With[
	{u = Unique["FunctionRepository`$" <> StringJoin@StringSplit[data["UUID"], "-"] <> "`Object"]},
	u = data;
	SetAttributes[u, Temporary];
	NetApplication[u]
];
*)
(*Seem as Association*)
SetNetApplication[data_] := NetApplication[data];
MakeNetApplication[e_?AssociationQ] := Block[
	{uuid, ifMissing, wrapper, data},
	uuid = CreateUUID[];
	ifMissing[p_, default_] := If[MissingQ[e@p], default, e@p];
	wrapper = Activate@If[
		MissingQ@e[Method],
		Return[$Failed],
		Inactive[Set][
			Inactive[DownValues][Symbol["FunctionRepository`$" <> StringJoin@StringSplit[uuid, "-"] <> "`" <> ToString@e[Method]]],
			Inactive[ReplaceAll][
				Inactive[DownValues][e[Method]],
				{e[Method] -> Symbol["FunctionRepository`$" <> StringJoin@StringSplit[uuid, "-"] <> "`" <> ToString@e[Method]]}
			]
		]
	];
	data = <|
		"Name" -> e["Name"],
		"UUID" -> uuid,
		"Date" -> ifMissing["Date", Now],
		"Input" -> e["Input"],
		"Example" -> e["Example"],
		"Version" -> "NetApplication v1.3",
		"Model" -> e["Model"],
		Method -> Symbol["FunctionRepository`$" <> StringJoin@StringSplit[uuid, "-"] <> "`" <> ToString@e[Method]],
		MetaInformation -> ifMissing[MetaInformation, <||>]
	|>;
	SetAttributes[data, Temporary];
	SetNetApplication[data]
];



(* ::Subsubsection:: *)
(*Saver*)
Serialize := Serialize = ResourceFunction["BinarySerializeWithDefinitions"];
PackNetApplication[app_] := Block[
	{name, bin},
	name = Normal[app]["Name"] <> ".app";
	If[FileExistsQ@name, DeleteFile@name];
	bin = CreateFile[name];
	BinaryWrite[bin, Serialize@app];
	Close[bin]
]

(* ::Subsubsection::Closed:: *)
(*Icon*)


$icon = Block[
	{
		layerCounts = {3, 2, 5, 1},
		graph, vStyle
	},
	graph = GraphUnion @@ MapThread[
		IndexGraph, {
			CompleteGraph /@ Partition[layerCounts, 2, 1],
			FoldList[Plus, 0, layerCounts[[;; -3]]]
		}];
	vStyle = Catenate[Thread /@ Thread[
		TakeList[VertexList[graph], layerCounts] -> ColorData[97] /@ Range@Length[layerCounts]
	]];
	Graph[
		graph,
		GraphLayout -> {"MultipartiteEmbedding", "VertexPartition" -> layerCounts},
		GraphStyle -> "BasicBlack",
		VertexSize -> 0.65,
		VertexStyle -> vStyle,
		ImageSize -> 60
	]
];


(* ::Subsubsection:: *)
(*Interface*)


(*define application functions*)

SetAttributes[apply, HoldAllComplete];
$basicInterfaceFunctions = {
	Part, Extract, Take, Drop, First, Last, Most, Rest, Length,
	Lookup, KeyTake, KeyDrop, MemberQ, KeyMemberQ, KeyExistsQ
};
apply[NetApplication[s_], f_, args___] := f[s, args];
apply[NetApplication[s_][a___], f_, args___] := f[s[a], args];
apply[NetApplication[s_][[a___]], f_, args___] := f[s[[a]], args];
apply[e_, r__] := apply[Evaluate@e, r] /; MatchQ[e, NetApplication[_Symbol]];
Map[(NetApplication /: #[o_NetApplication, a___] := apply[o, #, a]) &, $basicInterfaceFunctions];
(*define mutation interface via apply function*)
SetAttributes[mutate, HoldAllComplete];
$basicMutationFunctions = {
	SetDelayed, Unset, KeyDropFrom,
	PrependTo, AppendTo, AssociateTo, AddTo, SubtractFrom, TimesBy, DivideBy
};
Map[(
	mutate[#[o : (_Association?(MatchQ[#, NetApplication[_Association]] &) | NetApplication[_Association]), a___]] := apply[o, #, a];
	mutate[#[o : (_Association?(MatchQ[#, NetApplication[_Association]] &) | NetApplication[_Association])[k__], a___]] := apply[o, #, a];
	mutate[#[o : (_Association?(MatchQ[#, NetApplication[_Association]] &) | NetApplication[_Association])[[k___]], a___]] := apply[o, #, a];
) &, $basicMutationFunctions];
Language`SetMutationHandler[NetApplication, mutate];


(* ::Subsubsection:: *)
(*Object*)

showByte = UnitConvert[Quantity[N@ByteCount@#, "Bytes"], "Megabytes"]&;
NetApplicationQ[asc_?AssociationQ] := AllTrue[{"Name", "Input", "Model"}, KeyExistsQ[asc, #]&];
NetApplicationQ[___] = False;
NetApplication::illInput = "Illegal parameters, please read the instructions again.";
NetApplication /: MakeBoxes[
	obj : NetApplication[asc_? NetApplicationQ],
	form : (StandardForm | TraditionalForm)
] := Module[
	{above, below},
	above = {
		{BoxForm`SummaryItem[{"Name: ", asc["Name"]}]},
		{BoxForm`SummaryItem[{"Hash: ", Hash[asc, "Expression", "HexString"]}]},
		{BoxForm`SummaryItem[{"Input: ", asc["Input"]}]}
	};
	below = {
		{BoxForm`SummaryItem[{"Byte: ", showByte@asc["Model"]}]},
		{BoxForm`SummaryItem[{"Date: ", DateString@asc["Date"]}]},
		{BoxForm`SummaryItem[{"UUID: ", asc["UUID"]}]}
	};
	BoxForm`ArrangeSummaryBox[
		"NetApplication",
		obj, $icon, above, below, form,
		"Interpretable" -> Automatic
	]
];


(* ::Subsubsection:: *)
(*Methods*)


NetApplication /: Print[NetApplication[s_]] := Information[Evaluate@s[Method], LongForm -> False];
NetApplication /: Normal[NetApplication[s_]] := s;
(*NetApplication[asc_?AssociationQ][Save]:=doSave;*)
NetApplication[asc_?AssociationQ][Input] := Activate@Lookup[asc, "Example"];
NetApplication[asc_?AssociationQ][Function] := GeneralUtilities`PrintDefinitionsLocal@Lookup[asc, Method];
NetApplication[asc_?AssociationQ][NetModel] := Lookup[asc, "Models"];
NetApplication[asc_?AssociationQ][MetaInformation] := asc;
NetApplication[s_Symbol][args___] := s[Method][o, Function[Null, #[args], HoldFirst]];


(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{ },
	{Protected, ReadProtected}
];
End[]
