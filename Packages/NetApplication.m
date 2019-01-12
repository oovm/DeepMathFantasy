(* ::Package:: *)

(* ::Subsection:: *)
(*Inherit*)


NetApplication::usage = "2";
MakeNetApplication::usage = "Makes an object";
PackNetApplication::usage = "Makes an object";



(* ::Subsection:: *)
(*Main*)


Begin["`Tools`NetApplication`"];


(* ::Subsubsection::Closed:: *)
(*Constructor*)
alias[old_, new_] := Set[
	Language`ExtendedDefinition[new],
	Language`ExtendedDefinition[old] /. HoldPattern[old] :> new
];

(*Regarded  as Symbol*)
(*
NetApplication ~ SetAttributes ~ HoldFirst;
SetNetApplication[data_] := With[
	{u = Unique["FunctionRepository`$" <> StringJoin@StringSplit[data["UUID"], "-"] <> "`Object"]},
	u = data;
	SetAttributes[u, Temporary];
	NetApplication[u]
];
*)
(*Regarded as Association*)
SetNetApplication[data_] := NetApplication[data];
MakeNetApplication[e_?AssociationQ] := Block[
	{uuid, ifMissing, wrapper, data},
	uuid = If[MissingQ[e@"UUID"], CreateUUID[], e@"UUID"];
	wrapper = Activate@If[
		MissingQ@e[Method],
		Return[$Failed],
		alias[e[Method], Symbol["FunctionRepository`$" <> StringJoin@StringSplit[uuid, "-"] <> "`" <> SymbolName@e[Method]]]
	];
	data = <|
		"Name" -> If[MissingQ[e@"Name"], Return[$Failed], e@"Name"],
		"UUID" -> uuid,
		"Date" -> If[MissingQ[e@"Date"], Now, e@"Date"],
		"Update" -> Now,
		"Input" -> If[MissingQ[e@"Input"], Return[$Failed], e@"Input"],
		"Example" -> e["Example"],
		"Version" -> "NetApplication v1.4.0",
		"Model" -> e["Model"],
		Method -> Symbol["FunctionRepository`$" <> StringJoin@StringSplit[uuid, "-"] <> "`" <> SymbolName@e[Method]],
		MetaInformation -> If[MissingQ[e@MetaInformation], <||>, e@MetaInformation]
	|>;
	SetAttributes[data, Temporary];
	SetNetApplication[data]
];


(* ::Subsubsection:: *)
(*Saver*)


Serialize := Serialize = ResourceFunction["BinarySerializeWithDefinitions"];
Options[PackNetApplication] = {"Name" -> Automatic};
PackNetApplication[app_, OptionsPattern] := Block[
	{name, bin},
	name = If[
		OptionValue["Name"] =!= Automatic,
		OptionValue["Name"],
		Normal[app]["Name"] <> ".app";
	];
	If[FileExistsQ@name, DeleteFile@name];
	bin = CreateFile[name];
	BinaryWrite[bin, Serialize@app];
	Close[bin]
];


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


(* ::Subsubsection::Closed:: *)
(*Interface*)


(*define application functions*)
(*
SetAttributes[apply, HoldAllComplete];
$basicInterfaceFunctions = {
	Part, Extract, Take, Drop, First, Last, Most, Rest, Length,
	Lookup, KeyTake, KeyDrop, MemberQ, KeyMemberQ, KeyExistsQ
};
apply[NetApplication[s_], f_, args___] := f[s, args];
apply[NetApplication[s_][a___], f_, args___] := f[s[a], args];
apply[NetApplication[s_][[a___]], f_, args___] := f[s[[a]], args];
apply[e_, r__] := With[
	{o = Block[
		{NetApplication, Part},
		SetAttributes[NetApplication, HoldAllComplete];
		Hold[Evaluate@e]
	]},
	Replace[o, Hold[a_] :> apply[a, r]] /; MatchQ[o,
		Hold[NetApplication[_Association]]
			| Hold[NetApplication[_Association][___]]
			| Hold[NetApplication[_Association][[___]]]
	]
];
Map[(NetApplication /: #[o_NetApplication, a___] := apply[o, #, a]) &, $basicInterfaceFunctions];
*)

(*define mutation interface via apply function*)
NetApplicationMutationHandler ~ SetAttributes ~ HoldAllComplete;
NetApplicationMutationHandler[Set[object_[key_], value_]] := Block[
	{new = Join[object[MetaInformation], <|key -> value|>]},
	Set[object, NetApplication[Join[Normal@object, MetaInformation -> <|new|>]]];
	Return[Null]
];
Language`SetMutationHandler[NetApplication, NetApplicationMutationHandler];
(*
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
*)

(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*Methods*)


NetApplication /: Print[NetApplication[s_]] := Information[Evaluate@s[Method], LongForm -> False];
NetApplication /: Normal[NetApplication[s_]] := s;
(*NetApplication[asc_?AssociationQ][New]:=makeNew;*)
(*NetApplication[asc_?AssociationQ][Save]:=doSave;*)
NetApplication[asc_?AssociationQ][Input] := Activate@Lookup[asc, "Example"];
NetApplication[asc_?AssociationQ][Function] := GeneralUtilities`PrintDefinitionsLocal@Lookup[asc, Method];
NetApplication[asc_?AssociationQ][NetModel] := Lookup[asc, "Models"];
NetApplication[asc_?AssociationQ][MetaInformation] := Lookup[asc, MetaInformation];
NetApplication[this_][args___] := this[Method][this, args];


(* ::Subsection:: *)
(*Additional*)


SetAttributes[
	{NetApplication},
	{ReadProtected}
];
End[]
