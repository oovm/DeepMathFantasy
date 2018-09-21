(* ::Package:: *)

SetDirectory@NotebookDirectory[];
$GluonCVRemote = "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/gluon/models/";
$GluonCVModels = "https://github.com/dmlc/gluon-cv/raw/master/gluoncv/model_zoo/model_store.py";
GluonCVFormat[name_, hash_] := <|
	"Name" -> name,
	"SHA1" -> hash,
	"Remote" -> StringJoin[$GluonCVRemote, name, "-", StringTake[hash, 8], ".zip"]
|>;
GluonCVFilter = StringCases[#, "('" ~~ a__ ~~ "', '" ~~ b__ ~~ "')" :> GluonCVFormat[b, a]]&;
Hyperlink["GluonCVModels", $GluonCVModels]
data = Flatten[GluonCVFilter /@ Import[$GluonCVModels, {"Text", "Lines"}][[11 ;; 89]]];
Dataset[data]
Export["GluonCV.dark", data, "WXF", PerformanceGoal -> "Size"]


(* ::Text:: *)
(*"2018-09-21T17:18:47"*)


DateString["ISODateTime"]
