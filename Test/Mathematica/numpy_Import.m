dataset = Normal@SemanticImport["path.txt"];
data = Flatten@Values@dataset;
shape = StringReplace[Keys@First@dataset, {"#" -> "", "(" -> "{", ")" -> "}"}];
ReshapeLayer[ToExpression@First@shape][data]
