(* ::Package:: *)

Paclet[
	Name -> "DeepMath",
	Version -> "0.1.0",
	Extensions -> {
		{
			"Kernel",
			"Root" -> ".",
			"Context" -> {"DeepMath`"}
		},
		{
			"Resource",
			"Root" -> "Resources",
			"Resources" -> {
				{
					"GluonCV",
					"GluonCV.dark"
				}
			}
		}
	}
]
