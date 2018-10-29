(* ::Package:: *)

Paclet[
  Name -> "DeepMath",
  Version -> "1.0.0",
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
