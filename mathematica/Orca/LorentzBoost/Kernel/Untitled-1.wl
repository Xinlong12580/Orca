(* ::Package:: *)

BeginPackage["LorentzBoost`DefineVector`"]
MakeVector::usage="Constructor of the Class Vector"

Begin["`Private`"]
MakeVector[T_,X_,Y_,Z_]:=Module[{Vector},
	Vector=<|
		"ClassName"->"Vector",
		1->X,
		2->Y,
		3->Z,
		4->T,
		"Value"={{X},{Y},{Z},{T}}
	|>;
}
End[]
EndPackage[]
