(* ::Package:: *)

Get["Constants`Components`Tensors`"]
Get["LorentzBoost`Components`DefineVector`"]
BeginPackage["LonrentzBoost`Operations`"]
OVMultiply::usage="Vector Multiplication"
OVSubtract::usage="Vector Subtraction"
OVAdd::usage="Vector Addition"
Begin["`Private`"]
OVMultiply[p1_,p2_]:=Module[{mul},
	mul=Transpose[p1[["Value"]]] . Constants`Tensors`Metg . p2[["Value"]];
	mul=mul[[1,1]];
	mul
];
OVSubtract[p1_,p2_]:=Module[{subvalue,subvector},
	subvalue=p1[["Value"]]-p2[["Value"]];
	If[p1[["ClassName"]]=="Momentum",
		subvector=LorentzBoost`DefineVector`MakeMomentum[subvalue[[4,1]],subvalue[[1,1]],subvalue[[2,1]],subvalue[[3,1]]],
		subvector=LorentzBoost`DefineVector`MakeVector[subvalue[[4,1]],subvalue[[1,1]],subvalue[[2,1]],subvalue[[3,1]]]
	];
	subvector
];
OVAdd[p1_,p2_]:=Module[{subvalue,subvector},
	subvalue=p1[["Value"]]+p2[["Value"]];
	If[p1[["ClassName"]]=="Momentum",
		subvector=LorentzBoost`DefineVector`MakeMomentum[subvalue[[4,1]],subvalue[[1,1]],subvalue[[2,1]],subvalue[[3,1]]],
		subvector=LorentzBoost`DefineVector`MakeVector[subvalue[[4,1]],subvalue[[1,1]],subvalue[[2,1]],subvalue[[3,1]]]
	];
	subvector
];
End[]
EndPackage[]




