(* ::Package:: *)

(*AppendTo[$Path, "D:\\work_directory\\Programs\\Orca\\mathematica\\Orca"];
*)


Get["Constants`Components`Matrices`"]
Get["Spinor`Components`DefineSpinor`"]
MGamma[[4]]
BeginPackage["Spinor`Transform`"]
OChargeConjugation::usage="ChargeConjugation Operator"
OParity::usage="Parity Operator"
OChirality::usage="Chirality Operator"
Begin["`private`"]

OChargeConjugation[Spinor_]:=Module[{ReturnSpinor},
	ReternSpinorValue=I*Constants`Matrices`MGamma[[2]] . Conjugate[Spinor[["Value"]]];
	ReturnSpinor=Spinor`DefineSpinor`MakeSpinor[ReternSpinorValue[[1,1]],ReternSpinorValue[[1,2]],ReternSpinorValue[[1,3]],ReternSpinorValue[[1,4]]];
	ReturnSpinor
];
OParity[Spinor_]:=Module[{ReturnSpinor},
	ReternSpinorValue=Constants`Matrices`MGamma[[4]] . Conjugate[Spinor[["Value"]]];
	ReturnSpinor=Spinor`DefineSpinor`MakeSpinor[ReternSpinorValue[[1,1]],ReternSpinorValue[[1,2]],ReternSpinorValue[[1,3]],ReternSpinorValue[[1,4]]];
	ReturnSpinor
];
OChirality[Spinor_]:=Module[{ReturnSpinor},
	ReternSpinorValue=Constants`Matrices`MGamma[[5]] . Conjugate[Spinor[["Value"]]];
	ReturnSpinor=Spinor`DefineSpinor`MakeSpinor[ReternSpinorValue[[1,1]],ReternSpinorValue[[1,2]],ReternSpinorValue[[1,3]],ReternSpinorValue[[1,4]]];
	ReturnSpinor
];

End[]
EndPackage[]
Adjoint[GenerateSpinorPolar[1,0.5,Pi,0,"P","Helicity","Right"]]

MakeASpinor[1,0.5,0,0]

