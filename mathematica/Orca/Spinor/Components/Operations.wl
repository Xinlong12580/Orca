(* ::Package:: *)

(*AppendTo[$Path, "D:\\work_directory\\Programs\\Orca\\mathematica\\Orca"];
*)


Get["Constants`Components`Matrices`"]
Get["Spinor`Components`DefineSpinor`"]
MGamma[[4]]
BeginPackage["Spinor`Transform`"]
Adjoint::usage="Generate adjoint spinor from spinor"
ChargeConjugation::usage="ChargeConjugation Operator"
Begin["`private`"]

Adjoint[Spinor_]:=Module[{ASpinor},
	If[Spinor[["ClassName"]]=="Spinor",(
		ASpinorValue=ConjugateTranspose[Spinor[["Value"]]] . Constants`Matrices`MGamma[[4]];
		ASpinor=Spinor`DefineSpinor`MakeASpinor[ASpinorValue[[1,1]],ASpinorValue[[1,2]],ASpinorValue[[1,3]],ASpinorValue[[1,4]]];
	),(
		ASpinorValue=ConjugateTranspose[Spinor[["Value"]] . Constants`Matrices`MGamma[[4]]];
		ASpinor=Spinor`DefineSpinor`MakeSpinor[ASpinorValue[[1,1]],ASpinorValue[[2,1]],ASpinorValue[[3,1]],ASpinorValue[[4,1]]];
	)]
	ASpinor
];

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







