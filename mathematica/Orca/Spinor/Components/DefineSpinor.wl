(* ::Package:: *)

BeginPackage["Spinor`DefineSpinor`"]
GenerateSpinorPolar::usage="Generate Dirac Spinors"
GenerateASpinorPolar::usage="Generate Adjoint Dirac Spinors"
Adjoint::usage="Generate adjoint spinor from spinor"
MakeSpinor::usage="Constructor of Class Spinor"
MakeASpinor::usage="Constructor of Class ASpinor"
Begin["Private`"]

MakeSpinor[S1_,S2_,S3_,S4_]:=Module[{Spinor},
	Spinor=<| 
		"ClassName"->"Spinor",
		1->S1,
		2->S2,
		3->S3,
		4->S4,
		"Value"->{{S1},{S2},{S3},{S4}}
	|>;
	Spinor
];

MakeASpinor[S1_,S2_,S3_,S4_]:=Module[{ASpinor},
	ASpinor=<| 
		"ClassName"->"ASpinor",
		1->S1,
		2->S2,
		3->S3,
		4->S4,
		"Value"->{{S1,S2,S3,S4}}
	|>;
	ASpinor
];

Adjoint[Spinor_]:=Module[{ASpinor},

	If[Spinor[["ClassName"]]=="Spinor",(
		ASpinorValue=ConjugateTranspose[Spinor[["Value"]]] . Constants`Matrices`MGamma[[4]];
		ASpinor=Spinor`DefineSpinor`MakeASpinor[ASpinorValue[[1,1]],ASpinorValue[[1,2]],ASpinorValue[[1,3]],ASpinorValue[[1,4]]];
	),(
		ASpinorValue=ConjugateTranspose[Spinor[["Value"]] . Constants`Matrices`MGamma[[4]]];
		ASpinor=Spinor`DefineSpinor`MakeSpinor[ASpinorValue[[1,1]],ASpinorValue[[2,1]],ASpinorValue[[3,1]],ASpinorValue[[4,1]]];
	)];
	ASpinor
];

GenerateSpinorPolar[E_,p_,theta_,phi_,ParticleType_,SymmetryOperator_,Eigenvalue_]:=Module[{m,S1,S2,S3,S4,Spinor},
	If [E^2-p^2<0 || E<0, Throw["Invalid 4-momentum"]];
	If [!(0<=theta<=Pi && 0<=phi<=2*Pi), Throw["Invad direction"]];
	If[(ParticleType != "P") && (ParticleType!= "AP"), Throw["Invalid particle typle"]];
	If [SymmetryOperator!="Conjugation" && SymmetryOperator!="Helicity" && SymmetryOperator != "Parity" && SymmetryOperator != "Chiraty", Throw["Invalid symmetry operator"]];
	If[(SymmetryOperator == "Conjugation" && Eigenvalue != "Plus" && Eigenvalue != "Minus") ||
		(SymmetryOperator == "Helicity" && Eigenvalue != "Left" && Eigenvalue != "Right") ||
		(SymmetryOperator == "Parity" && Eigenvalue != "Plus" && Eigenvalue != "Minus") ||
		(SymmetryOperator == "Chiraty" && Eigenvalue != "Left" && Eigenvalue != "Right"),Throw["Invad eigenvalue"]
	];		
	
	m=Sqrt[E^2-p^2];
	S1=0;S2=0;S3=0;S4=0;
	If [ParticleType=="P" && SymmetryOperator == "Helicity" && Eigenvalue == "Left", 
		S1=-Sqrt[E+m]*Sin[theta/2];
		S2=Sqrt[E+m]*Cos[theta/2]*Exp[I*phi];
		S3=Sqrt[E+m]*p/(E+m)*Sin[theta/2];
		S4=-Sqrt[E+m]*p/(E+m)*Cos[theta/2]*Exp[I*phi]
	];
	If [ParticleType=="P" && SymmetryOperator == "Helicity" && Eigenvalue == "Right", 
		S1=Sqrt[E+m]*Cos[theta/2];
		S2=Sqrt[E+m]*Sin[theta/2]*Exp[I*phi];
		S3=Sqrt[E+m]*p/(E+m)*Cos[theta/2];
		S4=Sqrt[E+m]*p/(E+m)*Sin[theta/2]*Exp[I*phi]
	];
	If [ParticleType=="AP" && SymmetryOperator == "Helicity" && Eigenvalue == "Left", 
		S1=Sqrt[E+m]*p/(E+m)*Cos[theta/2];
		S2=Sqrt[E+m]*p/(E+m)*Sin[theta/2]*Exp[I*phi];
		S3=Sqrt[E+m]*Cos[theta/2];
		S4=Sqrt[E+m]*Sin[theta/2]*Exp[I*phi]
	];
	If [ParticleType=="AP" && SymmetryOperator == "Helicity" && Eigenvalue == "Right", 
		S1=Sqrt[E+m]*p/(E+m)*Sin[theta/2];
		S2=-Sqrt[E+m]*p/(E+m)*Cos[theta/2]*Exp[I*phi];
		S3=-Sqrt[E+m]*Sin[theta/2];
		S4=Sqrt[E+m]*Cos[theta/2]*Exp[I*phi]
	];
	Spinor=MakeSpinor[S1,S2,S3,S4];
	Spinor
];

GenerateASpinorPolar[E_,p_,theta_,phi_,ParticleType_,SymmetryOperator_,Eigenvalue_]:=Module[{Spinor, ASpinor},
	Spinor=GenerateSpinorPolar[E,p,theta,phi,ParticleType,SymmetryOperator,Eigenvalue];
	ASpinor=Adjoint[Spinor];
	ASpinor
];
End[]
EndPackage[]

(*
Print[GenerateSpinor[E,P,theta,phi,"AP","Helicity","Right"]]
*)

