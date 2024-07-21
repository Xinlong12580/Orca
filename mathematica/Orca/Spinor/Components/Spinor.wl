(* ::Package:: *)

BeginPackage["Spinor`GenerateSpinor`"]
GenerateSpinor::usage="Generate Dirac Spinors"
Begin["Private`"]
GenerateSpinor[E_,p_,theta_,phi_,ParticleType_,SymmetryOperator_,Eigenvalue_]:=Module[{S1,S2,S3,S4},
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
{S1,S2,S3,S4}
];
End[]
EndPackage[]

(*Print[GenerateSpinor[E,P,theta,phi,"AP","Helicity","Right"]]*)



