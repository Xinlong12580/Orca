(* ::Package:: *)

BeginPackage["Constants`Matrices`"]
MGamma::usage = "Gamma matrices";
MBeta::usage="Beta matrix";
MAlpha::usage="Alpha matrices";
Begin["`Private`"]

MGamma[1]={{0,0,0,1},{0,0,1,0},{0,-1,0,0},{-1,0,0,0}}
MGamma[2]={{0,0,0,-I},{0,0,I,0},{0,I,0,0},{-I,0,0,0}}
MGamma[3]={{0,0,1,0},{0,0,0,-1},{-1,0,0,0},{0,1,0,0}}
MGamma[4]={{1,0,0,0},{0,1,0,0},{0,0,-1,0},{0,0,0,-1}}
MBeta=MGamma[4]
MAlpha[1]=MGamma[4] . MGamma[1]
MAlpha[2]=MGamma[4] . MGamma[2]
MAlpha[3]=MGamma[4] . MGamma[3]
End[]
EndPackage[]
