(* ::Package:: *)

BeginPackage["General`ShowExpr`"]
ShowExpr::usage:="Print Expressions"
Begin["`Private`"]
ShowExpr[Name_]:=Module[{Temp,Expr},
	If[Name=="DiracEquation" || Name=="DiracEquationP",	
		Expr = TraditionalForm[HoldForm[(I*"\[Gamma]"^"\[Mu]"*"\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)"-"m")"\[Psi]"==0]];
		Print[Expr];
	];
	If[Name=="DiracEquationAP",	
		Expr = TraditionalForm[HoldForm[(I*"\[Gamma]"^"\[Mu]"*"\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)"+"m")"\[Psi]"==0]];
		Print[Expr];
	];
	If[Name=="KleinGordonEquation" || Name=="KGEquation",	
		Expr = TraditionalForm[HoldForm[("\!\(\*SuperscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)"*"\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)"+"m"^2)"\[Psi]"==0]];
		Print[Expr];
	];
	If[Name=="FixedProtonepScattering",
		Expr = TraditionalForm[HoldForm[ "|M|"^2 == (16*Pi^2*"\[Alpha]"^2*Subscript["m","p"]^2*Subscript["m","e"]^2/("p"^4*Sin["\[Theta]"/2]^4)(1+"\[Beta]"^2"\[Gamma]"^2Cos["\[Theta]"/2]^2))]];
		Print[Expr];
	];	
	If[Name=="RutherfordScattering",
		Expr = TraditionalForm[HoldForm[ ("d\[Sigma]"/"d\[CapitalOmega]" == "\[Alpha]"^2/(16*Subscript["E","K"]^2*Sin["\[Theta]"/2]^4))]];
		Print[Expr];
	];	
	Expr
];
End[];
EndPackage[];
ShowExpr["RutherfordScattering"];






