(* ::Package:: *)

BeginPackage["LorentzBoost`DefineVector`"]
MakeVector::usage="Constructor of the Class Vector"
MakeMomentum::suage="Constructor of the Class Momentum"
MakeMomentumPolar::usage="Constructor of the Class Momentum with polar coordinates"
Begin["`Private`"]
MakeVector[T_,X_,Y_,Z_]:=Module[{Vector},
	Vector=<|
		"ClassName"->"Vector",
		1->X,
		2->Y,
		3->Z,
		4->T,
		"Value"->{{X},{Y},{Z},{T}}
	|>;
	Vector
]
MakeMomentum[E_,px_,py_,pz_]:=Module[{Momentum},
	Momentum=MakeVector[E,px,py,pz];
	Momentum["ClassName"]="Momentum";
	Momentum = Append[Momentum, <|"E" -> E|>];
	Momentum = Append[Momentum, <|"px" -> px|>];
	Momentum = Append[Momentum, <|"py" -> py|>];
	Momentum = Append[Momentum, <|"pz" -> pz|>];
	Momentum = Append[Momentum, <|"p" -> Sqrt[px^2+py^2+pz^2]|>];
	If[RealValuedNumberQ[px] && RealValuedNumberQ[py] && RealValuedNumberQ[pz] && px==0 && py == 0 && pz==0,
		Momentum = Append[Momentum, <|"theta" -> 0|>],
		Momentum = Append[Momentum, <|"theta" -> ArcTan[pz,px^2+py^2]|>]
	];
	If[RealValuedNumberQ[px] && RealValuedNumberQ[py] && px==0 && py == 0 ,
		Momentum = Append[Momentum, <|"phi" -> 0|>],
		Momentum = Append[Momentum, <|"phi" -> ArcTan[px,py]|>]
	];
	Momentum = Append[Momentum, <|"mass" -> Sqrt[E^2-(px^2+py^2+pz^2)]|>];
	Momentum
]
MakeMomentumPolar[E_,p_,theta_,phi_]:=Module[{px,py,pz,Momentum},
	px=p*Sin[theta]*Cos[phi];
	py=p*Sin[theta]*Sin[phi];
	pz=p*Cos[theta];
	Momentum=MakeVector[E,px,py,pz];
	Momentum["ClassName"]="Momentum";
	Momentum = Append[Momentum, <|"E" -> E|>];
	Momentum = Append[Momentum, <|"px" -> px|>];
	Momentum = Append[Momentum, <|"py" -> py|>];
	Momentum = Append[Momentum, <|"pz" -> pz|>];
	Momentum = Append[Momentum, <|"p" -> p|>];
	Momentum = Append[Momentum, <|"theta" -> theta|>];
	Momentum = Append[Momentum, <|"phi" -> phi|>];
	Momentum = Append[Momentum, <|"mass" -> Sqrt[E^2-p^2]|>];
	Momentum
]
End[]
EndPackage[]
MakeMomentumPolar[EE,p,theta,phi]

