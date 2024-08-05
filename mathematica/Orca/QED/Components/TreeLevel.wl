(* ::Package:: *)

Get["Spinor`Components`DefineSpinor`"]
Get["Constants`Components`Matrices`"]
Get["Constants`Components`Tensors`"]
Get["LorentzBoost`Components`Operations`"]
BeginPackage["QED`TreeLevel`"]
Print[$Path];
QEDTreeLevelS::usage="QED tree level S channel matrix element";
QEDTreeLevelSFast::usage="QED tree level S channel matrix element by Lorentz invariant form";
QEDTreeLevelSFast2::usage="QED tree level S channelmatrix element by Lorentz invariant form 2";
QEDTreeLevelT::usage="QED tree level T channel matrix element";
QEDTreeLevelTFast::usage="QED tree level T channel matrix element by Lorentz invariant form";
QEDTreeLevelU::usage="QED tree level T channel matrix element";
QEDTreeLevelUFast::usage="QED tree level T channel matrix element by Lorentz invariant form";
Begin["`private`"]
QEDTreeLevelSFast[p1_,p2_,p3_,p4_]:=Module[{alpha,MM},
	alpha=Symbol["\[Alpha]"];
	(*MM=128*Pi*2*alpha^4/(LonrentzBoost`Operations`OVMultiply[LonrentzBoost`Operations`OVSubtract[p1,p3],LonrentzBoost`Operations`OVSubtract[p1,p3]])^2*(LonrentzBoost`Operations`OVMultiply[p1,p2]*LonrentzBoost`Operations`OVMultiply[p3,p4]+LonrentzBoost`Operations`OVMultiply[p1,p4]*LonrentzBoost`Operations`OVMultiply[p2,p3]-alpha^2*LonrentzBoost`Operations`OVMultiply[p1,p3]);
	*)
	MM=32*Pi^2*alpha^2*(LonrentzBoost`Operations`OVMultiply[p1,p3]^2+LonrentzBoost`Operations`OVMultiply[p1,p4]^2)/LonrentzBoost`Operations`OVMultiply[p1,p2]^2;
	MM
];

QEDTreeLevelSFast2[p1_,p2_,p3_,p4_]:=Module[{tt,uu,ss,alpha,MM},
	alpha=Symbol["\[Alpha]"];
	tt=LonrentzBoost`Operations`OVMultiply[LonrentzBoost`Operations`OVSubtract[p1,p3],LonrentzBoost`Operations`OVSubtract[p1,p3]];
	uu=LonrentzBoost`Operations`OVMultiply[LonrentzBoost`Operations`OVSubtract[p1,p4],LonrentzBoost`Operations`OVSubtract[p1,p4]];
	ss=LonrentzBoost`Operations`OVMultiply[LonrentzBoost`Operations`OVAdd[p1,p2],LonrentzBoost`Operations`OVAdd[p1,p2]];
	MM=32*Pi^2*alpha^2*(tt^2+uu^2)/ss^2;
	MM
];
QEDTreeLevelS[p1_,p2_,p3_,p4_]:=Module[{j1RR,j1RL,j1LR,j1LL,j2RR,j2RL,j2LR,j2LL,Config1, Config2,alpha,qsq, MM},
	j1RR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1RR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"AP","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j1RL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1RL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"AP","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Left"][["Value"]]))[[1,1]]
	];
	j1LR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1LR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"AP","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j1LL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1LL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"AP","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Left"][["Value"]]))[[1,1]]
	];
	j2RR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2RR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"AP","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j2RL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2RL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"AP","Helicity","Left"][["Value"]]))[[1,1]]
	];
	j2LR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2LR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"AP","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j2LL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2LL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"AP","Helicity","Left"][["Value"]]))[[1,1]]
	];
	Config1={j1LL,j1LR,j1RL,j1RR};
	Config2={j2LL,j2LR,j2RL,j2RR};
	
	MM=0;
	alpha=Symbol["\[Alpha]"];
	qsq=(p1["E"]+p2["E"])^2-(p1["px"]+p2["px"])^2-(p1["py"]+p2["py"])^2-(p1["pz"]+p2["pz"])^2;
	Print[qsq];
	For[i=1,i<5,i++,
		For[j=1,j<5,j++,
			MM=MM+(1/4*(-4*Pi*alpha/qsq)^2*(Config2[[i]] . Constants`Tensors`Metg . Transpose[Config1[[j]]])*Conjugate[Config2[[i]] . Constants`Tensors`Metg . Transpose[Config1[[j]]]])
		]
	];
	MM=MM[[1,1]];
	MM
];

QEDTreeLevelT[p1_,p2_,p3_,p4_]:=Module[{j1RR,j1RL,j1LR,j1LL,j2RR,j2RL,j2LR,j2LL,Config1, Config2,alpha,qsq, MM},
	j1RR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1RR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j1RL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1RL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Left"][["Value"]]))[[1,1]]
	];
	j1LR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1LR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j1LL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j1LL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p3[["E"]],p3[["p"]],p3[["theta"]],p3[["phi"]],"P","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p1[["E"]],p1[["p"]],p1[["theta"]],p1[["phi"]],"P","Helicity","Left"][["Value"]]))[[1,1]]
	];
	j2RR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2RR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"P","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"P","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j2RL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2RL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"P","Helicity","Right"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"P","Helicity","Left"][["Value"]]))[[1,1]]
	];
	j2LR={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2LR[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"P","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"P","Helicity","Right"][["Value"]]))[[1,1]]
	];
	j2LL={{0,0,0,0}};
	For[ i=1,i<5,i++,
		j2LL[[1,i]]=((Spinor`DefineSpinor`GenerateASpinorPolar[p4[["E"]],p4[["p"]],p4[["theta"]],p4[["phi"]],"P","Helicity","Left"][["Value"]]) . Constants`Matrices`MGamma[[i]] . (Spinor`DefineSpinor`GenerateSpinorPolar[p2[["E"]],p2[["p"]],p2[["theta"]],p2[["phi"]],"P","Helicity","Left"][["Value"]]))[[1,1]]
	];
	Config1={j1LL,j1LR,j1RL,j1RR};
	Config2={j2LL,j2LR,j2RL,j2RR};
	
	MM=0;
	alpha=Symbol["\[Alpha]"];
	qsq=(p1["E"]-p3["E"])^2-(p1["px"]-p3["px"])^2-(p1["py"]-p3["py"])^2-(p1["pz"]-p3["pz"])^2;
	For[i=1,i<5,i++,
		For[j=1,j<5,j++,
			MM=MM+(1/4*(-4*Pi*alpha/qsq)^2*(Config1[[i]] . Constants`Tensors`Metg . Transpose[Config2[[j]]])*Conjugate[Config1[[i]] . Constants`Tensors`Metg . Transpose[Config2[[j]]]])
		]
	];
	MM=MM[[1,1]];
	MM
];

QEDTreeLevelU[p1_,p2_,p3_,p4_]:=Module[{MM},
	MM=QEDTreeLevelT[p1,p2,p4,p3];
	MM
];

End[]
EndPackage[]
Get["LorentzBoost`Components`DefineVector`"]
EE=Symbol["EE"]
Assuming[EE>0,Simplify[Conjugate[Sqrt[EE]]]]
theta=Symbol["theta"]
Assuming[theta<=Pi,theta>=0]
P1theta=0; P1phi=0;P1E=EE; 
P2theta=Pi; P2phi=Pi;P2E=EE;
P3theta=theta;P3phi=0;P3E=EE;
P4theta=Pi-theta;P4phi=Pi;P4E=EE;
p1v=MakeMomentumPolar[P1E,P1E,P1theta,P1phi]
p2v=MakeMomentumPolar[P2E,P2E,P2theta,P2phi]
p3v=MakeMomentumPolar[P3E,P3E,P3theta,P3phi]
p4v=MakeMomentumPolar[P4E,P4E,P4theta,P4phi]

MM=QEDTreeLevelSFast2[p1v,p2v,p3v,p4v];
MM=Assuming[{EE>0,Pi>=theta>=0},Simplify[MM]]











