MGamma(4)=[1 0 0 0; 0 1 0 0; 0 0 -1 0; 0 0 0 -1];
MGamma(1)=[0 0 0 1; 0 0 1 0; 0 -1 0 0; -1 0 0 0];
MGamma(2)=[0 0 0 -1i; 0 0 1i 0; 0 1i 0 0; -1i 0 0 0];
MGamma(3)=[0 0 1 0; 0 0 0 -1; -1 0 0 0; 0 1 0 0];
MBeta=MGamma(4);
MAlpha(1)=MGamma(4)*MGamma(1);
MAlpha(2)=MGamma(4)*MGamma(2);
MAlpha(3)=MGamma(4)*MGamma(3);