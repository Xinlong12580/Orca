MGamma(4).Dirac=[1 0 0 0; 0 1 0 0; 0 0 -1 0; 0 0 0 -1];
MGamma(1).Dirac=[0 0 0 1; 0 0 1 0; 0 -1 0 0; -1 0 0 0];
MGamma(2).Dirac=[0 0 0 -1i; 0 0 1i 0; 0 1i 0 0; -1i 0 0 0];
MGamma(3).Dirac=[0 0 1 0; 0 0 0 -1; -1 0 0 0; 0 1 0 0];
MBeta.Dirac=MGamma(4).Dirac;
MAlpha(1).Dirac=MGamma(4).Dirac*MGamma(1).Dirac;
MAlpha(2).Dirac=MGamma(4).Dirac*MGamma(2).Dirac;
MAlpha(3).Dirac=MGamma(4).Dirac*MGamma(3).Dirac;