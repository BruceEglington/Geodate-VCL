{ **********************************************************************
  *                        Program TESTPCA.PAS                         *
  *                           Version 1.2d                             *
  *                     (c) J. Debord, July 2002                       *
  **********************************************************************
  This program performs a principal component analysis of a series of
  variables read from an ASCII file with the following format:

    Line 1     : Title of study
    Line 2     : Number of variables (n)
    Next lines : Names of variables x1, x2, ..., xn
    Next line  : Number of observations

    The next lines contain the coordinates (x1, x2, ..., xn) of the
    observations (1 observation by line). The coordinates must be
    separated by spaces or tabulations.

  The sample file pca.dat is taken from:
  P. DAGNELIE, Analyse statistique a plusieurs variables,
  Presses Agronomiques de Gembloux, Belgique, 1982
  ********************************************************************** }

program testpca;

uses
  fmath, matrices, regress, pastring;

var
  FileName : String;      { Name of input file }
  N        : Integer;     { Number of observations }
  Nvar     : Integer;     { Number of variables }
  XName    : TStrVector;  { Variable names }
  X        : TMatrix;     { Variables }
  M        : TVector;     { Mean vector }
  S        : TVector;     { Standard deviations }
  V        : TMatrix;     { Variance-covariance matrix }
  R        : TMatrix;     { Correlation matrix }
  Lambda   : TVector;     { Eigenvalues of correlation matrix }
  C        : TMatrix;     { Eigenvectors of correlation matrix }
  Rc       : TMatrix;     { Correlations between variables & princ. factors }
  Z        : TMatrix;     { Scaled variables }
  F        : TMatrix;     { Principal factors }

  procedure ReadMatrix(FileName : String; var X : TMatrix;
                       var XName : TStrVector; var N, Nvar : Integer);
{ ----------------------------------------------------------------------
  Reads data matrix from file. Note that the arrays are passed as VAR
  parameters because they are dimensioned inside the procedure.
  ---------------------------------------------------------------------- }
  var
    F     : Text;     { Data file }
    I, K  : Integer;  { Loop variable }
    Title : String;   { Title of study }
  begin
    Assign(F, FileName);
    Reset(F);
    ReadLn(F, Title);
    ReadLn(F, Nvar);

    DimVector(XName, Nvar);
    for I := 1 to Nvar do
      ReadLn(F, XName[I]);

    ReadLn(F, N);
    DimMatrix(X, Nvar, N);

    for K := 1 to N do
      for I := 1 to Nvar do
        Read(F, X[I,K]);

    Close(F);
  end;

  procedure WriteVector(Title : String; B : TVector; Nvar : Integer);
{ ----------------------------------------------------------------------
  Writes vector B
  ---------------------------------------------------------------------- }
  var
    I : Integer;
  begin
    WriteLn(Title, ' :', #10);
    for I := 1 to Nvar do
      WriteLn(B[I]:10:4);
    WriteLn;
  end;

  procedure WriteMatrix(Title : String; A : TMatrix; Nvar : Integer);
{ ----------------------------------------------------------------------
  Writes the transpose of the square matrix A
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn(Title, ' :', #10);
    for J := 1 to Nvar do
      begin
        for I := 1 to Nvar do
          Write(A[I,J]:10:4);
        WriteLn;
      end;
    WriteLn;
  end;

  procedure WriteOutputFile(FileName : String; F : TMatrix; N, Nvar : Integer);
  var
    OutF : Text;
    OutFName : String;
    I, K : Integer;
  begin
    K := Pos('.', FileName);
    OutFName := Copy(FileName, 1, Pred(K)) + '.out';
    Assign(OutF, OutFName);
    Rewrite(OutF);

    WriteLn(OutF, 'Principal factors');
    WriteLn(OutF, Nvar);

    for I := 1 to Nvar do
      WriteLn(OutF, 'F' + Trim(Int2Str(I)));

    WriteLn(OutF, N);
    for K := 1 to N do
      begin
        for I := 1 to Nvar do
          Write(OutF, F[I,K]:10:4);
        WriteLn(OutF);
      end;

    Close(OutF);
    WriteLn('Principal factors saved in ', OutFName);
  end;

begin
  { Read matrix from file }
  FileName := 'pca.dat';
  ReadMatrix(FileName, X, XName, N, Nvar);

  { Dimension arrays }
  DimVector(M, Nvar);
  DimVector(S, Nvar);
  DimMatrix(V, Nvar, Nvar);
  DimMatrix(R, Nvar, Nvar);
  DimVector(Lambda, Nvar);
  DimMatrix(C, Nvar, Nvar);
  DimMatrix(Rc, Nvar, Nvar);
  DimMatrix(Z, Nvar, N);
  DimMatrix(F, Nvar, N);

  { Compute means and standard deviations }
  VecMean(X, N, 1, Nvar, M);
  VecSD(X, N, 1, Nvar, M, S);

  { Scale variables }
  ScaleVar(X, N, 1, Nvar, M, S, Z);

  { Compute variance-covariance matrix }
  MatVarCov(X, N, 1, Nvar, M, V);

  { Compute correlation matrix }
  MatCorrel(V, 1, Nvar, R);

  { Perform principal component analysis.
    The original matrix R is destroyed. }
  case PCA(R, 1, Nvar, Lambda, C, Rc) of
    MAT_OK : begin
               { Compute principal factors }
               PrinFac(Z, N, 1, Nvar, C, F);

               { Display results }
               WriteLn;
               WriteVector('Eigenvalues of correlation matrix', Lambda, Nvar);
               WriteMatrix('Eigenvectors (columns) of correlation matrix', C, Nvar);
               WriteMatrix('Correlations between factors (columns) and variables (lines)', Rc, Nvar);

               { Save principal factors }
               WriteOutputFile(FileName, F, N, Nvar);
             end;
    MAT_NON_CONV : WriteLn('Non-convergence of eigenvalue computation');
  end;

end.
