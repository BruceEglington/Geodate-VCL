{ **********************************************************************
  *                        Program CORREL.PAS                          *
  *                           Version 1.2d                             *
  *                     (c) J. Debord, July 2002                       *
  **********************************************************************
  This program computes the correlation matrix of a series of variables
  read from an ASCII file with the following format:

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

program correl;

uses
  fmath, matrices, regress;

var
  FileName : String;      { Name of input file }
  Title    : String;      { Title of study }
  N        : Integer;     { Number of observations }
  Nvar     : Integer;     { Number of variables }
  XName    : TStrVector;  { Variable names }
  X        : TMatrix;     { Variables }
  M        : TVector;     { Mean vector }
  V        : TMatrix;     { Variance-covariance matrix }
  R        : TMatrix;     { Correlation matrix }

  procedure ReadMatrix(FileName : String; var Title : String;
                       var X : TMatrix; var XName : TStrVector;
                       var N, Nvar : Integer);
{ ----------------------------------------------------------------------
  Reads data matrix from file. Note that the arrays are passed as VAR
  parameters because they are dimensioned inside the procedure.
  ---------------------------------------------------------------------- }
  var
    F    : Text;     { Data file }
    I, K : Integer;  { Loop variable }
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
  Writes vector on screen
  ---------------------------------------------------------------------- }
  var
    I : Integer;
  begin
    WriteLn(Title, ' :', #10);
    for I := 1 to Nvar do
      WriteLn(B[I]:12:4);
    WriteLn;
  end;

  procedure WriteMatrix(Title : String; A : TMatrix; Nvar : Integer);
{ ----------------------------------------------------------------------
  Writes matrix on screen
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn(Title, ' :', #10);
    for I := 1 to Nvar do
      begin
        for J := 1 to Nvar do
          Write(A[I,J]:12:4);
        WriteLn;
      end;
    WriteLn;
  end;

begin
  { Read matrix from file }
  FileName := 'pca.dat';
  ReadMatrix(FileName, Title, X, XName, N, Nvar);

  { Dimension arrays }
  DimVector(M, Nvar);
  DimMatrix(V, Nvar, Nvar);
  DimMatrix(R, Nvar, Nvar);

  { Compute mean vector }
  VecMean(X, N, 1, Nvar, M);

  { Compute variance-covariance matrix }
  MatVarCov(X, N, 1, Nvar, M, V);

  { Compute correlation matrix }
  MatCorrel(V, 1, Nvar, R);

  { Display results }
  WriteLn;
  WriteVector('Mean vector', M, Nvar);
  WriteMatrix('Variance-covariance matrix', V, Nvar);
  WriteMatrix('Correlation matrix', R, Nvar);
end.
