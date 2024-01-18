{ **********************************************************************
  *                        Program EIGENVEC.PAS                        *
  *                            Version 1.5d                            *
  *                    (c) J. Debord, February 2003                    *
  **********************************************************************
  This program computes the eigenvalues and eigenvectors of a
  general square matrix.

  The matrix is stored in a data file with the following structure :

    Line 1             : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix

  The file MATRIX1.DAT is an example data file with N = 4.
  ********************************************************************** }

program eigenvec;

uses
  fmath, matrices, eigen;

var
  A         : TMatrix;  { Matrix }
  N         : Integer;  { Dimension of matrix }
  Lambda_Re,
  Lambda_Im : TVector;  { Eigenvalues (real and imaginary parts) }
  V         : TMatrix;  { Eigenvectors }
  I         : Integer;  { Loop variable }
  ErrCode   : Integer;  { Error code }

  procedure ReadMatrix(FileName : String; var A : TMatrix; var N : Integer);
{ ----------------------------------------------------------------------
  Reads matrix from file. Note that A is passed as a VAR parameter
  because it is dimensioned inside the procedure.
  ---------------------------------------------------------------------- }
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variable }
  begin
    Assign(F, FileName);
    Reset(F);
    Read(F, N);
    DimMatrix(A, N, N);
    for I := 1 to N do
      for J := 1 to N do
        Read(F, A[I,J]);
    Close(F);
  end;

  procedure WriteNumber(Re, Im : Float);
{ ----------------------------------------------------------------------
  Writes a real or complex number
  ---------------------------------------------------------------------- }
  begin
    Write(Re:12:6);
    if Im = 0.0 then
      WriteLn
    else if Im > 0.0 then
      WriteLn(' + ', Im:12:6, ' * i')
    else
      WriteLn(' - ', -Im:12:6, ' * i')
  end;

  procedure WriteEigenValue(Lambda_Re, Lambda_Im : TVector; I : Integer);
{ ----------------------------------------------------------------------
  Writes the I-th eigenvalue
  ---------------------------------------------------------------------- }
  begin
    WriteLn; Write('Eigenvalue: ');
    WriteNumber(Lambda_Re[I], Lambda_Im[I]);
  end;

  procedure WriteEigenVector(Lambda_Re, Lambda_Im : TVector;
                             V : TMatrix; N, I : Integer);
{ ----------------------------------------------------------------------
  Writes the I-th eigenvector
  ---------------------------------------------------------------------- }
  var
    K : Integer;
  begin
    WriteLn; WriteLn('Eigenvector: '); WriteLn;
    if Lambda_Im[I] = 0.0 then
      { Eigenvector is in column I of V }
      for K := 1 to N do
        WriteNumber(V[K,I], 0.0)
    else if Lambda_Im[I] > 0.0 then
      { Real and imag. parts of eigenvector are in columns I and (I+1) }
      for K := 1 to N do
        WriteNumber(V[K,I], V[K,I+1])
    else
      { Conjugate of eigenvector is in columns (I-1) and I }
      for K := 1 to N do
        WriteNumber(V[K,I-1], - V[K,I]);
  end;

begin
  ReadMatrix('matrix1.dat', A, N);

  DimVector(Lambda_Re, N);
  DimVector(Lambda_Im, N);
  DimMatrix(V, N, N);

  { Compute eigenvalues and eigenvectors (A is destroyed) }
  ErrCode := EigenVect(A, 1, N, Lambda_Re, Lambda_Im, V);

  { Display results }
  if ErrCode = 0 then
    for I := 1 to N do
      begin
        WriteEigenValue(Lambda_Re, Lambda_Im, I);
        WriteEigenVector(Lambda_Re, Lambda_Im, V, N, I);
        WriteLn; Write('Press <Enter> to continue...');
        ReadLn;
      end
  else
    begin
      WriteLn('Unable to find eigenvalues Lambda[1] to Lambda[', -ErrCode, ']');
      WriteLn('Eigenvalues Lambda[', 1 - ErrCode, '] to Lambda[', N, ']:');
      for I := 1 - ErrCode to N do
        WriteEigenValue(Lambda_Re, Lambda_Im, I);
    end;

end.

