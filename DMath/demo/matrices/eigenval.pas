{ **********************************************************************
  *                        Program EIGENVAL.PAS                        *
  *                            Version 1.2d                            *
  *                    (c) J. Debord, February 2003                    *
  **********************************************************************
  This program computes the eigenvalues of a general square matrix (see
  EIGENSYM.PAS for a symmetric matrix). The matrix is stored in a data
  file with the following structure :

    Line 1             : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix

  The file MATRIX1.DAT is an example data file with N = 4
  ********************************************************************** }

program eigenval;

uses
  fmath, matrices, eigen;

var
  A         : TMatrix;  { Matrix }
  N         : Integer;  { Dimension of matrix }
  Lambda_Re,
  Lambda_Im : TVector;  { Eigenvalues (real and imaginary parts) }
  I         : Integer;  { Loop variable }
  ErrCode   : Integer;  { Error code }

  procedure ReadMatrix(FileName : String; var A : TMatrix;
                       var N : Integer);
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

  procedure WriteMatrix(Title : String; A : TMatrix; N : Integer);
{ ----------------------------------------------------------------------
  Writes matrix on screen
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn;
    WriteLn(Title, ':');
    WriteLn;
    for I := 1 to N do
      begin
        for J := 1 to N do
          Write(A[I,J]:12:6);
        WriteLn;
      end;
    WriteLn;
  end;

  procedure WriteEigenValue(Lambda_Re, Lambda_Im : TVector; I : Integer);
{ ----------------------------------------------------------------------
  Writes the I-th eigenvalue
  ---------------------------------------------------------------------- }
  begin
    if Lambda_Im[I] = 0.0 then
      WriteLn(Lambda_Re[I]:12:6)
    else
      begin
        Write(Lambda_Re[I]:12:6);
        if Lambda_Im[I] > 0.0 then Write(' + ') else Write(' - ');
        WriteLn(Abs(Lambda_Im[I]):12:6, ' * i');
      end;
  end;

begin
  { Read matrix from file }
  ReadMatrix('matrix1.dat', A, N);
  WriteMatrix('Original matrix', A, N);

  { Dimension the vectors containing the eigenvalues }
  DimVector(Lambda_Re, N);
  DimVector(Lambda_Im, N);

  { Compute eigenvalues }
  ErrCode := EigenVals(A, 1, N, Lambda_Re, Lambda_Im);
  if ErrCode = 0 then
    begin
      WriteLn('Eigenvalues:');
      WriteLn;
      for I := 1 to N do
        WriteEigenValue(Lambda_Re, Lambda_Im, I);
    end
  else
    begin
      WriteLn('Unable to find eigenvalues Lambda[1] to Lambda[', -ErrCode, ']');
      WriteLn('Eigenvalues Lambda[', 1 - ErrCode, '] to Lambda[', N, ']:');
      for I := 1 - ErrCode to N do
        WriteEigenValue(Lambda_Re, Lambda_Im, I);
    end;
end.

