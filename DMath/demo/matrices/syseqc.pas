{ **********************************************************************
  *                         Program SYSEQC.PAS                         *
  *                            Version 1.3d                            *
  *                      (c) J. Debord, July 2002                      *
  **********************************************************************
  This program solves a system of linear equations with complex
  coefficients (A * X = B) by LU decomposition. The system is stored
  in a data file with the following structure :

    Line  1            : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix, followed by constant vector

  Complex numbers are given in rectangular form : real part, followed
  by imaginary part.

  The file MATRIX3.DAT is an example data file with N = 2

  LU decomposition factors the square matrix A as a product L * U, where
  L is a lower triangular matrix (with unit diagonal terms) and U is an
  upper triangular matrix. Then the system is solved as two subsystems,
  L * Z = B and U * X = Z, taking advantage of the triangular nature of
  the matrices.
  ********************************************************************** }

program syseqc;

uses
  fmath, matrices, pastring;

var
  A : TCompMatrix;  { System matrix }
  B : TCompVector;  { Constant vector }
  X : TCompVector;  { Solution vector }
  N : Integer;      { Dimension of matrix }

  procedure ReadSystem(FileName : String; var A : TCompMatrix;
                       var B : TCompVector; var N : Integer);
{ ----------------------------------------------------------------------
  Reads data from file. Note that matrices and vectors are passed as VAR
  parameters because they are dimensioned inside the procedure.
  ---------------------------------------------------------------------- }
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variables }
    X, Y : Float;    { Real and imaginary parts }
  begin
    Assign(F, FileName);
    Reset(F);

    { Read matrix and constant vector }
    Read(F, N);
    DimMatrix(A, N, N);
    DimVector(B, N);

    for I := 1 to N do
      begin
        { Read line I of matrix }
        for J := 1 to N do
          begin
            Read(F, X, Y);
            A[I,J] := Cmplx(X, Y);
          end;
        { Read element I of constant vector }
        Read(F, X, Y);
        B[I] := Cmplx(X, Y);
      end;

    Close(F);
  end;

  procedure WriteMatrix(Title : String; A : TCompMatrix; N : Integer);
{ ----------------------------------------------------------------------
  Writes system matrix on screen
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn(Title, ' :', #10);
    for I := 1 to N do
      begin
        for J := 1 to N do
          Write(Comp2Str(A[I,J]));
        WriteLn;
      end;
    WriteLn;
  end;

  procedure WriteVector(Title : String; B : TCompVector; N : Integer);
{ ----------------------------------------------------------------------
  Writes constant vector or solution vector.
  ---------------------------------------------------------------------- }
  var
    I : Integer;
  begin
    WriteLn(Title, ' :', #10);
    for I := 1 to N do
      WriteLn(Comp2Str(B[I]));
    WriteLn;
  end;

begin
  WriteLn;

  { Read and display data }
  ReadSystem('matrix3.dat', A, B, N);
  WriteMatrix('System matrix', A, N);
  WriteVector('Constant vector', B, N);

  { Dimension solution vector }
  DimVector(X, N);

  { Perform LU decomposition of A. If successful, solve system }
  if LU_Decomp(A, 1, N) = 0 then
    begin
      LU_Solve(A, B, 1, N, X);
      WriteVector('Solution vector', X, N);
    end
  else
    Write('Singular matrix!');
end.

