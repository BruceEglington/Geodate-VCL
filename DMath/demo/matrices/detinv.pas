{ **********************************************************************
  *                         Program DETINV.PAS                         *
  *                            Version 1.2d                            *
  *                   (c) J. Debord, September 2003                    *
  **********************************************************************
  This program computes the determinant and inverse of a square matrix.
  The matrix is stored in a data file with the following structure :

    Line 1             : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix

  The file MATRIX1.DAT is an example data file with N = 4
  ********************************************************************** }

program detinv;

uses
  fmath, matrices;

var
  A, A_inv : TMatrix;  { Matrix and its inverse }
  N        : Integer;  { Dimension of matrix }
  Det      : Float;    { Determinant }

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
    WriteLn(Title, ' :', #10);
    for I := 1 to N do
      begin
        for J := 1 to N do
          Write(A[I,J]:12:6);
        WriteLn;
      end;
    WriteLn;
  end;

begin
  { Read matrix from file }
  ReadMatrix('matrix1.dat', A, N);
  WriteMatrix('Original matrix', A, N);

  { Dimension inverse matrix }
  DimMatrix(A_inv, N, N);

  { Compute inverse matrix and determinant using the InvDet function }
  if InvDet(A, 1, N, A_inv, Det) <> MAT_OK then
    begin
      WriteLn('Singular matrix!');
      Halt;
    end;

  { Write results }
  WriteMatrix('Inverse matrix', A_inv, N);
  WriteLn('Determinant = ', Det:12:6, #10);

  { Reinvert inverse matrix using the InvMat function }
  if InvMat(A_inv, 1, N, A) = MAT_OK then
    WriteMatrix('Reinverted inverse matrix', A, N);
end.
