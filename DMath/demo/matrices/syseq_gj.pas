{ **********************************************************************
  *                        Program SYSEQ_GJ.PAS                        *
  *                            Version 1.0d                            *
  *                   (c) J. Debord, September 2003                    *
  **********************************************************************
  This program solves a system of linear equations (A * X = B) with
  several constant vectors by the Gauss-Jordan method. The system is
  stored in a data file with the following structure :

    Line  1            : Size of matrix A (N)
    Lines 2 to (N + 1) : Matrix A
    Line (N + 2)       : Number of columns of matrix B (M)
    Lines (N + 3) etc. : Matrix B

  The file MATRIX2.DAT is an example data file with N = 4 and M = 5
  ********************************************************************** }

program SysEq_GJ;

uses
  FMath, Matrices;

  procedure WriteMatrix(Title : String; A : TMatrix; N, M : Integer);
{ ----------------------------------------------------------------------
  Writes a matrix A (N x M)
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn(Title, ' :');
    WriteLn;
    for I := 1 to N do
      begin
        for J := 1 to M do
          Write(A[I,J]:12:6);
        WriteLn;
      end;
    WriteLn;
  end;

var
  A, B, X : TMatrix;  { System matrices }
  A_inv   : TMatrix;  { Inverse matrix }
  N, M    : Integer;  { Matrix dimensions }
  D       : Float;    { Determinant }
  F       : Text;     { Data file }
  I, J    : Integer;  { Loop variables }

begin
  Assign(F, 'matrix2.dat');
  Reset(F);

  { Read matrix A }
  Read(F, N);
  DimMatrix(A, N, N);
  for I := 1 to N do
    for J := 1 to N do
      Read(F, A[I,J]);

  { Read matrix B }
  Read(F, M);
  DimMatrix(B, N, M);
  for I := 1 to N do
    for J := 1 to M do
      Read(F, B[I,J]);

  Close(F);

  { Display data }
  WriteMatrix('System matrix', A, N, N);
  WriteMatrix('Constant vectors', B, N, M);

  { Dimension inverse matrix and solution matrix }
  DimMatrix(A_inv, N, M);
  DimMatrix(X, N, M);

  { Solve system }
  case GaussJordan(A, B, 1, N, M, A_inv, X, D) of
    MAT_OK     : WriteMatrix('Solution vectors', X, N, M);
    MAT_SINGUL : Write('Singular matrix!');
  end;
end.

