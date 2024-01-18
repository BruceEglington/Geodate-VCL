 { **********************************************************************
  *                        Program SYSEQSVD.PAS                        *
  *                            Version 1.4d                            *
  *                    (c) J. Debord, October 2003                     *
  **********************************************************************
  This program solves a system of linear equations (A * X = B) with
  several constant vectors by singular value decomposition (SVD). The
  system is stored in a data file with the following structure :

    Line  1            : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix
    Line  (N + 2)      : number of constant vectors (M)
    Lines (N + 3) etc  : constant vectors (one by column)

  The file MATRIX2.DAT is an example data file with N = 4 and M = 5

  SVD factors the matrix A (n x m, with n >= m) as a product U * S * V'
  where U is a (n x m) column-orthogonal matrix, S a (m x m) diagonal
  matrix with elements >= 0 (the singular values) and V a (m x m)
  orthogonal matrix. The singular values which are close to zero (within
  a user-specified tolerance) are set to zero, and then the solution of
  the system is computed as X = V * Diag(1/s(i)) * U' * B, for s(i) <> 0
  With this editing of the singular values, the method may be applied to
  singular or quasi-singular matrices.

  If the matrix A is singular, two cases are possible:

  1) If the system is underdetermined, the method returns the vector X
     which has the smallest norm.
  2) If the system is impossible, the method returns a vector X such that
     A * X is the best approximation to B in the least-squares sense.
  ********************************************************************** }

program syseqsvd;

uses
  fmath, matrices;

const
  TOL = 1.0E-8;  { A singular value will be set to zero if it is   }
                 { lower than TOL times the highest singular value }
var
  A      : TMatrix;  { System matrix }
  S      : TVector;  { Singular values }
  V      : TMatrix;  { Orthogonal matrix from the SV decomposition }
  B      : TMatrix;  { Constant vectors }
  B1, X1 : TVector;  { Auxiliary vectors }
  X      : TMatrix;  { Solutions }
  N      : Integer;  { Dimension of matrix }
  M      : Integer;  { Number of constant vectors }
  I      : Integer;  { Loop variable }

  procedure ReadMatrices(FileName : String; var A, B : TMatrix;
                         var N, M : Integer);
{ ----------------------------------------------------------------------
  Reads data from file. Note that matrices are passed as VAR parameters
  because they are dimensioned inside the procedure.
  ---------------------------------------------------------------------- }
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variable }
  begin
    Assign(F, FileName);
    Reset(F);

    { Read matrix }
    Read(F, N);
    DimMatrix(A, N, N);
    for I := 1 to N do
      for J := 1 to N do
        Read(F, A[I,J]);

    { Read constant vectors }
    Read(F, M);
    DimMatrix(B, N, M);
    for I := 1 to N do
      for J := 1 to M do
        Read(F, B[I,J]);
    Close(F);
  end;

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

begin
  WriteLn;

  { Read and display data }
  ReadMatrices('matrix2.dat', A, B, N, M);
  WriteMatrix('System matrix', A, N, N);
  WriteMatrix('Constant vectors', B, N, M);

  { Dimension arrays }
  DimVector(S, N);
  DimMatrix(V, N, N);
  DimMatrix(X, M, N);

  DimVector(B1, N);
  DimVector(X1, N);

  { Perform SV decomposition of A
    Note that U is stored in place of A }
  if SV_Decomp(A, 1, N, N, S, V) <> MAT_OK then
    begin
      Write('Non-convergence of singular value decomposition!');
      Halt;
    end;

  { Set the lowest singular values to zero }
  SV_SetZero(S, 1, N, TOL);

  { Solve system for each constant vector }
  for I := 1 to M do
    begin
      CopyVectorFromCol(B1, B, 1, N, I);
      SV_Solve(A, S, V, B1, 1, N, N, X1);
      CopyColFromVector(X, X1, 1, N, I);
    end;

  WriteMatrix('Solution vectors', X, N, M);
end.

