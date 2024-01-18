 { **********************************************************************
  *                        Program SYSEQ_QR.PAS                        *
  *                            Version 1.2d                            *
  *                    (c) J. Debord, October 2003                     *
  **********************************************************************
  This program solves a system of linear equations (A * X = B) with
  several constant vectors by QR decomposition. The system is stored
  in a data file with the following structure :

    Line  1            : dimension of the matrix (N)
    Lines 2 to (N + 1) : matrix
    Line  (N + 2)      : number of constant vectors (M)
    Lines (N + 3) etc  : constant vectors (one by column)

  The file MATRIX2.DAT is an example data file with N = 4 and M = 5

  The QR decomposition factors the matrix A (n x m, with n >= m) as a
  product QR where Q is a (n x m) column-orthogonal matrix and R a (m x m)
  upper triangular matrix. The system becomes QRX = B, then RX = Q'B, which
  is solved by back-substitution.

  If the system is impossible (n > m), the method returns a vector X such
  that A * X is the best approximation to B in the least-squares sense.
  ********************************************************************** }

program syseq_qr;

uses
  fmath, matrices;

var
  A, R   : TMatrix;  { System matrix and R factor }
  B      : TMatrix;  { Constant vectors }
  X      : TMatrix;  { Solutions }
  B1, X1 : TVector;  { Auxiliary vectors }
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
  DimMatrix(R, N, N);
  DimMatrix(X, N, M);

  DimVector(B1, N);
  DimVector(X1, N);

  { Perform QR decomposition of A }
  if QR_Decomp(A, 1, N, N, R) <> MAT_OK then
    begin
      Write('Singular matrix!');
      Halt;
    end;

  { Solve system for each constant vector }
  for I := 1 to M do
    begin
      CopyVectorFromCol(B1, B, 1, N, I);
      QR_Solve(A, R, B1, 1, N, N, X1);
      CopyColFromVector(X, X1, 1, N, I);
    end;

  WriteMatrix('Solution vectors', X, N, M);
end.

