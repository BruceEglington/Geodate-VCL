{ **********************************************************************
  *                         Program EIGENSYM.PAS                       *
  *                             Version 1.5d                           *
  *                     (c) J. Debord, February 2004                   *
  **********************************************************************
  This program computes the eigenvalues and eigenvectors of a symmetric
  matrix by the iterative method of Jacobi. The method is demonstrated
  with Hilbert matrices. These matrices are ill-conditioned (i.e. the
  ratio of the lowest to the highest eigenvalue is very low).

  The Jacobi method applies a series of rotations to the original matrix
  in order to transform it into a diagonal matrix. The diagonal terms of
  this matrix are the eigenvalues. The product of the rotation matrices
  gives the eigenvectors. The original matrix is destroyed during the
  process.

  The variable TOL defines the tolerance with which an off-diagonal
  element of the transformed matrix is considered zero (expressed as
  a fraction of the sum of squared diagonal terms). The constant
  MAXITER defines the maximal number of iterations allowed. These two
  constants are linked, i.e. decreasing TOL may need increasing MAXITER
  to avoid non-convergence of the Jacobi procedure.
  ********************************************************************** }

program eigensym;

uses
  fmath, matrices, eigen;

const
  MAXITER = 1000; { Maximum number of iterations }

var
  N      : Integer;  { Size of matrix }
  A      : TMatrix;  { Matrix }
  V      : TMatrix;  { Eigenvectors }
  Lambda : TVector;  { Eigenvalues }
  Tol    : Float;    { Required precision }

  procedure Hilbert(A : TMatrix; N : Integer);
{ ----------------------------------------------------------------------
  Generates the Hilbert matrix of order N

        ( 1      1/2     1/3     1/4     ... 1/N      )
        ( 1/2    1/3     1/4     1/5     ... 1/(N+1)  )
    A = ( 1/3    1/4     1/5     1/6     ... 1/(N+2)  )
        ( ........................................... )
        ( 1/N    1/(N+1) 1/(N+2) 1/(N+3) ... 1/(2N-1) )

  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    { First row of matrix }
    A[1,1] := 1.0;
    for J := 2 to N do
      A[1,J] := 1.0 / J;

    for I := 2 to N do
      begin
        { Last column of matrix }
        A[I,N] := 1.0 / (N + I - 1);
        { Fill matrix }
        for J := 1 to N - 1 do
          A[I,J] := A[I - 1,J + 1];
      end;
  end;

  procedure WriteResults(N : Integer; V : TMatrix; Lambda : TVector);
{ ----------------------------------------------------------------------
  Outputs results to screen
  ---------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn;
    WriteLn('Eigenvalues :', #10);
    for I := 1 to N do
      WriteLn(Lambda[I]:26);

    if N < 8 then
      begin
        WriteLn(#10'Eigenvectors (columns) :', #10);
        for I := 1 to N do
          begin
            for J := 1 to N do
              Write(V[I,J]:10:6);
            WriteLn;
          end;
      end;
  end;

begin
  Tol := Sqrt(MACHEP);
  repeat
    WriteLn;
    Write('Order of Hilbert matrix (1 to end) : ');
    ReadLn(N);

    if N > 1 then
      begin
        { Allocate vectors and matrices }
        DimMatrix(A, N, N);
        DimMatrix(V, N, N);
        DimVector(Lambda, N);

        { Generate Hilbert matrix of order N }
        Hilbert(A, N);

        { Compute eigenvalues and eigenvectors }
        case Jacobi(A, 1, N, MAXITER, Tol, V, Lambda) of
          MAT_OK       : WriteResults(N, V, Lambda);
          MAT_NON_CONV : WriteLn('Too many iterations!');
        end;
      end;
  until N < 2;
end.
