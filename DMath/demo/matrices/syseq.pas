{ **********************************************************************
  *                           Program SYSEQ.PAS                        *
  *                              Version 1.5d                          *
  *                     (c) J. Debord, September 2003                  *
  **********************************************************************
  This program solves a system of linear equations, of the form AX = B,
  by the method of Gauss-Jordan. The method is demonstrated by solving
  a series of Hilbert systems of increasing order. Hilbert systems have
  ill-conditioned matrices (i.e. with determinants close to zero), so
  that the matrix is considered singular for an order which depends on
  the numerical precision of the software.

  This example also shows that vectors and matrices may be redimensioned
  several times.

  To adapt this program to other systems of equations, you must replace
  the main loop by the instructions which read the data, followed by a
  call to the GaussJordan function. Don't forget to allocate all arrays
  before using them. For instance :

        Write('Order of system : ');
        Readln(N);
        DimMatrix(A, N, N);
        DimVector(B, N);
        DimMatrix(A_inv, N, N);
        DimVector(X, N);
        ........................................................
        instructions (or call to subroutine) reading A and B
        ........................................................
        ErrCode := GaussJordan(A, B, 1, N, A_inv, X, Det)

  where ErrCode is an integer variable which contains the error code
  returned by the function (see the documentation of function GaussJordan
  in unit MATRICES.PAS).
  ********************************************************************** }

program syseq;

uses
  fmath, matrices;

var
  N       : Integer;  { Order of the system }
  ErrCode : Integer;  { Error code }
  A       : TMatrix;  { System matrix }
  B       : TVector;  { Constant vector }
  A_inv   : TMatrix;  { Inverse matrix }
  X       : TVector;  { Solution vector }
  Det     : Float;    { Determinant }

procedure Hilbert(A : TMatrix; B : TVector; N : Integer);
{ Generates the Hilbert system of order N

  A[1..N, 1..N] = system matrix :

        ( 1      1/2     1/3     1/4     ... 1/N      )
        ( 1/2    1/3     1/4     1/5     ... 1/(N+1)  )
    A = ( 1/3    1/4     1/5     1/6     ... 1/(N+2)  )
        ( ........................................... )
        ( 1/N    1/(N+1) 1/(N+2) 1/(N+3) ... 1/(2N-1) )

  B[1..N] = vector of constant terms :

            N
    B[i] = Sum A[i,j]
           j=1

  The solution vector is X = [1 1 1 ... 1]  }

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
        A[I,J] := A[I - 1, J + 1];
    end;

  { Constant vector }
  for I := 1 to N do
    begin
      B[I] := 0.0;
      for J := 1 to N do
        B[I] := B[I] + A[I,J];
    end;
end;

procedure WriteSolution(ErrCode, N : Integer;
                        A          : TMatrix;
                        B, X       : TVector;
                        Det        : Float);
{ Outputs results to screen }
var
  I, J : Integer;
begin
  WriteLn;
  Writeln('HILBERT SYSTEM OF ORDER ', N, #10);
  if ErrCode = MAT_SINGUL then
    begin
      Writeln('Quasi-singular matrix !');
      Exit;
    end;

  if N < 7 then
    begin
      Writeln('System matrix and constant vector :', #10);
      for I := 1 to N do
        begin
          for J := 1 to N do
            Write(A[I,J]:10:6);
          Writeln(B[I]:10:6);
        end;
      Writeln;
    end;

  Writeln('Solution vector :', #10);

  for I := 1 to N do
    Writeln(X[I]:10:6);

  WriteLn;
  WriteLn('Determinant =', Det:10);
  WriteLn;

  Write('Press <Enter> ...');
  ReadLn;
end;

begin
  { Initialize }
  N := 1;
  ErrCode := 0;

  { Main loop }
  while ErrCode = 0 do
    begin
      { Set system order }
      Inc(N);

      { Allocate (or re-allocate) vectors and matrices }
      DimMatrix(A, N, N);
      DimVector(B, N);
      DimMatrix(A_inv, N, N);
      DimVector(X, N);

      { Generate Hilbert system of order N }
      Hilbert(A, B, N);

      { Solve Hilbert system }
      ErrCode := GaussJordan(A, B, 1, N, A_inv, X, Det);

      { Write solution }
      WriteSolution(ErrCode, N, A, B, X, Det);
    end;
end.
