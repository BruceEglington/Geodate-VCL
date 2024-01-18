{ **********************************************************************
  *                          Unit MATRICES.PAS                         *
  *                            Version 2.4d                            *
  *                   (c) J. Debord, September 2003                    *
  **********************************************************************
  This unit implements vector and matrix operations with dynamic arrays.
  There are 10 types available:

    TVector,     TMatrix      for floating point arrays
    TIntVector,  TIntMatrix   for integer arrays
    TCompVector, TCompMatrix  for complex arrays
    TBoolVector, TBoolMatrix  for boolean arrays
    TStrVector,  TStrMatrix   for string arrays

  To use these arrays in your programs, you must:

  (1) Declare variables of the appropriate type, e.g.

         var
           V : TVector;
           A : TMatrix;

  (2) Allocate each array BEFORE using it:

         DimVector(V, N);         creates vector V[0..N]
         DimMatrix(A, N, M);      creates matrix A[0..N, 0..M]
                                  where N, M are two integer variables

         If the allocation succeeds, all array elements are initialized
         to zero (for numeric arrays), False (for boolean arrays), or
         the null string (for string arrays). Otherwise, the array is
         initialized to NIL.

         The Dim... procedure may be used again to redimension the array.

  (3) Use arrays as in standard Pascal, noting that:

      (a) You cannot use the assignment operator (:=) to copy the
          contents of an array into another array. Writing B := A
          simply makes B point to the same memory block than A. You
          must use one of the provided Copy... procedures (see their
          documentation in the interface part of the unit).

      (b) All arrays begin at index 0, so that the 0-indexed element
          is always present, even if you don't use it.

      (c) A matrix is declared as an array of vectors, so that A[I]
          denotes the I-th vector of matrix A and may be used as any
          vector.

  (4) To deallocate an array, assign the value NIL

         V := nil;

  For more information, read the comments of each routine in the
  interface part of the unit, and check the demo programs.
  **********************************************************************
  References :
  1) 'Mathematiques et Statistiques' by H. Haut (PSI ed.) : GaussJordan
     and related functions
  2) EISPACK (http://www.netlib.org/eispack) : SV_Decomp
  3) 'Matrix Computations' by Golub & Van Loan : QR_Decomp & QR_Solve
     (Pascal implementation contributed by Mark Vaughan)
  ********************************************************************** }

unit matrices;

interface

uses
  fmath;

{ **********************************************************************
  This section defines some error codes.
  ********************************************************************** }

const
  MAT_OK       =   0;  { No error }
  MAT_SINGUL   = - 1;  { Singular matrix }
  MAT_NON_CONV = - 2;  { Non convergence of iterative procedure }
  MAT_NOT_PD   = - 3;  { Matrix not positive definite }

{ **********************************************************************
  This section defines the vector and matrix types.
  ********************************************************************** }

type
  TVector     = array of Float;
  TIntVector  = array of Integer;
  TCompVector = array of Complex;
  TBoolVector = array of Boolean;
  TStrVector  = array of String;

  TMatrix     = array of TVector;
  TIntMatrix  = array of TIntVector;
  TCompMatrix = array of TCompVector;
  TBoolMatrix = array of TBoolVector;
  TStrMatrix  = array of TStrVector;

{ **********************************************************************
  This section defines a function of several variables
  ********************************************************************** }

type
  TFuncNVar = function(X : TVector) : Float;

{ **********************************************************************
  Memory allocation routines
  ********************************************************************** }

procedure DimVector(var V : TVector; Ubound : Integer); overload;
{ ----------------------------------------------------------------------
  Creates floating point vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimVector(var V : TIntVector; Ubound : Integer); overload;
{ ----------------------------------------------------------------------
  Creates integer vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimVector(var V : TCompVector; Ubound : Integer); overload;
{ ----------------------------------------------------------------------
  Creates complex vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimVector(var V : TBoolVector; Ubound : Integer); overload;
{ ----------------------------------------------------------------------
  Creates boolean vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimVector(var V : TStrVector; Ubound : Integer); overload;
{ ----------------------------------------------------------------------
  Creates string vector V[0..Ubound]
  ---------------------------------------------------------------------- }

procedure DimMatrix(var A : TMatrix; Ubound1, Ubound2 : Integer); overload;
{ ----------------------------------------------------------------------
  Creates floating point matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimMatrix(var A : TIntMatrix; Ubound1, Ubound2 : Integer); overload;
{ ----------------------------------------------------------------------
  Creates integer matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimMatrix(var A : TCompMatrix; Ubound1, Ubound2 : Integer); overload;
{ ----------------------------------------------------------------------
  Creates complex matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimMatrix(var A : TBoolMatrix; Ubound1, Ubound2 : Integer); overload;
{ ----------------------------------------------------------------------
  Creates boolean matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

procedure DimMatrix(var A : TStrMatrix; Ubound1, Ubound2 : Integer); overload;
{ ----------------------------------------------------------------------
  Creates string matrix A[0..Ubound1, 0..Ubound2]
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Routines for copying vectors and matrices
  ----------------------------------------------------------------------
  Lbound, Ubound   : indices of first and last vector elements
  Lbound1, Lbound2 : indices of first matrix element in each dimension
  Ubound1, Ubound2 : indices of last matrix element in each dimension
  ********************************************************************** }

procedure SwapRows(I, K : Integer; A : TMatrix; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Exchanges rows I and K of matrix A
  ---------------------------------------------------------------------- }

procedure SwapCols(J, K : Integer; A : TMatrix; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Exchanges columns J and K of matrix A
  ---------------------------------------------------------------------- }

procedure CopyVector(Dest, Source : TVector; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Copies vector Source into vector Dest
  ---------------------------------------------------------------------- }

procedure CopyMatrix(Dest, Source : TMatrix;
                     Lbound1, Lbound2, Ubound1, Ubound2 : Integer);
{ ----------------------------------------------------------------------
  Copies matrix Source into matrix Dest
  ---------------------------------------------------------------------- }

procedure CopyRowFromVector(Dest : TMatrix; Source : TVector;
                            Lbound, Ubound, Row : Integer);
{ ----------------------------------------------------------------------
  Copies vector Source into line Row of matrix Dest
  ---------------------------------------------------------------------- }

procedure CopyColFromVector(Dest : TMatrix; Source : TVector;
                            Lbound, Ubound, Col : Integer);
{ ----------------------------------------------------------------------
  Copies vector Source into column Col of matrix Dest
  ---------------------------------------------------------------------- }

procedure CopyVectorFromRow(Dest : TVector; Source : TMatrix;
                            Lbound, Ubound, Row : Integer);
{ ----------------------------------------------------------------------
  Copies line Row of matrix Source into vector Dest
  ---------------------------------------------------------------------- }

procedure CopyVectorFromCol(Dest : TVector; Source : TMatrix;
                            Lbound, Ubound, Col : Integer);
{ ----------------------------------------------------------------------
  Copies column Col of matrix Source into vector Dest
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Vector and matrix functions
  ********************************************************************** }

function Min(X : TVector; Lbound, Ubound : Integer) : Float; overload;
{ ----------------------------------------------------------------------
  Returns the lowest value of vector X
  ---------------------------------------------------------------------- }

function Min(X : TIntVector; Lbound, Ubound : Integer) : Integer; overload;
{ ----------------------------------------------------------------------
  Returns the lowest value of integer vector X
  ---------------------------------------------------------------------- }

function Max(X : TVector; Lbound, Ubound : Integer) : Float; overload;
{ ----------------------------------------------------------------------
  Returns the highest value of vector X
  ---------------------------------------------------------------------- }

function Max(X : TIntVector; Lbound, Ubound : Integer) : Integer; overload;
{ ----------------------------------------------------------------------
  Returns the highest value of integer vector X
  ---------------------------------------------------------------------- }

procedure Transpose(A : TMatrix; Lbound1, Lbound2, Ubound1, Ubound2 : Integer;
                    A_t : TMatrix); overload;
{ ----------------------------------------------------------------------
  Transposes a real matrix
  ----------------------------------------------------------------------
  Input parameters : A       = original matrix
                     Lbound1,
                     Lbound2 = indices of 1st matrix elem. in each dim.
                     Ubound1,
                     Ubound2 = indices of last matrix elem. in each dim.
  ----------------------------------------------------------------------
  Output parameter : A_t     = transposed matrix
  ---------------------------------------------------------------------- }

procedure Transpose(A : TIntMatrix;
                    Lbound1, Lbound2, Ubound1, Ubound2 : Integer;
                    A_t : TIntMatrix); overload;
{ ----------------------------------------------------------------------
  Transposes an integer matrix
  ---------------------------------------------------------------------- }

function GaussJordan(A              : TMatrix;
                     B              : TVector;
                     Lbound, Ubound : Integer;
                     A_inv          : TMatrix;
                     X              : TVector;
                     var Det        : Float) : Integer; overload;
{ ----------------------------------------------------------------------
  Solves the system A * X = B (where B and X are vectors)
  by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Input parameters  : A      = system matrix
                      B      = constant vector
                      Lbound = index of first matrix element
                      Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameters : A_inv  = inverse matrix
                      X      = solution vector
                      Det    = determinant of A
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_SINGUL
  ---------------------------------------------------------------------- }

function GaussJordan(A, B                     : TMatrix;
                     Lbound, Ubound1, Ubound2 : Integer;
                     A_inv, X                 : TMatrix;
                     var Det                  : Float) : Integer; overload;
{ ----------------------------------------------------------------------
  Solves the system A * X = B (where B and X are matrices)
  by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Input parameters  : A       = system matrix
                      B       = constant matrix
                      Lbound  = index of first matrix element in A and B
                      Ubound1 = index of last matrix element in A
                      Ubound2 = index of last matrix element in B
  ----------------------------------------------------------------------
  Output parameters : A_inv  = inverse matrix
                      X      = solution matrix
                      Det    = determinant of A
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_SINGUL
  ---------------------------------------------------------------------- }

function InvMat(A              : TMatrix;
                Lbound, Ubound : Integer;
                A_inv          : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Computes the inverse of a square matrix by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Parameters : as in Gauss-Jordan
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SINGUL
  ---------------------------------------------------------------------- }

function InvDet(A              : TMatrix;
                Lbound, Ubound : Integer;
                A_inv          : TMatrix;
                var Det        : Float) : Integer;
{ ----------------------------------------------------------------------
  Computes the inverse and the determinant of a square matrix
  by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Parameters : as in Gauss-Jordan
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SINGUL
  ---------------------------------------------------------------------- }

function Det(A              : TMatrix;
             Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Computes the determinant of a square matrix by the Gauss-Jordan method
  ----------------------------------------------------------------------
  Parameters : as in Gauss-Jordan
  ----------------------------------------------------------------------
  Notes : (1) This procedure destroys the original matrix A
          (2) If the matrix is quasi-singular, the value 0 is returned
  ---------------------------------------------------------------------- }

function Cholesky(A : TMatrix; Lbound, Ubound : Integer;
                  L : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Cholesky decomposition. Factors the symmetric positive definite matrix
  A as a product L * L', where L is a lower triangular matrix. This
  procedure may be used as a test of positive definiteness.
  ----------------------------------------------------------------------
  Input parameters : A      = matrix
                     Lbound = index of first matrix element
                     Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameter : L      = Cholesky factor of matrix A
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_NOT_PD
  ---------------------------------------------------------------------- }

 function LU_Decomp(A : TMatrix; Lbound, Ubound : Integer) : Integer; overload;
{ ----------------------------------------------------------------------
  LU decomposition. Factors the square matrix A as a product L * U,
  where L is a lower triangular matrix (with unit diagonal terms) and U
  is an upper triangular matrix. This routine is used in conjunction
  with LU_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A      = matrix
                     Lbound = index of first matrix element
                     Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameter : A      = contains the elements of L and U
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SINGUL
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure LU_Solve(A : TMatrix; B : TVector; Lbound, Ubound : Integer;
                   X : TVector); overload;
{ ----------------------------------------------------------------------
  Solves a system of equations whose matrix has been transformed by
  LU_Decomp
  ----------------------------------------------------------------------
  Input parameters : A      = result from LU_Decomp
                     B      = constant vector
                     Lbound,
                     Ubound = as in LU_Decomp
  ----------------------------------------------------------------------
  Output parameter : X      = solution vector
  ---------------------------------------------------------------------- }

function LU_Decomp(A : TCompMatrix; Lbound, Ubound : Integer) : Integer; overload;
{ ----------------------------------------------------------------------
  LU decomposition for complex matrices
  ---------------------------------------------------------------------- }

procedure LU_Solve(A : TCompMatrix; B : TCompVector;
                   Lbound, Ubound : Integer; X : TCompVector); overload;
{ ----------------------------------------------------------------------
  LU solution for complex matrices
  ---------------------------------------------------------------------- }

function SV_Decomp(A : TMatrix; Lbound, Ubound1, Ubound2 : Integer;
                   S : TVector; V : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Singular value decomposition. Factors the matrix A (n x m, with n >= m)
  as a product U * S * V' where U is a (n x m) column-orthogonal matrix,
  S a (m x m) diagonal matrix with elements >= 0 (the singular values)
  and V a (m x m) orthogonal matrix. This routine is used in conjunction
  with SV_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A       = matrix
                     Lbound  = index of first matrix element
                     Ubound1 = index of last matrix element in 1st dim.
                     Ubound2 = index of last matrix element in 2nd dim.
  ----------------------------------------------------------------------
  Output parameter : A       = contains the elements of U
                     S       = vector of singular values
                     V       = orthogonal matrix
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_NON_CONV
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure SV_SetZero(S : TVector; Lbound, Ubound : Integer; Tol : Float);
{ ----------------------------------------------------------------------
  Sets the singular values to zero if they are lower than a specified
  threshold.
  ----------------------------------------------------------------------
  Input parameters : S      = vector of singular values
                     Tol    = relative tolerance
                              Threshold value will be Tol * Max(S)
                     Lbound = index of first vector element
                     Ubound = index of last vector element
  ----------------------------------------------------------------------
  Output parameter : S      = modified singular values
  ---------------------------------------------------------------------- }

procedure SV_Solve(U : TMatrix; S : TVector; V : TMatrix; B : TVector;
                   Lbound, Ubound1, Ubound2 : Integer;
                   X : TVector);
{ ----------------------------------------------------------------------
  Solves a system of equations by singular value decomposition, after
  the matrix has been transformed by SV_Decomp, and the lowest singular
  values have been set to zero by SV_SetZero.
  ----------------------------------------------------------------------
  Input parameters : U, S, V = vector and matrices from SV_Decomp
                     B       = constant vector
                     Lbound,
                     Ubound1,
                     Ubound2 = as in SV_Decomp
  ----------------------------------------------------------------------
  Output parameter : X       = solution vector
                             = V * Diag(1/s(i)) * U' * B, for s(i) <> 0
  ---------------------------------------------------------------------- }

procedure SV_Approx(U : TMatrix; S : TVector; V : TMatrix;
                    Lbound, Ubound1, Ubound2 : Integer;
                    A : TMatrix);
{ ----------------------------------------------------------------------
  Approximates a matrix A by the product USV', after the lowest singular
  values have been set to zero by SV_SetZero.
  ----------------------------------------------------------------------
  Input parameters : U, S, V = vector and matrices from SV_Decomp
                     Lbound,
                     Ubound1,
                     Ubound2 = as in SV_Decomp
  ----------------------------------------------------------------------
  Output parameter : A       = approximated matrix
  ---------------------------------------------------------------------- }

function QR_Decomp(A : TMatrix; Lbound, Ubound1, Ubound2 : Integer;
                   R : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  QR decomposition. Factors the matrix A (n x m, with n >= m) as a
  product Q * R where Q is a (n x m) column-orthogonal matrix, and R
  a (m x m) upper triangular matrix. This routine is used in conjunction
  with QR_Solve to solve a system of equations.
  ----------------------------------------------------------------------
  Input parameters : A       = matrix
                     Lbound  = index of first matrix element
                     Ubound1 = index of last matrix element in 1st dim.
                     Ubound2 = index of last matrix element in 2nd dim.
  ----------------------------------------------------------------------
  Output parameter : A       = contains the elements of Q
                     R       = upper triangular matrix
  ----------------------------------------------------------------------
  Possible results : MAT_OK
                     MAT_SING
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure QR_Solve(Q, R : TMatrix; B : TVector;
                   Lbound, Ubound1, Ubound2 : Integer;
                   X : TVector);
{ ----------------------------------------------------------------------
  Solves a system of equations by the QR decomposition,
  after the matrix has been transformed by QR_Decomp.
  ----------------------------------------------------------------------
  Input parameters : Q, R    = matrices from QR_Decomp
                     B       = constant vector
                     Lbound,
                     Ubound1,
                     Ubound2 = as in QR_Decomp
  ----------------------------------------------------------------------
  Output parameter : X       = solution vector
  ---------------------------------------------------------------------- }

implementation

  procedure DimVector(var V : TVector; Ubound : Integer); overload;
  var
    I : Integer;
  begin
    { Check bounds }
    if Ubound < 0 then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    SetLength(V, Succ(Ubound));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V[I] := 0.0;
  end;

  procedure DimVector(var V : TIntVector; Ubound : Integer); overload;
  var
    I : Integer;
  begin
    { Check bounds }
    if Ubound < 0 then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    SetLength(V, Succ(Ubound));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V[I] := 0;
  end;

  procedure DimVector(var V : TCompVector; Ubound : Integer); overload;
  var
    I : Integer;
  begin
    { Check bounds }
    if Ubound < 0 then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    SetLength(V, Succ(Ubound));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V[I] := C_zero;
  end;

  procedure DimVector(var V : TBoolVector; Ubound : Integer); overload;
  var
    I : Integer;
  begin
    { Check bounds }
    if Ubound < 0 then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    SetLength(V, Succ(Ubound));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V[I] := False;
  end;

  procedure DimVector(var V : TStrVector; Ubound : Integer); overload;
  var
    I : Integer;
  begin
    { Check bounds }
    if Ubound < 0 then
      begin
        V := nil;
        Exit;
      end;

    { Allocate vector }
    SetLength(V, Succ(Ubound));
    if V = nil then Exit;

    { Initialize vector }
    for I := 0 to Ubound do
      V[I] := '';
  end;

  procedure DimMatrix(var A : TMatrix; Ubound1, Ubound2 : Integer); overload;
  var
    I, J : Integer;
  begin
    if (Ubound1 < 0) or (Ubound2 < 0) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    SetLength(A, Succ(Ubound1), Succ(Ubound2));
    if A = nil then Exit;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A[I,J] := 0.0;
  end;

  procedure DimMatrix(var A : TIntMatrix; Ubound1, Ubound2 : Integer); overload;
  var
    I, J : Integer;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound2 < 0) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    SetLength(A, Succ(Ubound1), Succ(Ubound2));
    if A = nil then Exit;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A[I,J] := 0;
  end;

  procedure DimMatrix(var A : TCompMatrix; Ubound1, Ubound2 : Integer); overload;
  var
    I, J : Integer;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound2 < 0) then
         begin
           A := nil;
           Exit;
         end;

    { Allocate matrix }
    SetLength(A, Succ(Ubound1), Succ(Ubound2));
    if A = nil then Exit;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A[I,J] := C_zero;
  end;

  procedure DimMatrix(var A : TBoolMatrix; Ubound1, Ubound2 : Integer); overload;
  var
    I, J : Integer;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound2 < 0) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    SetLength(A, Succ(Ubound1), Succ(Ubound2));
    if A = nil then Exit;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A[I,J] := False;
  end;

  procedure DimMatrix(var A : TStrMatrix; Ubound1, Ubound2 : Integer); overload;
  var
    I, J : Integer;
  begin
    { Check bounds }
    if (Ubound1 < 0) or (Ubound2 < 0) then
      begin
        A := nil;
        Exit;
      end;

    { Allocate matrix }
    SetLength(A, Succ(Ubound1), Succ(Ubound2));
    if A = nil then Exit;

    { Initialize matrix }
    for I := 0 to Ubound1 do
      for J := 0 to Ubound2 do
        A[I,J] := '';
  end;

  procedure SwapRows(I, K : Integer; A : TMatrix; Lbound, Ubound : Integer);
  var
    J : Integer;
  begin
    for J := Lbound to Ubound do
      Swap(A[I,J], A[K,J]);
  end;

  procedure SwapCols(J, K : Integer; A : TMatrix; Lbound, Ubound : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Swap(A[I,J], A[I,K]);
  end;

  procedure CopyVector(Dest, Source : TVector; Lbound, Ubound : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Dest[I] := Source[I];
  end;

  procedure CopyMatrix(Dest, Source : TMatrix;
                       Lbound1, Lbound2, Ubound1, Ubound2 : Integer);
  var
    I, J : Integer;
  begin
    for I := Lbound1 to Ubound1 do
      for J := Lbound2 to Ubound2 do
        Dest[I,J] := Source[I,J];
  end;

  procedure CopyRowFromVector(Dest : TMatrix; Source : TVector;
                              Lbound, Ubound, Row : Integer);
  var
    J : Integer;
  begin
    for J := Lbound to Ubound do
      Dest[Row,J] := Source[J];
  end;

  procedure CopyColFromVector(Dest : TMatrix; Source : TVector;
                              Lbound, Ubound, Col : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Dest[I,Col] := Source[I];
  end;

  procedure CopyVectorFromRow(Dest : TVector; Source : TMatrix;
                              Lbound, Ubound, Row : Integer);
  var
    J : Integer;
  begin
    for J := Lbound to Ubound do
      Dest[J] := Source[Row,J];
  end;

  procedure CopyVectorFromCol(Dest : TVector; Source : TMatrix;
                              Lbound, Ubound, Col : Integer);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      Dest[I] := Source[I,Col];
  end;

  function Min(X : TVector; Lbound, Ubound : Integer) : Float; overload;
  var
    Xmin : Float;
    I    : Integer;
  begin
    Xmin := X[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X[I] < Xmin then Xmin := X[I];
    Min := Xmin;
  end;

  function Max(X : TVector; Lbound, Ubound : Integer) : Float; overload;
  var
    Xmax : Float;
    I    : Integer;
  begin
    Xmax := X[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X[I] > Xmax then Xmax := X[I];
    Max := Xmax;
  end;

  function Min(X : TIntVector; Lbound, Ubound : Integer) : Integer; overload;
  var
    I, Xmin : Integer;
  begin
    Xmin := X[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X[I] < Xmin then Xmin := X[I];
    Min := Xmin;
  end;

  function Max(X : TIntVector; Lbound, Ubound : Integer) : Integer; overload;
  var
    I, Xmax : Integer;
  begin
    Xmax := X[Lbound];
    for I := Succ(Lbound) to Ubound do
      if X[I] > Xmax then Xmax := X[I];
    Max := Xmax;
  end;

  procedure Transpose(A : TIntMatrix;
                      Lbound1, Lbound2, Ubound1, Ubound2 : Integer;
                      A_t : TIntMatrix); overload;
  var
    I, J : Integer;
  begin
    for I := Lbound1 to Ubound1 do
      for J := Lbound2 to Ubound2 do
        A_t[J,I] := A[I,J];
  end;

  procedure Transpose(A : TMatrix;
                      Lbound1, Lbound2, Ubound1, Ubound2 : Integer;
                      A_t : TMatrix); overload;
  var
    I, J : Integer;
  begin
    for I := Lbound1 to Ubound1 do
      for J := Lbound2 to Ubound2 do
        A_t[J,I] := A[I,J];
  end;

  function GaussJordan(A              : TMatrix;
                       B              : TVector;
                       Lbound, Ubound : Integer;
                       A_inv          : TMatrix;
                       X              : TVector;
                       var Det        : Float) : Integer; overload;
  var
    I, J, K    : Integer;     { Loop variables }
    Ik, Jk     : Integer;     { Pivot coordinates }
    Pvt        : Float;       { Pivot }
    T          : Float;       { Auxiliary variable }
    PRow, PCol : TIntVector;  { Store line and column of pivot }
    MCol       : TVector;     { Stores a column of the matrix }
  begin
    DimVector(PRow, Ubound);
    DimVector(PCol, Ubound);
    DimVector(MCol, Ubound);

    { Copy A into A_inv and B into X }
    CopyMatrix(A_inv, A, Lbound, Lbound, Ubound, Ubound);
    CopyVector(X, B, Lbound, Ubound);

    Det := 1.0;
    K := Lbound;
    while K <= Ubound do
      begin
        { Search for largest pivot in submatrix A_inv[K..Ubound, K..Ubound] }
        Pvt := A_inv[K,K];
        Ik := K;
        Jk := K;
        for I := K to Ubound do
          for J := K to Ubound do
            if Abs(A_inv[I,J]) > Abs(Pvt) then
              begin
                Pvt := A_inv[I,J];
                Ik := I;
                Jk := J;
              end;

        { Pivot too small ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            GaussJordan := MAT_SINGUL;
            Exit;
          end;

        { Save pivot position }
        PRow[K] := Ik;
        PCol[K] := Jk;

        { Update determinant }
        Det := Det * Pvt;
        if Ik <> K then Det := - Det;
        if Jk <> K then Det := - Det;

        { Exchange current row (K) with pivot row (Ik) }
        if Ik <> K then
          begin
            SwapRows(Ik, K, A_inv, Lbound, Ubound);
            Swap(X[Ik], X[K]);
          end;

        { Exchange current column (K) with pivot column (Jk) }
        if Jk <> K then
          SwapCols(Jk, K, A_inv, Lbound, Ubound);

        { Store col. K of A_inv into MCol and set this col. to 0 }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              MCol[I] := A_inv[I, K];
              A_inv[I, K] := 0.0;
            end
          else
            begin
              MCol[I] := 0.0;
              A_inv[I, K] := 1.0;
            end;

        { Transform pivot row }
        for J := Lbound to Ubound do
          A_inv[K,J] := A_inv[K,J] / Pvt;
        X[K] := X[K] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              T := MCol[I];
              for J := Lbound to Ubound do
                A_inv[I,J] := A_inv[I,J] - T * A_inv[K,J];
              X[I] := X[I] - T * X[K];
            end;
        Inc(K);
      end;

    { Exchange rows of inverse matrix and solution vector }
    for I := Ubound downto Lbound do
      begin
        Ik := PCol[I];
        if Ik <> I then
          begin
            SwapRows(Ik, I, A_inv, Lbound, Ubound);
            Swap(X[Ik], X[I]);
          end;
      end;

    { Exchange columns of inverse matrix }
    for J := Ubound downto Lbound do
      begin
        Jk := PRow[J];
        if Jk <> J then
          SwapCols(Jk, J, A_inv, Lbound, Ubound);
      end;

    GaussJordan := MAT_OK;
  end;

  function GaussJordan(A, B                     : TMatrix;
                       Lbound, Ubound1, Ubound2 : Integer;
                       A_inv, X                 : TMatrix;
                       var Det                  : Float) : Integer; overload;
  var
    I, J, K    : Integer;     { Loop variables }
    Ik, Jk     : Integer;     { Pivot coordinates }
    Pvt        : Float;       { Pivot }
    T          : Float;       { Auxiliary variable }
    PRow, PCol : TIntVector;  { Store line and column of pivot }
    MCol       : TVector;     { Stores a column of the matrix }
  begin
    DimVector(PRow, Ubound1);
    DimVector(PCol, Ubound1);
    DimVector(MCol, Ubound1);

    { Copy A into A_inv and B into X }
    CopyMatrix(A_inv, A, Lbound, Lbound, Ubound1, Ubound1);
    CopyMatrix(X, B, Lbound, Lbound, Ubound1, Ubound2);

    Det := 1.0;
    K := Lbound;
    while K <= Ubound1 do
      begin
        { Search for largest pivot in submatrix A_inv[K..Ubound1, K..Ubound1] }
        Pvt := A_inv[K,K];
        Ik := K;
        Jk := K;
        for I := K to Ubound1 do
          for J := K to Ubound1 do
            if Abs(A_inv[I,J]) > Abs(Pvt) then
              begin
                Pvt := A_inv[I,J];
                Ik := I;
                Jk := J;
              end;

        { Pivot too small ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            GaussJordan := MAT_SINGUL;
            Exit;
          end;

        { Save pivot position }
        PRow[K] := Ik;
        PCol[K] := Jk;

        { Update determinant }
        Det := Det * Pvt;
        if Ik <> K then Det := - Det;
        if Jk <> K then Det := - Det;

        { Exchange current row (K) with pivot row (Ik) }
        if Ik <> K then
          begin
            SwapRows(Ik, K, A_inv, Lbound, Ubound1);
            SwapRows(Ik, K, X, Lbound, Ubound2);
          end;

        { Exchange current column (K) with pivot column (Jk) }
        if Jk <> K then
          SwapCols(Jk, K, A_inv, Lbound, Ubound1);

        { Store col. K of A_inv into MCol and set this col. to 0 }
        for I := Lbound to Ubound1 do
          if I <> K then
            begin
              MCol[I] := A_inv[I, K];
              A_inv[I, K] := 0.0;
            end
          else
            begin
              MCol[I] := 0.0;
              A_inv[I, K] := 1.0;
            end;

        { Transform pivot row }
        for J := Lbound to Ubound1 do
          A_inv[K,J] := A_inv[K,J] / Pvt;
        for J := Lbound to Ubound2 do
          X[K,J] := X[K,J] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound1 do
          if I <> K then
            begin
              T := MCol[I];
              for J := Lbound to Ubound1 do
                A_inv[I,J] := A_inv[I,J] - T * A_inv[K,J];
              for J := Lbound to Ubound2 do
                X[I,J] := X[I,J] - T * X[K,J];
            end;

        Inc(K);
      end;

    { Exchange lines of inverse and solution matrices }
    for I := Ubound1 downto Lbound do
      begin
        Ik := PCol[I];
        if Ik <> I then
          begin
            SwapRows(Ik, I, A_inv, Lbound, Ubound1);
            SwapRows(Ik, I, X, Lbound, Ubound2);
          end;
      end;

    { Exchange columns of inverse matrix }
    for J := Ubound1 downto Lbound do
      begin
        Jk := PRow[J];
        if Jk <> J then
          SwapCols(Jk, J, A_inv, Lbound, Ubound1);
      end;

    GaussJordan := MAT_OK;
  end;

  function InvMat(A              : TMatrix;
                  Lbound, Ubound : Integer;
                  A_inv          : TMatrix) : Integer;
  var
    I, J, K    : Integer;     { Loop variables }
    Ik, Jk     : Integer;     { Pivot coordinates }
    Pvt        : Float;       { Pivot }
    T          : Float;       { Auxiliary variable }
    PRow, PCol : TIntVector;  { Store line and column of pivot }
    MCol       : TVector;     { Stores a column of the matrix }
  begin
    DimVector(PRow, Ubound);
    DimVector(PCol, Ubound);
    DimVector(MCol, Ubound);

    { Copy A into A_inv }
    CopyMatrix(A_inv, A, Lbound, Lbound, Ubound, Ubound);

    K := Lbound;
    while K <= Ubound do
      begin
        { Search for largest pivot in submatrix A_inv[K..Ubound, K..Ubound] }
        Pvt := A_inv[K,K];
        Ik := K;
        Jk := K;
        for I := K to Ubound do
          for J := K to Ubound do
            if Abs(A_inv[I,J]) > Abs(Pvt) then
              begin
                Pvt := A_inv[I,J];
                Ik := I;
                Jk := J;
              end;

        { Pivot too small ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            InvMat := MAT_SINGUL;
            Exit;
          end;

        { Save pivot position }
        PRow[K] := Ik;
        PCol[K] := Jk;

        { Exchange current row (K) with pivot row (Ik) }
        if Ik <> K then
          SwapRows(Ik, K, A_inv, Lbound, Ubound);

        { Exchange current column (K) with pivot column (Jk) }
        if Jk <> K then
          SwapCols(Jk, K, A_inv, Lbound, Ubound);

        { Store col. K of A_inv into MCol and set this col. to 0 }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              MCol[I] := A_inv[I, K];
              A_inv[I, K] := 0.0;
            end
          else
            begin
              MCol[I] := 0.0;
              A_inv[I, K] := 1.0;
            end;

        { Transform pivot row }
        for J := Lbound to Ubound do
          A_inv[K,J] := A_inv[K,J] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              T := MCol[I];
              for J := Lbound to Ubound do
                A_inv[I,J] := A_inv[I,J] - T * A_inv[K,J];
            end;
        Inc(K);
      end;

    { Exchange lines of inverse matrix }
    for I := Ubound downto Lbound do
      begin
        Ik := PCol[I];
        if Ik <> I then
          SwapRows(Ik, I, A_inv, Lbound, Ubound);
      end;

    { Exchange columns of inverse matrix }
    for J := Ubound downto Lbound do
      begin
        Jk := PRow[J];
        if Jk <> J then
          SwapCols(Jk, J, A_inv, Lbound, Ubound);
      end;

    InvMat := MAT_OK;
  end;

  function InvDet(A              : TMatrix;
                  Lbound, Ubound : Integer;
                  A_inv          : TMatrix;
                  var Det        : Float) : Integer;
  var
    I, J, K    : Integer;     { Loop variables }
    Ik, Jk     : Integer;     { Pivot coordinates }
    Pvt        : Float;       { Pivot }
    T          : Float;       { Auxiliary variable }
    PRow, PCol : TIntVector;  { Store line and column of pivot }
    MCol       : TVector;     { Stores a column of the matrix }
  begin
    DimVector(PRow, Ubound);
    DimVector(PCol, Ubound);
    DimVector(MCol, Ubound);

    { Copy A into A_inv }
    CopyMatrix(A_inv, A, Lbound, Lbound, Ubound, Ubound);

    Det := 1.0;
    K := Lbound;
    while K <= Ubound do
      begin
        { Search for largest pivot in submatrix A_inv[K..Ubound, K..Ubound] }
        Pvt := A_inv[K,K];
        Ik := K;
        Jk := K;
        for I := K to Ubound do
          for J := K to Ubound do
            if Abs(A_inv[I,J]) > Abs(Pvt) then
              begin
                Pvt := A_inv[I,J];
                Ik := I;
                Jk := J;
              end;

        { Pivot too small ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            InvDet := MAT_SINGUL;
            Exit;
          end;

        { Save pivot position }
        PRow[K] := Ik;
        PCol[K] := Jk;

        { Update determinant }
        Det := Det * Pvt;
        if Ik <> K then Det := - Det;
        if Jk <> K then Det := - Det;

        { Exchange current row (K) with pivot row (Ik) }
        if Ik <> K then
          SwapRows(Ik, K, A_inv, Lbound, Ubound);

        { Exchange current column (K) with pivot column (Jk) }
        if Jk <> K then
          SwapCols(Jk, K, A_inv, Lbound, Ubound);

        { Store col. K of A_inv into MCol and set this col. to 0 }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              MCol[I] := A_inv[I, K];
              A_inv[I, K] := 0.0;
            end
          else
            begin
              MCol[I] := 0.0;
              A_inv[I, K] := 1.0;
            end;

        { Transform pivot row }
        for J := Lbound to Ubound do
          A_inv[K,J] := A_inv[K,J] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              T := MCol[I];
              for J := Lbound to Ubound do
                A_inv[I,J] := A_inv[I,J] - T * A_inv[K,J];
            end;
        Inc(K);
      end;

    { Exchange lines of inverse matrix }
    for I := Ubound downto Lbound do
      begin
        Ik := PCol[I];
        if Ik <> I then
          SwapRows(Ik, I, A_inv, Lbound, Ubound);
      end;

    { Exchange columns of inverse matrix }
    for J := Ubound downto Lbound do
      begin
        Jk := PRow[J];
        if Jk <> J then
          SwapCols(Jk, J, A_inv, Lbound, Ubound);
      end;

    InvDet := MAT_OK;
  end;

  function Det(A : TMatrix; Lbound, Ubound : Integer) : Float;
  var
    I, J, K    : Integer;     { Loop variables }
    Ik, Jk     : Integer;     { Pivot coordinates }
    Pvt        : Float;       { Pivot }
    T          : Float;       { Auxiliary variable }
    PRow, PCol : TIntVector;  { Store line and column of pivot }
    MCol       : TVector;     { Stores a column of the matrix }
    D          : Float;       { Determinant }
  begin
    DimVector(PRow, Ubound);
    DimVector(PCol, Ubound);
    DimVector(MCol, Ubound);

    D := 1.0;
    K := Lbound;
    while K <= Ubound do
      begin
        { Search for largest pivot in submatrix A[K..Ubound, K..Ubound] }
        Pvt := A[K,K];
        Ik := K;
        Jk := K;
        for I := K to Ubound do
          for J := K to Ubound do
            if Abs(A[I,J]) > Abs(Pvt) then
              begin
                Pvt := A[I,J];
                Ik := I;
                Jk := J;
              end;

        { Pivot too small ==> quasi-singular matrix }
        if Abs(Pvt) < MACHEP then
          begin
            Det := 0.0;
            Exit;
          end;

        { Save pivot position }
        PRow[K] := Ik;
        PCol[K] := Jk;

        { Update determinant }
        D := D * Pvt;
        if Ik <> K then D := - D;
        if Jk <> K then D := - D;

        { Exchange current row (K) with pivot row (Ik) }
        if Ik <> K then
          SwapRows(Ik, K, A, Lbound, Ubound);

        { Exchange current column (K) with pivot column (Jk) }
        if Jk <> K then
          SwapCols(Jk, K, A, Lbound, Ubound);

        { Store col. K of A into MCol and set this col. to 0 }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              MCol[I] := A[I, K];
              A[I, K] := 0.0;
            end
          else
            begin
              MCol[I] := 0.0;
              A[I, K] := 1.0;
            end;

        { Transform pivot row }
        for J := Lbound to Ubound do
          A[K,J] := A[K,J] / Pvt;

        { Transform other rows }
        for I := Lbound to Ubound do
          if I <> K then
            begin
              T := MCol[I];
              for J := Lbound to Ubound do
                A[I,J] := A[I,J] - T * A[K,J];
            end;
        Inc(K);
      end;

    Det := D;
  end;

  function Cholesky(A : TMatrix; Lbound, Ubound : Integer;
                    L : TMatrix) : Integer;
  var
    I, J, K : Integer;
    Sum     : Float;
  begin
    for K := Lbound to Ubound do
      begin
        Sum := A[K,K];
        for J := Lbound to Pred(K) do
          Sum := Sum - Sqr(L[K,J]);

        if Sum <= 0.0 then
          begin
            Cholesky := MAT_NOT_PD;
            Exit;
          end;

        L[K,K] := Sqrt(Sum);
        for I := Succ(K) to Ubound do
          begin
            Sum := A[I,K];
            for J := Lbound to Pred(K) do
              Sum := Sum - L[I,J] * L[K,J];
            L[I,K] := Sum / L[K,K];
          end;
      end;
    Cholesky := MAT_OK;
  end;

var                    { Used by LU procedures }
  Index : TIntVector;  { Records the row permutations }

  function LU_Decomp(A : TMatrix; Lbound, Ubound : Integer) : Integer; overload;
  var
    I, Imax, J, K : Integer;
    Pvt, T, Sum   : Float;
    V             : TVector;
  begin
    DimVector(V, Ubound);
    DimVector(Index, Ubound);

    for I := Lbound to Ubound do
      begin
        Pvt := 0.0;
        for J := Lbound to Ubound do
          if Abs(A[I,J]) > Pvt then
            Pvt := Abs(A[I,J]);
        if Pvt < MACHEP then
          begin
            LU_Decomp := MAT_SINGUL;
            Exit;
          end;
        V[I] := 1.0 / Pvt;
      end;
    for J := Lbound to Ubound do
      begin
        for I := Lbound to Pred(J) do
          begin
            Sum := A[I,J];
            for K := Lbound to Pred(I) do
              Sum := Sum - A[I,K] * A[K,J];
            A[I,J] := Sum;
          end;
        Imax := 0;
        Pvt := 0.0;
        for I := J to Ubound do
          begin
            Sum := A[I,J];
            for K := Lbound to Pred(J) do
              Sum := Sum - A[I,K] * A[K,J];
            A[I,J] := Sum;
            T := V[I] * Abs(Sum);
            if T > Pvt then
              begin
                Pvt := T;
                Imax := I;
              end;
          end;
        if J <> Imax then
          begin
            SwapRows(Imax, J, A, Lbound, Ubound);
            V[Imax] := V[J];
          end;
        Index[J] := Imax;
        if A[J,J] = 0.0 then
          A[J,J] := MACHEP;
        if J <> Ubound then
          begin
            T := 1.0 / A[J,J];
            for I := Succ(J) to Ubound do
              A[I,J] := A[I,J] * T;
          end;
      end;
    LU_Decomp := MAT_OK;
  end;

  procedure LU_Solve(A : TMatrix; B : TVector; Lbound, Ubound : Integer;
                     X : TVector); overload;
  var
    I, Ip, J, K : Integer;
    Sum         : Float;
  begin
    K := Pred(Lbound);
    CopyVector(X, B, Lbound, Ubound);
    for I := Lbound to Ubound do
      begin
        Ip := Index[I];
        Sum := X[Ip];
        X[Ip] := X[I];
        if K >= Lbound then
          for J := K to Pred(I) do
            Sum := Sum - A[I,J] * X[J]
        else if Sum <> 0.0 then
          K := I;
        X[I] := Sum;
      end;
    for I := Ubound downto Lbound do
      begin
        Sum := X[I];
        if I < Ubound then
          for J := Succ(I) to Ubound do
            Sum := Sum - A[I,J] * X[J];
        X[I] := Sum / A[I,I];
      end;
  end;

  function LU_Decomp(A : TCompMatrix; Lbound, Ubound : Integer) : Integer; overload;
  var
    I, Imax, J, K : Integer;
    C, Pvt, T     : Float;
    Sum           : Complex;
    V             : TVector;
  begin
    DimVector(V, Ubound);
    DimVector(Index, Ubound);

    for I := Lbound to Ubound do
      begin
        Pvt := 0.0;
        for J := Lbound to Ubound do
          begin
            C := CAbs(A[I,J]);
            if C > Pvt then Pvt := C;
          end;
        if Pvt < MACHEP then
          begin
            LU_Decomp := MAT_SINGUL;
            Exit;
          end;
        V[I] := 1.0 / Pvt;
      end;
    for J := Lbound to Ubound do
      begin
        for I := Lbound to Pred(J) do
          begin
            Sum := A[I,J];
            for K := Lbound to Pred(I) do
              { Sum := Sum - A[I,K] * A[K,J]; }
              Sum := CSub(Sum, CMult(A[I,K], A[K,J]));
            A[I,J] := Sum;
          end;
        Imax := 0;
        Pvt := 0.0;
        for I := J to Ubound do
          begin
            Sum := A[I,J];
            for K := Lbound to Pred(J) do
              { Sum := Sum - A[I,K] * A[K,J]; }
              Sum := CSub(Sum, CMult(A[I,K], A[K,J]));
            A[I,J] := Sum;
            T := V[I] * CAbs(Sum);
            if T > Pvt then
              begin
                Pvt := T;
                Imax := I;
              end;
          end;
        if J <> Imax then
          begin
            { SwapRows(Imax, J, A, Lbound, Ubound); }
            for K := Lbound to Ubound do
              Swap(A[Imax,K], A[J,K]);
            V[Imax] := V[J];
          end;
        Index[J] := Imax;
        if CAbs(A[J,J]) = 0.0 then
          A[J,J] := Cmplx(MACHEP, MACHEP);
        if J <> Ubound then
          for I := Succ(J) to Ubound do
            { A[I,J] := A[I,J] / A[J,J]; }
            A[I,J] := CDiv(A[I,J], A[J,J]);
      end;
    LU_Decomp := MAT_OK;
  end;

  procedure LU_Solve(A : TCompMatrix; B : TCompVector;
                     Lbound, Ubound : Integer; X : TCompVector); overload;
  var
    I, Ip, J, K : Integer;
    Sum         : Complex;
  begin
    K := Pred(Lbound);
    { CopyVector(X, B, Lbound, Ubound); }
    for I := Lbound to Ubound do
      X[I] := B[I];
    for I := Lbound to Ubound do
      begin
        Ip := Index[I];
        Sum := X[Ip];
        X[Ip] := X[I];
        if K >= Lbound then
          for J := K to Pred(I) do
            { Sum := Sum - A[I,J] * X[J] }
            Sum := CSub(Sum, CMult(A[I,J], X[J]))
        else if CAbs(Sum) <> 0.0 then
          K := I;
        X[I] := Sum;
      end;
    for I := Ubound downto Lbound do
      begin
        Sum := X[I];
        if I < Ubound then
          for J := Succ(I) to Ubound do
            { Sum := Sum - A[I,J] * X[J]; }
            Sum := CSub(Sum, CMult(A[I,J], X[J]));
        { X[I] := Sum / A[I,I]; }
        X[I] := CDiv(Sum, A[I,I]);
      end;
  end;

  function SV_Decomp(A : TMatrix;
                     Lbound, Ubound1, Ubound2 : Integer;
                     S : TVector;
                     V : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  This function is a translation of the EISPACK subroutine SVD

  This function determines the singular value decomposition
  A = U.S.V' of a real M by N rectangular matrix. Householder
  bidiagonalization and a variant of the QR algorithm are used.
  ----------------------------------------------------------------------
  This is a crude translation. Many of the original goto's
  have been kept!
  ---------------------------------------------------------------------- }

  var
    I, J, K, L, I1, K1, L1, Mn, Its : Integer;
    C, F, G, H, T, X, Y, Z, Tst1, Tst2, Scale : Float;
    R : TVector;

  label
    190, 210, 270, 290, 360, 390, 410, 430, 460,
    475, 490, 510, 520, 540, 565, 580, 650, 700;

  begin
    DimVector(R, Ubound2);
    Scale := 0.0;
    G := 0.0;
    X := 0.0;

    { Householder reduction to bidiagonal form }
    for I := Lbound to Ubound2 do
      begin
        L := I + 1;
        R[I] := Scale * G;
        G := 0.0;
        T := 0.0;
        Scale := 0.0;
        if I > Ubound1 then goto 210;

        for K := I to Ubound1 do
          Scale := Scale + Abs(A[K, I]);

        if Scale = 0.0 then goto 210;

        for K := I to Ubound1 do
          begin
            A[K, I] := A[K, I] / Scale;
            T := T + Sqr(A[K, I]);
          end;

        F := A[I, I];
        G := - DSgn(Sqrt(T), F);
        H := F * G - T;
        A[I, I] := F - G;
        if I = Ubound2 then goto 190;

        for J := L to Ubound2 do
          begin
            T := 0.0;
            for K := I to Ubound1 do
              T := T + A[K, I] * A[K, J];
            F := T / H;
            for K := I to Ubound1 do
              A[K, J] := A[K, J] + F * A[K, I];
          end;

190:    for K := I to Ubound1 do
          A[K, I] := Scale * A[K, I];

210:    S[I] := Scale * G;
        G := 0.0;
        T := 0.0;
        Scale := 0.0;
        if (I > Ubound1) or (I = Ubound2) then goto 290;

        for K := L to Ubound2 do
          Scale := Scale + Abs(A[I, K]);

        if Scale = 0.0 then goto 290;

        for K := L to Ubound2 do
          begin
            A[I, K] := A[I, K] / Scale;
            T := T + Sqr(A[I, K]);
          end;

        F := A[I, L];
        G := - DSgn(Sqrt(T), F);
        H := F * G - T;
        A[I, L] := F - G;

        for K := L to Ubound2 do
          R[K] := A[I, K] / H;

        if I = Ubound1 then goto 270;

        for J := L to Ubound1 do
          begin
            T := 0.0;
            for K := L to Ubound2 do
              T := T + A[J, K] * A[I, K];
            for K := L to Ubound2 do
              A[J, K] := A[J, K] + T * R[K];
          end;

270:    for K := L to Ubound2 do
          A[I, K] := Scale * A[I, K];

290:    X := Max(X, Abs(S[I]) + Abs(R[I]));
      end;

    { Accumulation of right-hand transformations }
    for I := Ubound2 downto Lbound do
      begin
        if I = Ubound2 then goto 390;
        if G = 0.0 then goto 360;

        for J := L to Ubound2 do
          { Double division avoids possible underflow }
          V[J, I] := (A[I, J] / A[I, L]) / G;

        for J := L to Ubound2 do
          begin
            T := 0.0;
            for K := L to Ubound2 do
              T := T + A[I, K] * V[K, J];
            for K := L to Ubound2 do
              V[K, J] := V[K, J] + T * V[K, I];
          end;

360:    for J := L to Ubound2 do
          begin
            V[I, J] := 0.0;
            V[J, I] := 0.0;
          end;

390:    V[I, I] := 1.0;
        G := R[I];
        L := I;
      end;


410:{ Accumulation of left-hand transformations }
    Mn := Min(Ubound1, Ubound2);

    for I := Mn downto Lbound do
      begin
        L := I + 1;
        G := S[I];
        if I = Ubound2 then goto 430;

        for J := L to Ubound2 do
          A[I, J] := 0.0;

430:    if G = 0.0 then goto 475;
        if I = Mn then goto 460;

        for J := L to Ubound2 do
          begin
            T := 0.0;

            for K := L to Ubound1 do
              T := T + A[K, I] * A[K, J];

            { Double division avoids possible underflow }
            F := (T / A[I, I]) / G;

            for K := I to Ubound1 do
              A[K, J] := A[K, J] + F * A[K, I];
          end;

460:    for J := I to Ubound1 do
          A[J, I] := A[J, I] / G;

        goto 490;

475:    for J := I to Ubound1 do
          A[J, I] := 0.0;

490:    A[I, I] := A[I, I] + 1.0;
      end;

510:{ Diagonalization of the bidiagonal form }
    Tst1 := X;
    for K := Ubound2 downto Lbound do
      begin
        K1 := K - 1;
        Its := 0;

520:    { Test for splitting }
        for L := K downto Lbound do
          begin
            L1 := L - 1;
            Tst2 := Tst1 + Abs(R[L]);
            if Tst2 = Tst1 then goto 565;
            { R[Lbound] is always zero, so there is no exit
                through the bottom of the loop  }
            Tst2 := Tst1 + Abs(S[L1]);
            if Tst2 = Tst1 then goto 540;
          end;

540:    { Cancellation of R[L] if L greater than 1 }
        C := 0.0;
        T := 1.0;

        for I := L to K do
          begin
            F := T * R[I];
            R[I] := C * R[I];
            Tst2 := Tst1 + Abs(F);
            if Tst2 = Tst1 then goto 565;
            G := S[I];
            H := Pythag(F, G);
            S[I] := H;
            C := G / H;
            T := - F / H;

            for J := Lbound to Ubound1 do
              begin
                Y := A[J, L1];
                Z := A[J, I];
                A[J, L1] := Y * C + Z * T;
                A[J, I] := - Y * T + Z * C;
              end;
           end;

565:    { Test for convergence }
        Z := S[K];
        if L = K then goto 650;

        if Its = 30 then
          begin
            SV_Decomp := - K;
            Exit;
          end;

        { Shift from bottom 2 by 2 minor }
        Its := Its + 1;
        X := S[L];
        Y := S[K1];
        G := R[K1];
        H := R[K];
        F := 0.5 * (((G + Z) / H) * ((G - Z) / Y) + Y / H - H / Y);
        G := Pythag(F, 1.0);
        F := X - (Z / X) * Z + (H / X) * (Y / (F + DSgn(G, F)) - H);

        { Next QR transformation }
        C := 1.0;
        T := 1.0;

        for I1 := L to K1 do
          begin
            I := I1 + 1;
            G := R[I];
            Y := S[I];
            H := T * G;
            G := C * G;
            Z := Pythag(F, H);
            R[I1] := Z;
            C := F / Z;
            T := H / Z;
            F := X * C + G * T;
            G := - X * T + G * C;
            H := Y * T;
            Y := Y * C;

            for J := Lbound to Ubound2 do
              begin
                X := V[J, I1];
                Z := V[J, I];
                V[J, I1] := X * C + Z * T;
                V[J, I] := - X * T + Z * C;
              end;

            Z := Pythag(F, H);
            S[I1] := Z;
            { Rotation can be arbitrary if Z is zero }
            if Z = 0.0 then goto 580;
            C := F / Z;
            T := H / Z;
580:        F := C * G + T * Y;
            X := - T * G + C * Y;

            for J := Lbound to Ubound1 do
              begin
                Y := A[J, I1];
                Z := A[J, I];
                A[J, I1] := Y * C + Z * T;
                A[J, I] := - Y * T + Z * C;
              end;
           end;

        R[L] := 0.0;
        R[K] := F;
        S[K] := X;
        goto 520;

650:    { Convergence }
        if Z >= 0.0 then goto 700;

        { S[K] is made non-negative }
        S[K] := - Z;

        for J := Lbound to Ubound2 do
          V[J, K] := - V[J, K];
700:  end;

    SV_Decomp := 0;
  end;

  procedure SV_SetZero(S : TVector; Lbound, Ubound : Integer; Tol : Float);
  var
    Threshold : Float;
    I         : Integer;
  begin
    Threshold := Tol * Max(S, Lbound, Ubound);
    for I := Lbound to Ubound do
      if S[I] < Threshold then S[I] := 0.0;
  end;

  procedure SV_Solve(U : TMatrix; S : TVector; V : TMatrix; B : TVector;
                     Lbound, Ubound1, Ubound2 : Integer;
                     X : TVector);
  var
    I, J, JJ : Integer;
    Sum      : Float;
    Tmp      : TVector;
  begin
    DimVector(Tmp, Ubound2);
    for J := Lbound to Ubound2 do
      begin
        Sum := 0.0;
        if S[J] > 0.0 then
          begin
            for I := Lbound to Ubound1 do
              Sum := Sum + U[I,J] * B[I];
            Sum := Sum / S[J];
          end;
        Tmp[J] := Sum;
      end;
    for J := Lbound to Ubound2 do
      begin
        Sum := 0.0;
        for JJ := Lbound to Ubound2 do
          Sum := Sum + V[J,JJ] * Tmp[JJ];
        X[J] := Sum;
      end;
  end;

  procedure SV_Approx(U : TMatrix; S : TVector; V : TMatrix;
                      Lbound, Ubound1, Ubound2 : Integer; A : TMatrix);
  var
    I, J, K : Integer;
  begin
    for I := Lbound to Ubound1 do
      for J := Lbound to Ubound2 do
        begin
          A[I,J] := 0.0;
          for K := Lbound to Ubound2 do
            if S[K] > 0.0 then
              A[I,J] := A[I,J] + U[I,K] * V[J,K];
        end;
  end;

  function QR_Decomp(A : TMatrix; Lbound, Ubound1, Ubound2 : Integer;
                     R : TMatrix) : Integer;
  var
    I, J, K : Integer;
    Sum     : Float;
  begin
    for K := Lbound to Ubound2 do
      begin
        { Compute the "k"th diagonal entry in R }
        Sum := 0.0;
        for I := Lbound to Ubound1 do
          Sum := Sum + Sqr(A[I,K]);

        if Sum = 0.0 then
          begin
            QR_Decomp := MAT_SINGUL;
            Exit;
          end;

        R[K,K] := Sqrt(Sum);

        { Divide the entries in the "k"th column of A by the computed "k"th }
        { diagonal element of R.  this begins the process of overwriting A  }
        { with Q . . .                                                      }
        for I := Lbound to Ubound1 do
          A[I,K] := A[I,K] / R[K,K];

        for J := (K + 1) to Ubound2 do
          begin
            { Complete the remainder of the row entries in R }
            Sum := 0.0;
            for I := Lbound to Ubound1 do
              Sum := Sum + A[I,K] * A[I,J];
            R[K,J] := Sum;

            { Update the column entries of the Q/A matrix }
            for I := Lbound to Ubound1 do
              A[I,J] := A[I,J] - A[I,K] * R[K,J];
          end;
      end;

    QR_Decomp := MAT_OK;
  end;

  procedure QR_Solve(Q, R : TMatrix; B : TVector;
                     Lbound, Ubound1, Ubound2 : Integer;
                     X : TVector);
  var
    I, J : Integer;
    Sum  : Float;
  begin
    { Form Q'B and store the result in X }
    for J := Lbound to Ubound2 do
      begin
        X[J] := 0.0;
        for I := Lbound to Ubound1 do
          X[J] := X[J] + Q[I,J] * B[I];
      end;

    { Update X with the solution vector }
    X[Ubound2] := X[Ubound2] / R[Ubound2,Ubound2];
    for I := (Ubound2 - 1) downto Lbound do
      begin
        Sum := 0.0;
        for J := (I + 1) to Ubound2 do
          Sum := Sum + R[I,J] * X[J];
        X[I] := (X[I] - Sum) / R[I,I];
      end;
  end;

end.
