{ **********************************************************************
  *                          Unit REGRESS.PAS                          *
  *                            Version 2.5d                            *
  *                      (c) J. Debord, July 2004                      *
  **********************************************************************
                           Regression routines
  ********************************************************************** }

unit regress;

interface

uses
  fmath, fspec, matrices, randnum, eigen, optim, simopt, mcmc, stat;

{ **********************************************************************
  Type definitions
  ********************************************************************** }

{ Algorithm for linear regression }
type
  TRegAlgo = (
    GAUSS_JORDAN,  { Gauss-Jordan solution of normal equations }
    SVD);          { Singular value decomposition }

{ Optimization algorithm for nonlinear regression }
type
  TOptAlgo = (
    NL_MARQ,       { Marquardt algorithm }
    NL_SIMP,       { Simplex algorithm }
    NL_BFGS,       { BFGS algorithm }
    NL_SA,         { Simulated annealing }
    NL_MH);        { Metropolis-Hastings }

{ Regression modes }
type
  TRegMode = (UNWEIGHTED, WEIGHTED);

{ Regression function }
type
  TRegFunc = function(X : Float; B : TVector) : Float;

{ Procedure to compute the derivatives of the regression function
  with respect to the regression parameters }
type
  TDerivProc = procedure(RegFunc : TRegFunc;
                         X, Y    : Float;
                         B, D    : TVector);

{ Test of regression }
type
  TRegTest = record
    Vr,              { Residual variance }
    R2,              { Coefficient of determination }
    R2a,             { Adjusted coeff. of determination }
    F,               { Variance ratio (explained/residual) }
    Prob : Float;    { Probability of F }
  end;

var
  MHFile : String;   { File for saving Metropolis-Hastings results }

{ **********************************************************************
  Procedures to modify the regression settings
  ********************************************************************** }

procedure SetRegAlgo(Algo : TRegAlgo);
{ ----------------------------------------------------------------------
  Sets the linear regression algorithm according to Algo, which must be
  GAUSS_JORDAN or SVD. The default algorithm is SVD.
  ---------------------------------------------------------------------- }

procedure SetOptAlgo(Algo : TOptAlgo);
{ ----------------------------------------------------------------------
  Sets the optimization algorithm according to Algo, which must be
  NL_MARQ, NL_SIMP, NL_BFGS, NL_SA or NL_MH. The default is NL_MARQ
  ---------------------------------------------------------------------- }

procedure SetFirstPoint(Index : Integer);
{ ----------------------------------------------------------------------
  Sets the index of the first data point (usually 0 or 1). The default
  value is 1.
  ---------------------------------------------------------------------- }

function GetRegAlgo : TRegAlgo;
{ ----------------------------------------------------------------------
  Returns the linear regression algorithm
  ---------------------------------------------------------------------- }

function GetOptAlgo : TOptAlgo;
{ ----------------------------------------------------------------------
  Returns the optimization algorithm
  ---------------------------------------------------------------------- }

function GetFirstPoint : Integer;
{ ----------------------------------------------------------------------
  Returns the index of the first data point
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Unweighted regression routines
  **********************************************************************
  These routines fit equations to data by minimizing the sum of squared
  residuals :
                      SS = Sum [y(k) - ycalc(k)]^2

  where y(k) and ycalc(k) are respectively the observed and calculated
  value of the dependent variable for observation k. ycalc(k) is a
  function of the regression parameters b(0), b(1) ...

  The following regression types are implemented :

  * Simple linear regression :

                        y(k) = b(0) + b(1) * x(k)

  * Multiple linear regression :

    y(k) = b(0) + b(1) * x(1,k) + b(2) * x(2,k) + ... + b(Nvar) * x(Nvar,k)

  * Polynomial regression :

      y(k) = b(0) + b(1) * x(k) + b(2) * x(k)^2 + ... + b(Deg) * x(k)^Deg

  * Nonlinear regression :

                   y(k) = f[x(k), b(0), b(1), ... ]

    where f is a user-specified function.

  The following parameters are common to all routines :

  Input  : X = Vector or matrix of independent variables
           Y = Vector of dependent variable
           N = Index of the last observation
  Output : B = Regression parameters
           V = Inverse matrix of normal equations
  ********************************************************************** }

function LinFit(X, Y : TVector;
                N    : Integer;
                B    : TVector;
                V    : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Simple linear regression
  ---------------------------------------------------------------------- }

function MulFit(X        : TMatrix;
                Y        : TVector;
                N, Nvar  : Integer;
                ConsTerm : Boolean;
                B        : TVector;
                V        : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Multiple linear regression
  ----------------------------------------------------------------------
  Additional input parameters :
  Nvar     = Index of the last independent variable
  ConsTerm = Flags the presence of a constant term b(0)
  ---------------------------------------------------------------------- }

function PolFit(X, Y   : TVector;
                N, Deg : Integer;
                B      : TVector;
                V      : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Polynomial regression
  ----------------------------------------------------------------------
  Additional input parameter :
  Deg = Degree of polynomial
  ---------------------------------------------------------------------- }

function NLFit(RegFunc                    : TRegFunc;
               DerivProc                  : TDerivProc;
               X, Y                       : TVector;
               N, Lbound, Ubound, MaxIter : Integer;
               Tol                        : Float;
               B, B_min, B_max            : TVector;
               V                          : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Nonlinear regression
  ----------------------------------------------------------------------
  Additional input parameters :
  RegFunc        = Regression function
  DerivProc      = Procedure to compute the derivatives of RegFunc
  Lbound, Ubound = Indices of first and last function parameters
  MaxIter        = Maximum number of iterations
  Tol            = Required parameter precision
  B              = Initial parameter values
  B_min, B_max   = Lower and upper parameter bounds
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Weighted regression routines
  **********************************************************************
  These routines fit equations to data by minimizing the sum of weighted
  squared residuals :

                      SWS = Sum w(k)*[y(k) - ycalc(k)]^2

  where the "weight" w(k) is inversely proportional to the variance v(k)
  of the observation y(k). v(k) is usually computed as :

                       v(k) = Vr * g[y(k)] = Vr / w(k)

  where Vr is the residual variance and g is a user-specified function
  (e.g. g[y(k)] = y(k)^2 for a constant coefficient of variation).

  Function syntax and results are the same than for unweighted regression
  except that the vector of weights (W) is passed as an additional input
  parameter.
  ********************************************************************** }

function WLinFit(X, Y, W : TVector;
                 N       : Integer;
                 B       : TVector;
                 V       : TMatrix) : Integer;

function WMulFit(X        : TMatrix;
                 Y, W     : TVector;
                 N, Nvar  : Integer;
                 ConsTerm : Boolean;
                 B        : TVector;
                 V        : TMatrix) : Integer;

function WPolFit(X, Y, W : TVector;
                 N, Deg  : Integer;
                 B       : TVector;
                 V       : TMatrix) : Integer;

function WNLFit(RegFunc                    : TRegFunc;
                DerivProc                  : TDerivProc;
                X, Y, W                    : TVector;
                N, Lbound, Ubound, MaxIter : Integer;
                Tol                        : Float;
                B, B_min, B_max            : TVector;
                V                          : TMatrix) : Integer;

{ **********************************************************************
  Procedure to compute the derivatives of the regression function by
  numerical differentiation.
  ********************************************************************** }

procedure NumDeriv(RegFunc : TRegFunc;
                   X, Y    : Float;
                   B, D    : TVector);
{ ----------------------------------------------------------------------
  Input parameters  : RegFunc = Regression function
                      X, Y    = Coordinates of point
                      B       = Regression parameters

  Output parameter  : D       = Derivatives (D[I] contains the
                                derivative w.r.t. parameter B[I])
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Routines to test the quality of the regression
  **********************************************************************
  These routines compute the variance-covariance matrix of the fitted
  parameters and the different statistics used to test the quality of
  the fit.

  Input parameters  : Y      = Vector of dependent variable
                      Ycalc  = Computed Y values
                      W      = Vector of weights (if any)
                      N      = Index of the last observation
                      Lbound,
                      Ubound = Indices of first & last fitted parameters
                      V      = Inverse normal equations matrix

  Output parameters : V      = Variance-covariance matrix
                      Test   = Test statistics (Vr, R2, R2a, F, Prob)
  ********************************************************************** }

procedure RegTest(Y, Ycalc          : TVector;
                  N, Lbound, Ubound : Integer;
                  V                 : TMatrix;
                  var Test          : TRegTest);
{ ----------------------------------------------------------------------
  Test of unweighted regression
  ---------------------------------------------------------------------- }

procedure WRegTest(Y, Ycalc, W       : TVector;
                   N, Lbound, Ubound : Integer;
                   V                 : TMatrix;
                   var Test          : TRegTest);
{ ----------------------------------------------------------------------
  Test of weighted regression
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Test of regression parameters
  ********************************************************************** }

procedure ParamTest(B                 : TVector;
                    V                 : TMatrix;
                    N, Lbound, Ubound : Integer;
                    S, T, Prob        : TVector);
{ ----------------------------------------------------------------------
  This routine tests the significance of the parameters. It must be
  called AFTER RegTest or WRegTest since it uses the variance-covariance
  matrix.
  ----------------------------------------------------------------------
  Input parameters  : B      = Regression parameters
                      V      = Variance-covariance matrix
                      N      = Index of the last observation
                      Lbound,
                      Ubound = Indices of first & last fitted parameters
  ----------------------------------------------------------------------
  Output parameters : S      = Standard deviations of parameters
                      T      = Student's t
                      Prob   = Probabilities
  ---------------------------------------------------------------------- }

{ **********************************************************************
  Correlation and principal component analysis

  Common parameters:

  X              = matrix of variables (X[I] contains the I-th variable)
  N              = Index of the last observation
  Lbound, Ubound = Indices of first & last variables
  M              = Mean vector (M[I] = mean of X[I])
  S              = Vector of standard deviations
  V              = Variance-covariance matrix
  R              = Correlation matrix
  ********************************************************************** }

procedure VecMean(X                 : TMatrix;
                  N, Lbound, Ubound : Integer;
                  M                 : TVector);
{ ----------------------------------------------------------------------
  Computes the mean vector (M) from matrix X

  Input  : X, Lbound, Ubound
  Output : M
  ---------------------------------------------------------------------- }

procedure VecSD(X                 : TMatrix;
                N, Lbound, Ubound : Integer;
                M, S              : TVector);
{ ----------------------------------------------------------------------
  Computes the vector of standard deviations (S) from matrix X

  Input  : X, Lbound, Ubound, M
  Output : S
  ---------------------------------------------------------------------- }

procedure MatVarCov(X                 : TMatrix;
                    N, Lbound, Ubound : Integer;
                    M                 : TVector;
                    V                 : TMatrix);
{ ----------------------------------------------------------------------
  Computes the variance-covariance matrix (V) from matrix X

  Input  : X, Lbound, Ubound, M
  Output : V
  ---------------------------------------------------------------------- }

procedure MatCorrel(V              : TMatrix;
                    Lbound, Ubound : Integer;
                    R              : TMatrix);
{ ----------------------------------------------------------------------
  Computes the correlation matrix (R) from the variance-covariance
  matrix (V)

  Input  : V, Lbound, Ubound
  Output : R
  ---------------------------------------------------------------------- }

function PCA(R              : TMatrix;
             Lbound, Ubound : Integer;
             Lambda         : TVector;
             C, Rc          : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Performs a principal component analysis of the correlation matrix R
  ----------------------------------------------------------------------
  Input  : R, Lbound, Ubound
  Output : Lambda = Eigenvalues of the correlation matrix
                    (in descending order)
           C      = Eigenvectors of the correlation matrix
                    (C[I] is the I-th eigenvector)
           Rc     = Correlations between principal factors and variables
                    (Rc[I,J] is the correlation coefficient between
                     factor I and variable J)
  ----------------------------------------------------------------------
  Possible results : MAT_OK       : No error
                     MAT_NON_CONV : Non-convergence of eigenvalue
                                    determination
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix R
  ---------------------------------------------------------------------- }

procedure ScaleVar(X                 : TMatrix;
                   N, Lbound, Ubound : Integer;
                   M, S              : TVector;
                   Z                 : TMatrix);
{ ----------------------------------------------------------------------
  Scales a set of variables by subtracting means and dividing by SD's
  ----------------------------------------------------------------------
  Input  : X, N, Lbound, Ubound, M, S
  Output : Z = matrix of scaled variables (Z[I] contains the I-th var.)
  ---------------------------------------------------------------------- }

procedure PrinFac(Z                 : TMatrix;
                  N, Lbound, Ubound : Integer;
                  C, F              : TMatrix);
{ ----------------------------------------------------------------------
  Computes principal factors
  ----------------------------------------------------------------------
  Input  : Z, N, Lbound, Ubound
           C = matrix of eigenvectors from PCA
  Output : F = matrix of principal factors (F[I] contains the I-th factor)
  ---------------------------------------------------------------------- }

implementation

{ Constants for eigenvalue determination in PCA }
const
  PCA_MAXITER = 100;      { Max number of iterations }
  PCA_TOL     = 1.0E-6;   { Required precision }
  MAX_FUNC    = 1.0E+30;  { Max. value for objective function
                            (used to prevent overflow) }
{ Default settings }
var
  RegAlgo    : TRegAlgo;  { Linear regression algorithm }
  OptAlgo    : TOptAlgo;  { Optimization algorithms }
  FirstPoint : Integer;   { Index of first data point }

{ Global variables used by the nonlinear regression routines }
var
  NN         : Integer;     { Number of observations }
  XX         : TVector;     { X coordinates }
  YY         : TVector;     { Y coordinates }
  WW         : TVector;     { Weights }
  YYcalc     : TVector;     { Estimated Y values }
  FirstParam : Integer;     { Index of first fitted parameter }
  LastParam  : Integer;     { Index of last fitted parameter }
  ParamMin   : TVector;     { Lower bounds on parameters }
  ParamMax   : TVector;     { Higher bounds on parameters }
  RegFunc1   : TRegFunc;    { Regression function }
  DerivProc1 : TDerivProc;  { Derivation procedure }

  function TolSVD(N : Integer) : Float;
  { This function sets the relative threshold below which a singular value
    is considered zero. N is the number of observations. }
  begin
    TolSVD := N * MACHEP;
  end;

  procedure SetRegAlgo(Algo : TRegAlgo);
  begin
    RegAlgo := Algo;
  end;

  procedure SetOptAlgo(Algo : TOptAlgo);
  begin
    OptAlgo := Algo;
  end;

  procedure SetFirstPoint(Index : Integer);
  begin
    if Index >= 0 then
      FirstPoint := Index;
  end;

  function GetRegAlgo : TRegAlgo;
  begin
    GetRegAlgo := RegAlgo;
  end;

  function GetOptAlgo : TOptAlgo;
  begin
    GetOptAlgo := OptAlgo;
  end;

  function GetFirstPoint : Integer;
  begin
    GetFirstPoint := FirstPoint;
  end;

  function GenLinFit(Mode    : TRegMode;
                     X, Y, W : TVector;
                     N       : Integer;
                     B       : TVector;
                     V       : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  General linear regression routine
  ---------------------------------------------------------------------- }
  var
    WX, S, SX, SY, SX2, SXY, D : Float;
    K : Integer;
  begin
    S := 0.0;
    SX := 0.0;
    SY := 0.0;
    SX2 := 0.0;
    SXY := 0.0;
    if Mode = UNWEIGHTED then
      begin
        S := N - FirstPoint + 1;
        for K := FirstPoint to N do
          begin
            SX := SX + X[K];
            SY := SY + Y[K];
            SX2 := SX2 + Sqr(X[K]);
            SXY := SXY + X[K] * Y[K];
          end;
      end
    else
      begin
        for K := FirstPoint to N do
          begin
            WX := W[K] * X[K];
            S := S + W[K];
            SX := SX + WX;
            SY := SY + W[K] * Y[K];
            SX2 := SX2 + WX * X[K];
            SXY := SXY + WX * Y[K];
          end;
      end;
    D := S * SX2 - Sqr(SX);
    if D <= 0.0 then
      GenLinFit := MAT_SINGUL
    else
      begin
        V[0,0] := SX2 / D;
        V[0,1] := - SX / D;
        V[1,0] := V[0,1];
        V[1,1] := S / D;
        B[0] := V[0,0] * SY + V[0,1] * SXY;
        B[1] := V[1,0] * SY + V[1,1] * SXY;
        GenLinFit := MAT_OK;
      end;
  end;

  function LinFit(X, Y : TVector;
                  N    : Integer;
                  B    : TVector;
                  V    : TMatrix) : Integer;
  var
    W : TVector;
  begin
    W := nil;
    LinFit := GenLinFit(UNWEIGHTED, X, Y, W, N, B, V);
  end;

  function WLinFit(X, Y, W : TVector;
                   N       : Integer;
                   B       : TVector;
                   V       : TMatrix) : Integer;
  begin
    WLinFit := GenLinFit(WEIGHTED, X, Y, W, N, B, V);
  end;

  function Gauss_GenMulFit(Mode     : TRegMode;
                           X        : TMatrix;
                           Y, W     : TVector;
                           N, Nvar  : Integer;
                           ConsTerm : Boolean;
                           B        : TVector;
                           V        : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  General multiple linear regression routine (Gauss-Jordan algorithm)
  ---------------------------------------------------------------------- }
  var
    A       : TMatrix;  { Matrix of normal equations }
    G       : TVector;  { Constant vector }
    Det     : Float;    { Determinant of A }
    I, J, K : Integer;  { Loop variables }
    WX      : Float;
  begin
    DimMatrix(A, Nvar, Nvar);
    DimVector(G, Nvar);

    { If constant term, set line 0 and column 0 of matrix A,
      and element 0 of vecteur G }
    if ConsTerm then
      begin
        if Mode = UNWEIGHTED then
          begin
            A[0,0] := Int(N - FirstPoint + 1);
            for K := FirstPoint to N do
              begin
                for J := 1 to Nvar do
                  A[0,J] := A[0,J] + X[J,K];
                G[0] := G[0] + Y[K];
              end;
          end
        else
          begin
            for K := FirstPoint to N do
              begin
                A[0,0] := A[0,0] + W[K];
                for J := 1 to Nvar do
                  A[0,J] := A[0,J] + W[K] * X[J,K];
                G[0] := G[0] + W[K] * Y[K];
              end;
          end;
        for J := 1 to Nvar do
          A[J,0] := A[0,J];
      end;

    { Set other elements of A and G }
    if Mode = UNWEIGHTED then
      for K := FirstPoint to N do
        for I := 1 to Nvar do
          begin
            for J := I to Nvar do
              A[I,J] := A[I,J] + X[I,K] * X[J,K];
            G[I] := G[I] + X[I,K] * Y[K];
          end
    else
      for K := FirstPoint to N do
        for I := 1 to Nvar do
          begin
            WX := W[K] * X[I,K];
            for J := I to Nvar do
              A[I,J] := A[I,J] + WX * X[J,K];
            G[I] := G[I] + WX * Y[K];
          end;

    { Fill in symmetric matrix }
    for I := 2 to Nvar do
      for J := 1 to Pred(I) do
        A[I,J] := A[J,I];

    { Solve normal equations }
    if ConsTerm then
      Gauss_GenMulFit := GaussJordan(A, G, 0, Nvar, V, B, Det)
    else
      Gauss_GenMulFit := GaussJordan(A, G, 1, Nvar, V, B, Det);
  end;

  function SVD_GenMulFit(Mode     : TRegMode;
                         X        : TMatrix;
                         Y, W     : TVector;
                         N, Nvar  : Integer;
                         ConsTerm : Boolean;
                         B        : TVector;
                         V        : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  General multiple linear regression routine (SVD algorithm)
  ---------------------------------------------------------------------- }
  var
    U       : TMatrix;  { Matrix of independent variables for SVD }
    Z       : TVector;  { Vector of dependent variables for SVD }
    S       : TVector;  { Singular values }
    S2inv   : TVector;  { Inverses of squared singular values }
    V1      : TMatrix;  { Orthogonal matrix from SVD }
    Lbound  : Integer;  { Lower bound of U matrix in both dims. }
    Ubound  : Integer;  { Upper bound of U matrix in 1st dim. }
    I, J, K : Integer;  { Loop variables }
    Sigma   : Float;    { Square root of weight }
    Sum     : Float;    { Element of variance-covariance matrix }
    ErrCode : Integer;  { Error code }
  begin
    if ConsTerm then
      begin
        Lbound := 0;
        Ubound := N - FirstPoint;
      end
    else
      begin
        Lbound := 1;
        Ubound := N - FirstPoint + 1;
      end;

    { Dimension arrays }
    DimMatrix(U, Ubound, Nvar);
    DimVector(Z, Ubound);
    DimVector(S, Nvar);
    DimVector(S2inv, Nvar);
    DimMatrix(V1, Nvar, Nvar);

    { ----------------------------------------------------------
      Prepare arrays for SVD :
      If constant term, use U[0..(N - FirstPoint), 0..Nvar]
                        and Z[0..(N - FirstPoint)]
      Else              use U[1..(N - FirstPoint + 1), 1..Nvar]
                        and Z[1..(N - FirstPoint + 1)]
      ---------------------------------------------------------- }
    if Mode = UNWEIGHTED then
      for I := Lbound to Ubound do
        begin
          K := I - Lbound + FirstPoint;
          Z[I] := Y[K];
          if ConsTerm then
            U[I,0] := 1.0;
          for J := 1 to Nvar do
            U[I,J] := X[J,K];
        end
    else
      for I := Lbound to Ubound do
        begin
          K := I - Lbound + FirstPoint;
          Sigma := Sqrt(W[K]);
          Z[I] := Y[K] * Sigma;
          if ConsTerm then
            U[I,0] := Sigma;
          for J := 1 to Nvar do
            U[I,J] := X[J,K] * Sigma;
        end;

    { Perform singular value decomposition }
    ErrCode := SV_Decomp(U, Lbound, Ubound, Nvar, S, V1);

    if ErrCode = MAT_OK then
      begin
        { Set the lowest singular values to zero }
        SV_SetZero(S, Lbound, Nvar, TolSVD(N - FirstPoint + 1));

        { Solve the system }
        SV_Solve(U, S, V1, Z, Lbound, Ubound, Nvar, B);

        { Compute variance-covariance matrix }
        for I := Lbound to Nvar do
          if S[I] > 0.0 then
            S2inv[I] := 1.0 / Sqr(S[I])
          else
            S2inv[I] := 0.0;
        for I := Lbound to Nvar do
          for J := Lbound to I do
            begin
              Sum := 0.0;
              for K := Lbound to Nvar do
                Sum := Sum + V1[I,K] * V1[J,K] * S2inv[K];
              V[I,J] := Sum;
              V[J,I] := Sum;
            end;
      end;

    SVD_GenMulFit := ErrCode;
  end;

  function GenMulFit(Mode     : TRegMode;
                     X        : TMatrix;
                     Y, W     : TVector;
                     N, Nvar  : Integer;
                     ConsTerm : Boolean;
                     B        : TVector;
                     V        : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  General multiple linear regression routine
  ---------------------------------------------------------------------- }
  var
    ErrCode : Integer;
  begin
    ErrCode := 0;
    case RegAlgo of
      GAUSS_JORDAN : ErrCode := Gauss_GenMulFit(Mode, X, Y, W, N, Nvar,
                                                  ConsTerm, B, V);
      SVD :          ErrCode := SVD_GenMulFit(Mode, X, Y, W, N, Nvar,
                                                ConsTerm, B, V);
    end;
    GenMulFit := ErrCode;
  end;

  function MulFit(X        : TMatrix;
                  Y        : TVector;
                  N, Nvar  : Integer;
                  ConsTerm : Boolean;
                  B        : TVector;
                  V        : TMatrix) : Integer;
  var
    W : TVector;
  begin
    W := nil;
    MulFit := GenMulFit(UNWEIGHTED, X, Y, W, N, Nvar, ConsTerm, B, V);
  end;

  function WMulFit(X        : TMatrix;
                   Y, W     : TVector;
                   N, Nvar  : Integer;
                   ConsTerm : Boolean;
                   B        : TVector;
                   V        : TMatrix) : Integer;
  begin
    WMulFit := GenMulFit(WEIGHTED, X, Y, W, N, Nvar, ConsTerm, B, V);
  end;

  procedure PowMat(X : TVector; N, Deg : Integer; U : TMatrix);
{ ----------------------------------------------------------------------
  Computes matrix of increasing powers of X for polynomial regression
  ---------------------------------------------------------------------- }
  var
    I, K : Integer;
  begin
    for K := FirstPoint to N do
      begin
        U[1,K] := X[K];
        for I := 2 to Deg do
          U[I,K] := U[I - 1,K] * X[K];
      end;
  end;

  function GenPolFit(Mode    : TRegMode;
                     X, Y, W : TVector;
                     N, Deg  : Integer;
                     B       : TVector;
                     V       : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  General polynomial regression routine
  ---------------------------------------------------------------------- }
  var
    U : TMatrix;
  begin
    DimMatrix(U, Deg, N);
    PowMat(X, N, Deg, U);
    GenPolFit := GenMulFit(Mode, U, Y, W, N, Deg, True, B, V);
  end;

  function PolFit(X, Y   : TVector;
                  N, Deg : Integer;
                  B      : TVector;
                  V      : TMatrix) : Integer;
  var
    W : TVector;
  begin
    W := nil;
    PolFit := GenPolFit(UNWEIGHTED, X, Y, W, N, Deg, B, V);
  end;

  function WPolFit(X, Y, W : TVector;
                   N, Deg  : Integer;
                   B       : TVector;
                   V       : TMatrix) : Integer;
  begin
    WPolFit := GenPolFit(WEIGHTED, X, Y, W, N, Deg, B, V);
  end;

  procedure SetGlobalVar(RegFunc           : TRegFunc;
                         DerivProc         : TDerivProc;
                         Mode              : TRegMode;
                         X, Y, W           : TVector;
                         N, Lbound, Ubound : Integer;
                         B_min, B_max      : TVector);
  { Sets the global variables used by the nonlinear regression routines }
  begin
    NN := N;
    DimVector(XX, N);
    DimVector(YY, N);
    DimVector(YYcalc, N);

    CopyVector(XX, X, FirstPoint, N);
    CopyVector(YY, Y, FirstPoint, N);

    if Mode = WEIGHTED then
      begin
        DimVector(WW, N);
        CopyVector(WW, W, FirstPoint, N);
      end;

    DimVector(ParamMin, Ubound);
    DimVector(ParamMax, Ubound);

    CopyVector(ParamMin, B_min, Lbound, Ubound);
    CopyVector(ParamMax, B_max, Lbound, Ubound);

    FirstParam := Lbound;
    LastParam := Ubound;

    RegFunc1 := RegFunc;
    DerivProc1 := DerivProc;
  end;

  procedure NumDeriv(RegFunc : TRegFunc;
                     X, Y    : Float;
                     B, D    : TVector);
  var
    I             : Integer;
    Eps, Temp, Y1 : Float;
  begin
    Eps := Sqrt(MACHEP);
    for I := FirstParam to LastParam do
      begin
        Temp := B[I];                      { Save parameter }
        B[I] := B[I] + Eps * Abs(B[I]);    { Modified parameter }
        Y1 := RegFunc(X, B);
        D[I] := (Y1 - Y) / (B[I] - Temp);  { Derivative }
        B[I] := Temp;                      { Restore parameter }
      end;
  end;

  function OutOfBounds(B, B_min, B_max : TVector) : Boolean;
  { Check if the parameters are inside the bounds }
  var
    I   : Integer;
    OoB : Boolean;
  begin
    I := FirstParam;
    repeat
      OoB := (B[I] < B_min[I]) or (B[I] > B_max[I]);
      Inc(I);
    until OoB or (I > LastParam);
    OutOfBounds := OoB;
  end;

  function OLS_ObjFunc(B : TVector) : Float;
  { Objective function for unweighted nonlinear regression }
  var
    K : Integer;
    S : Float;
  begin
    if OutOfBounds(B, ParamMin, ParamMax) then
      begin
        OLS_ObjFunc := MAX_FUNC;
        Exit;
      end;
    S := 0.0;
    K := FirstPoint;
    repeat
      YYcalc[K] := RegFunc1(XX[K], B);
      S := S + Sqr(YY[K] - YYcalc[K]);
      Inc(K);
    until (K > NN) or (S > MAX_FUNC);
    if S > MAX_FUNC then S := MAX_FUNC;
    OLS_ObjFunc := S;
  end;

  procedure OLS_Gradient(Func           : TFuncNVar;
                         B              : TVector;
                         Lbound, Ubound : Integer;
                         G              : TVector);
  { Gradient for unweighted nonlinear regression.
    Func is a dummy parameter here. }
  var
    I, K : Integer;  { Loop variables }
    R    : Float;    { Residual }
    D    : TVector;  { Derivatives of the regression function }
  begin
    DimVector(D, Ubound);

    { Initialization }
    for I := Lbound to Ubound do
      G[I] := 0.0;

    { Compute Gradient }
    for K := FirstPoint to NN do
      begin
        R := YY[K] - YYcalc[K];
        DerivProc1(RegFunc1, XX[K], YYcalc[K], B, D);
        for I := Lbound to Ubound do
          G[I] := G[I] - D[I] * R;
      end;

    for I := Lbound to Ubound do
      G[I] := 2.0 * G[I];
  end;

  procedure OLS_HessGrad(Func           : TFuncNVar;
                         B              : TVector;
                         Lbound, Ubound : Integer;
                         G              : TVector;
                         H              : TMatrix);
  { Gradient and Hessian for unweighted nonlinear regression.
    Func is a dummy parameter here. }
  var
    I, J, K : Integer;  { Loop variables }
    R       : Float;    { Residual }
    D       : TVector;  { Derivatives of the regression function }
  begin
    DimVector(D, Ubound);

    { Initializations }
    for I := Lbound to Ubound do
      begin
        G[I] := 0.0;
        for J := I to Ubound do
          H[I,J] := 0.0;
      end;

    { Compute Gradient & Hessian }
    for K := FirstPoint to NN do
      begin
        R := YY[K] - YYcalc[K];
        DerivProc1(RegFunc1, XX[K], YYcalc[K], B, D);
        for I := Lbound to Ubound do
          begin
            G[I] := G[I] - D[I] * R;
            for J := I to Ubound do
              H[I,J] := H[I,J] + D[I] * D[J];
          end;
      end;

    { Fill in symmetric matrix }
    for I := Succ(Lbound) to Ubound do
      for J := Lbound to Pred(I) do
        H[I,J] := H[J,I];
  end;

  function WLS_ObjFunc(B : TVector) : Float;
  { Objective function for weighted nonlinear regression }
  var
    K : Integer;
    S : Float;
  begin
    if OutOfBounds(B, ParamMin, ParamMax) then
      begin
        WLS_ObjFunc := MAX_FUNC;
        Exit;
      end;
    S := 0.0;
    K := FirstPoint;
    repeat
      YYcalc[K] := RegFunc1(XX[K], B);
      S := S + WW[K] * Sqr(YY[K] - YYcalc[K]);
      Inc(K);
    until (K > NN) or (S > MAX_FUNC);
    if S > MAX_FUNC then S := MAX_FUNC;
    WLS_ObjFunc := S;
  end;

  procedure WLS_Gradient(Func           : TFuncNVar;
                         B              : TVector;
                         Lbound, Ubound : Integer;
                         G              : TVector);
  { Gradient for weighted nonlinear regression.
    Func is a dummy parameter here. }
  var
    I, K : Integer;  { Loop variables }
    R    : Float;    { Residual }
    D    : TVector;  { Derivatives of the regression function }
    WD   : Float;    { Weighted derivative }
  begin
    DimVector(D, Ubound);

    { Initialization }
    for I := Lbound to Ubound do
      G[I] := 0.0;

    { Compute Gradient }
    for K := FirstPoint to NN do
      begin
        R := YY[K] - YYcalc[K];
        DerivProc1(RegFunc1, XX[K], YYcalc[K], B, D);
        for I := Lbound to Ubound do
          begin
            WD := WW[K] * D[I];
            G[I] := G[I] - WD * R;
          end;
      end;

    for I := Lbound to Ubound do
      G[I] := 2.0 * G[I];
  end;

  procedure WLS_HessGrad(Func           : TFuncNVar;
                         B              : TVector;
                         Lbound, Ubound : Integer;
                         G              : TVector;
                         H              : TMatrix);
  { Gradient and Hessian for weighted nonlinear regression.
    Func is a dummy parameter here. }
  var
    I, J, K : Integer;  { Loop variables }
    R       : Float;    { Residual }
    D       : TVector;  { Derivatives of the regression function }
    WD      : Float;    { Weighted derivative }
  begin
    DimVector(D, Ubound);

    { Initialization }
    for I := Lbound to Ubound do
      begin
        G[I] := 0.0;
        for J := I to Ubound do
          H[I,J] := 0.0;
      end;

    { Compute Gradient & Hessian }
    for K := FirstPoint to NN do
      begin
        R := YY[K] - YYcalc[K];
        DerivProc1(RegFunc1, XX[K], YYcalc[K], B, D);
        for I := Lbound to Ubound do
          begin
            WD := WW[K] * D[I];
            G[I] := G[I] - WD * R;
            for J := I to Ubound do
              H[I,J] := H[I,J] + WD * D[J];
          end;
      end;

    { Fill in symmetric matrix }
    for I := Succ(Lbound) to Ubound do
      for J := Lbound to Pred(I) do
        H[I,J] := H[J,I];
  end;

  procedure SaveSim(B_sim : TMatrix; Lbound, Ubound : Integer);
  { Saves the simulated parameters from the last Metropolis-Hastings cycle }
  var
    F    : Text;
    I, J : Integer;
  begin
    Assign(F, MHFile);
    Rewrite(F);

    Writeln(F, 'Simulation (Metropolis-Hastings)');
    Write(F, ' Iter');
    for I := Lbound to Ubound do
      Write(F, 'b':14, I);
    Writeln(F);

    for J := 1 to MH_SavedSim do
      begin
        Write(F, J:5);
        for I := Lbound to Ubound do
          Write(F, ' ', B_sim[I,J]:14:6);
        Writeln(F);
      end;

    Close(F);
  end;

  function Simulate(ObjFunc         : TFuncNvar;
                    B, B_min, B_max : TVector;
                    Lbound, Ubound  : Integer;
                    var F_min       : Float;
                    V : TMatrix)    : Integer;
  { Simulation by the Metropolis-Hastings algorithm }
  var
    B_sim         : TMatrix;
    I, J, ErrCode : Integer;
  begin
    DimMatrix(B_sim, Ubound, MH_SavedSim);
    for I := Lbound to Ubound do
      B[I] := B_min[I] + RanMar * (B_max[I] - B_min[I]);

    { Initialize variance-covariance matrix }
    for I := Lbound to Ubound do
      for J := Lbound to Ubound do
        if I = J then
          { The parameter range is assumed to cover 6 SD's }
          V[I,J] := Sqr((B_max[I] - B_min[I]) / 6.0)
        else
          V[I,J] := 0.0;

    ErrCode := Hastings(ObjFunc, 2.0, B, V, Lbound, Ubound, B_sim, B, F_min);

    if (ErrCode = 0) and (MHFile <> '') then
      SaveSim(B_sim, Lbound, Ubound);

    Simulate := ErrCode;
  end;

  function GenNLFit(RegFunc                    : TRegFunc;
                    DerivProc                  : TDerivProc;
                    Mode                       : TRegMode;
                    X, Y, W                    : TVector;
                    N, Lbound, Ubound, MaxIter : Integer;
                    Tol                        : Float;
                    B, B_min, B_max            : TVector;
                    V                          : TMatrix) : Integer;
  { --------------------------------------------------------------------
    General nonlinear regression routine
    -------------------------------------------------------------------- }
  var
    F_min    : Float;      { Value of objective function at minimum }
    ErrCode  : Integer;    { Error code }
    G        : TVector;    { Gradient vector }
    H        : TMatrix;    { Hessian matrix }
    ObjFunc  : TFuncNVar;  { Objective function }
    GradProc : TGradient;  { Procedure to compute gradient }
    HessProc : THessGrad;  { Procedure to compute gradient and hessian }
  begin
    SetGlobalVar(RegFunc, DerivProc, Mode, X, Y, W,
                 N, Lbound, Ubound, B_min, B_max);

    ObjFunc  := OLS_ObjFunc;
    GradProc := OLS_Gradient;
    HessProc := OLS_HessGrad;

    if Mode = WEIGHTED then
      begin
        ObjFunc  := WLS_ObjFunc;
        GradProc := WLS_Gradient;
        HessProc := WLS_HessGrad;
      end;

    ErrCode := 0;
    case OptAlgo of
      NL_MARQ : ErrCode := Marquardt(ObjFunc, HessProc, B, Lbound, Ubound,
                                     MaxIter, Tol, F_min, V);
      NL_SIMP : ErrCode := Simplex(ObjFunc, B, Lbound, Ubound,
                                   MaxIter, Tol, F_min);
      NL_BFGS : ErrCode := BFGS(ObjFunc, GradProc, B, Lbound, Ubound,
                                MaxIter, Tol, F_min, V);
      NL_SA   : ErrCode := SimAnn(ObjFunc, B, B_min, B_max, Lbound, Ubound,
                                  MaxIter, Tol, F_min);
      NL_MH   : ErrCode := Simulate(ObjFunc, B, B_min, B_max, Lbound, Ubound,
                                    F_min, V);
    end;

    if (OptAlgo in [NL_SIMP, NL_SA]) and (ErrCode = OPT_OK) then
      begin
        { Compute the Hessian matrix and its inverse }
        DimVector(G, Ubound);
        DimMatrix(H, Ubound, Ubound);
        case Mode of
          UNWEIGHTED : OLS_HessGrad(ObjFunc, B, Lbound, Ubound, G, H);
          WEIGHTED   : WLS_HessGrad(ObjFunc, B, Lbound, Ubound, G, H);
        end;
        if InvMat(H, Lbound, Ubound, V) = 0 then
          ErrCode := OPT_OK
        else
          ErrCode := OPT_SING;
      end;

    GenNLFit := ErrCode;
  end;

  function NLFit(RegFunc                    : TRegFunc;
                 DerivProc                  : TDerivProc;
                 X, Y                       : TVector;
                 N, Lbound, Ubound, MaxIter : Integer;
                 Tol                        : Float;
                 B, B_min, B_max            : TVector;
                 V                          : TMatrix) : Integer;
  var
    W : TVector;
  begin
    W := nil;
    NLFit := GenNLFit(RegFunc, DerivProc, UNWEIGHTED, X, Y, W, N,
                      Lbound, Ubound, MaxIter, Tol, B, B_min, B_max, V);
  end;

  function WNLFit(RegFunc : TRegFunc; DerivProc : TDerivProc;
                  X, Y, W : TVector; N, Lbound, Ubound, MaxIter : Integer;
                  Tol : Float; B, B_min, B_max : TVector; V : TMatrix) : Integer;
  begin
    WNLFit := GenNLFit(RegFunc, DerivProc, WEIGHTED, X, Y, W, N,
                       Lbound, Ubound, MaxIter, Tol, B, B_min, B_max, V);
  end;

  procedure GenRegTest(Mode              : TRegMode;
                       Y, Ycalc, W       : TVector;
                       N, Lbound, Ubound : Integer;
                       V                 : TMatrix;
                       var Test          : TRegTest);
  var
    Ybar     : Float;    { Average Y value }
    SSt      : Float;    { Total sum of squares }
    SSe      : Float;    { Explained sum of squares }
    SSr      : Float;    { Residual sum of squares }
    Nobs     : Integer;  { Number of observations }
    Npar     : Integer;  { Number of fitted parameters }
    Nu1, Nu2 : Integer;  { Degrees of freedom }
    I, J     : Integer;  { Loop variables }
  begin
    Nobs := N - FirstPoint + 1;
    Npar := Ubound - Lbound + 1;
    with Test do
      if Nobs > Npar then
        begin
          Ybar := Average(Y, FirstPoint, N);
          if Mode = UNWEIGHTED then
            begin
              SSt := SumSqrDif(Y, FirstPoint, N, Ybar);
              SSe := SumSqrDif(Ycalc, FirstPoint, N, Ybar);
              SSr := SumSqrDifVect(Y, Ycalc, FirstPoint, N);
            end
          else
            begin
              SSt := SumWSqrDif(Y, W, FirstPoint, N, Ybar);
              SSe := SumWSqrDif(Ycalc, W, FirstPoint, N, Ybar);
              SSr := SumWSqrDifVect(Y, Ycalc, W, FirstPoint, N);
            end;
          Nu1 := Npar - 1;
          Nu2 := Nobs - Npar;
          R2 := SSe / SSt;
          R2a := 1.0 - (1.0 - R2) * (Nobs - 1) / Nu2;
          Vr := SSr / Nu2;
          if Vr > 0.0 then
            begin
              F := (SSe / Nu1) / Vr;
              Prob := PSnedecor(Nu1, Nu2, F);
            end
          else
            begin
              F := MAXNUM;
              Prob := 0.0;
            end;
        end
      else
        begin
          Vr := 0.0;
          R2 := 1.0;
          R2a := 0.0;
          F := 0.0;
          Prob := 1.0;
        end;

    { Compute variance-covariance matrix }
    for I := Lbound to Ubound do
      for J := I to Ubound do
        V[I,J] := V[I,J] * Test.Vr;
    for I := Succ(Lbound) to Ubound do
      for J := Lbound to Pred(I) do
        V[I,J] := V[J,I];
  end;

  procedure RegTest(Y, Ycalc          : TVector;
                    N, Lbound, Ubound : Integer;
                    V                 : TMatrix;
                    var Test          : TRegTest);
  var
    W : TVector;
  begin
    W := nil;
    GenRegTest(UNWEIGHTED, Y, Ycalc, W, N, Lbound, Ubound, V, Test);
  end;

  procedure WRegTest(Y, Ycalc, W       : TVector;
                     N, Lbound, Ubound : Integer;
                     V                 : TMatrix;
                     var Test          : TRegTest);
  begin
    GenRegTest(WEIGHTED, Y, Ycalc, W, N, Lbound, Ubound, V, Test);
  end;

  procedure ParamTest(B                 : TVector;
                      V                 : TMatrix;
                      N, Lbound, Ubound : Integer;
                      S, T, Prob        : TVector);
  var
    I    : Integer;
    Nu   : Integer;  { Degrees of freedom }
    Nobs : Integer;  { Number of observations }
    Nvar : Integer;  { Number of indep. variables }
  begin
    Nobs := N - FirstPoint + 1;
    Nvar := Ubound - Lbound + 1;
    Nu := Nobs - Nvar;            { DoF = Nb points - Nb parameters }
    for I := Lbound to Ubound do
      if V[I,I] > 0.0 then
        begin
          S[I] := Sqrt(V[I,I]);
          T[I] := B[I] / S[I];
          Prob[I] := PStudent(Nu, T[I]);
        end
      else
        begin
          S[I] := 0.0;
          T[I] := MAXNUM;
          Prob[I] := 0.0;
        end;
  end;

  procedure VecMean(X                 : TMatrix;
                    N, Lbound, Ubound : Integer;
                    M                 : TVector);
  var
    I, K, Nobs : Integer;
    Sum        : Float;
  begin
    Nobs := N - FirstPoint + 1;
    for I := Lbound to Ubound do
      begin
        Sum := 0.0;
        for K := FirstPoint to N do
          Sum := Sum + X[I,K];
        M[I] := Sum / Nobs;
      end;
  end;

  procedure VecSD(X                 : TMatrix;
                  N, Lbound, Ubound : Integer;
                  M, S              : TVector);
  var
    I, K, Nobs : Integer;
    Sum        : Float;
  begin
    Nobs := N - FirstPoint + 1;
    for I := Lbound to Ubound do
      begin
        Sum := 0.0;
        for K := FirstPoint to N do
          Sum := Sum + Sqr(X[I,K] - M[I]);
        S[I] := Sqrt(Sum / Nobs);
      end;
  end;

  procedure MatVarCov(X                 : TMatrix;
                      N, Lbound, Ubound : Integer;
                      M                 : TVector;
                      V                 : TMatrix);
  var
    I, J, K, Nobs : Integer;
    Sum           : Float;
  begin
    Nobs := N - FirstPoint + 1;
    for I := Lbound to Ubound do
      for J := I to Ubound do
      begin
        Sum := 0.0;
        for K := FirstPoint to N do
          Sum := Sum + (X[I,K] - M[I]) * (X[J,K] - M[J]);
        V[I,J] := Sum / Nobs;
      end;
    for I := Succ(Lbound) to Ubound do
      for J := Lbound to Pred(I) do
        V[I,J] := V[J,I];
  end;

  procedure MatCorrel(V              : TMatrix;
                      Lbound, Ubound : Integer;
                      R              : TMatrix);
  var
    I, J : Integer;
  begin
    for I := Lbound to Ubound do
      begin
        R[I,I] := 1.0;
        for J := Succ(I) to Ubound do
          begin
            R[I,J] := V[I,J] / Sqrt(V[I,I] * V[J,J]);
            R[J,I] := R[I,J];
          end;
      end;
  end;

  function PCA(R              : TMatrix;
               Lbound, Ubound : Integer;
               Lambda         : TVector;
               C, Rc          : TMatrix) : Integer;
  var
    I, J, ErrCode : Integer;
    Rac           : Float;
  begin
    { Compute eigenvalues and eigenvectors of correlation matrix }
    ErrCode := Jacobi(R, Lbound, Ubound, PCA_MAXITER, PCA_TOL, C, Lambda);

    if ErrCode <> 0 then
      begin
        PCA := ErrCode;
        Exit;
      end;

    { Compute correlations between principal factors and reduced variables }
    for I := Lbound to Ubound do
      begin
        Rac := Sqrt(Lambda[I]);
        for J := Lbound to Ubound do
          Rc[I,J] := C[I,J] * Rac;
      end;

    PCA := ErrCode;
  end;

  procedure ScaleVar(X                 : TMatrix;
                     N, Lbound, Ubound : Integer;
                     M, S              : TVector;
                     Z                 : TMatrix);
  var
    I, K : Integer;
  begin
    for I := Lbound to Ubound do
      for K := FirstPoint to N do
        Z[I,K] := (X[I,K] - M[I]) / S[I];
  end;

  procedure PrinFac(Z                 : TMatrix;
                    N, Lbound, Ubound : Integer;
                    C, F              : TMatrix);
  var
    I, J, K : Integer;
  begin
    for I := Lbound to Ubound do
      for K := FirstPoint to N do
        begin
          F[I,K] := 0.0;
          for J := Lbound to Ubound do
            F[I,K] := F[I,K] + C[I,J] * Z[J,K];
        end;
  end;

begin
  MHFile     := '';
  RegAlgo    := SVD;
  OptAlgo    := NL_MARQ;
  FirstPoint := 1;
  NN         := 1;
  XX         := nil;
  YY         := nil;
  WW         := nil;
  YYcalc     := nil;
  FirstParam := 0;
  LastParam  := 1;
  ParamMin   := nil;
  ParamMax   := nil;
end.
