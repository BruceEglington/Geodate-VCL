{ **********************************************************************
  *                           Unit RANDNUM.PAS                         *
  *                             Version 1.0d                           *
  *                     (c) J. Debord, February 2003                   *
  **********************************************************************
                          Random number generators
  ********************************************************************** }

unit randnum;

interface

uses
  fmath, matrices;

procedure RMarIn(Seed1, Seed2 : Integer);
{ ----------------------------------------------------------------------
  Initializes the Marsaglia 'Multiply with carry' random number generator
  The default initialization corresponds to RMarIn(1802, 9373)
  ---------------------------------------------------------------------- }

function IRanMar : LongInt;
{ ----------------------------------------------------------------------
  Returns a 32 bit random number in [ -2,147,483,648 ; 2,147,483,647 ]
  ---------------------------------------------------------------------- }

function RanMar : Float;
{ ----------------------------------------------------------------------
  Returns a random number in [0, 1[
  ---------------------------------------------------------------------- }

function RanGaussStd : Float;
{ ----------------------------------------------------------------------
  Returns a random number from the standard normal distribution
  (i.e. the Gaussian distribution with zero mean and unit variance)
  ---------------------------------------------------------------------- }

function RanGauss(Mu, Sigma : Float) : Float;
{ ----------------------------------------------------------------------
  Returns a random number from a Gaussian distribution
  with mean Mu and standard deviation Sigma
  ---------------------------------------------------------------------- }

procedure RanMult(M              : TVector;
                  L              : TMatrix;
                  Lbound, Ubound : Integer;
                  X              : TVector);
{ ----------------------------------------------------------------------
  Generates a random vector X from a multinormal distribution.
  M is the mean vector, L is the Cholesky factor of the
  variance-covariance matrix (L is a lower triangular matrix).
  Lbound and Ubound are the lower and upper indices of the
  elements of X.
  ---------------------------------------------------------------------- }

procedure RanMultIndep(M              : TVector;
                       S              : TVector;
                       Lbound, Ubound : Integer;
                       X              : TVector);
{ ----------------------------------------------------------------------
  Generates a random vector X from a multinormal distribution with
  uncorrelated variables. M is the mean vector, S is the vector
  of standard deviations. Lbound and Ubound are the lower and upper
  indices of the elements of X.
  ---------------------------------------------------------------------- }

implementation

var
  X1, X2     : Integer;  { Uniform random integers }
  C1, C2     : Integer;  { Carries }
  Gauss_Save : Float;    { Saves a gaussian random number }
  Gauss_Set  : Boolean;  { Flags if a gaussian number has been saved }

  procedure RMarIn(Seed1, Seed2 : Integer);
  begin
    X1 := Seed1;
    X2 := Seed2;
    C1 := 0;
    C2 := 0;
  end;

  function IRanMar : Integer;
  var
    Y1, Y2 : Integer;
  begin
    Y1 := 18000 * X1 + C1;
    X1 := Y1 and 65535;
    C1 := Y1 shr 16;
    Y2 := 30903 * X2 + C2;
    X2 := Y2 and 65535;
    C2 := Y2 shr 16;
    IRanMar := (X1 shl 16) + (X2 and 65535);
  end;

  function RanMar : Float;
  begin
    RanMar := (IRanMar + 2147483648.0) / 4294967296.0;
  end;

  function RanGaussStd : Float;
  { Computes 2 random numbers from the standard normal distribution,
    returns one and saves the other for the next call }
  var
    X, R, Theta : Float;
  begin
    if not Gauss_Set then
      begin
        repeat
          X := RanMar
        until X > 0.0;
        R := Sqrt(- 2.0 * Ln(X));
        Theta := TWOPI * RanMar;
        RanGaussStd := R * Cos(Theta);  { Return 1st number }
        Gauss_Save := R * Sin(Theta);   { Save 2nd number }
      end
    else
      RanGaussStd := Gauss_Save;        { Return saved number }
    Gauss_Set := not Gauss_Set;
  end;

  function RanGauss(Mu, Sigma : Float) : Float;
  begin
    RanGauss := Mu + Sigma * RanGaussStd;
  end;

  procedure RanMult(M              : TVector;
                    L              : TMatrix;
                    Lbound, Ubound : Integer;
                    X              : TVector);
  var
    U    : TVector;
    I, J : Integer;
  begin
    { Form a vector U of independent standard normal variates }
    DimVector(U, Ubound);
    for I := Lbound to Ubound do
      U[I] := RanGaussStd;

    { Form X = M + L*U, which follows the multinormal distribution }
    for I := Lbound to Ubound do
      begin
        X[I] := M[I];
        for J := Lbound to I do
          X[I] := X[I] + L[I,J] * U[J];
      end;
  end;

  procedure RanMultIndep(M              : TVector;
                         S              : TVector;
                         Lbound, Ubound : Integer;
                         X              : TVector);
  var
    I : Integer;
  begin
    for I := Lbound to Ubound do
      X[I] := RanGauss(M[I], S[I]);
  end;

begin
  Gauss_Save := 0.0;
  Gauss_Set := False;
  RMarIn(1802, 9373);
end.

