{ **********************************************************************
  *                          Unit POLYNOM.PAS                          *
  *                            Version 1.6d                            *
  *                      (c) J. Debord, July 2004                      *
  **********************************************************************
                   Polynomials and rational fractions
  **********************************************************************
  References:
  1) 'Mathematiques et Statistiques' by H. Haut (PSI ed.)
  2) 'Numerical Recipes' by Press et al.
  ********************************************************************** }

unit polynom;

interface

uses
  fmath, matrices;

function Poly(X : Float; Coef : TVector; Deg : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluates the polynomial :
  P(X) = Coef[0] + Coef[1] * X + Coef[2] * X^2 +...+ Coef[Deg] * X^Deg
  ---------------------------------------------------------------------- }

function RFrac(X : Float; Coef : TVector; Deg1, Deg2 : Integer) : Float;
{ ----------------------------------------------------------------------
  Evaluates the rational fraction :

           Coef[0] + Coef[1] * X + ... + Coef[Deg1] * X^Deg1
  F(X) = -----------------------------------------------------
         1 + Coef[Deg1+1] * X + ... + Coef[Deg1+Deg2] * X^Deg2
  ---------------------------------------------------------------------- }

function RootPol1(A, B : Float; var X : Float) : Integer;
{ ----------------------------------------------------------------------
  Solves the linear equation A + B * X = 0
  Returns  1 if no error (B <> 0)
          -1 if X is undetermined (A = B = 0)
          -2 if no solution (A <> 0, B = 0)
  ---------------------------------------------------------------------- }

function RootPol2(Coef, Xr, Xi : TVector) : Integer;
{ ----------------------------------------------------------------------
  Solves the quadratic equation:
  Coef[0] + Coef[1] * X + Coef[2] * X^2 = 0
  ---------------------------------------------------------------------- }

function RootPol3(Coef, Xr, Xi : TVector) : Integer;
{ ----------------------------------------------------------------------
  Solves the cubic equation:
  Coef[0] + Coef[1] * X + Coef[2] * X^2 + Coef[3] * X^3 = 0
  ---------------------------------------------------------------------- }

function RootPol4(Coef, Xr, Xi : TVector) : Integer;
{ ----------------------------------------------------------------------
  Solves the quartic equation:
  Coef[0] + Coef[1] * X + Coef[2] * X^2 + Coef[3] * X^3 +
                                             Coef[4] * X^4 = 0
  ---------------------------------------------------------------------- }

function SetRealRoots(Deg : Integer; Xr, Xi : TVector; Tol : Float) : Integer;
{ ----------------------------------------------------------------------
  Set the imaginary part of a root to zero if it is less than a
  fraction Tol of its real part. This root is therefore considered
  real. The function returns the total number of real roots.
  ---------------------------------------------------------------------- }

procedure SortRoots(Deg : Integer; Xr, Xi : TVector);
{ ----------------------------------------------------------------------
  Sort roots so that:

  (1) The Nr real roots are stored in elements [1..Nr] of vector Xr,
      in increasing order.

  (2) The complex roots are stored in elements [(Nr + 1)..Deg] of
      vectors Xr and Xi and are unordered.
  ---------------------------------------------------------------------- }


implementation

function Poly(X : Float; Coef : TVector; Deg : Integer) : Float;
var
  I : Integer;
  P : Float;
begin
  P := Coef[Deg];
  for I := Pred(Deg) downto 0 do
    P := P * X + Coef[I];
  Poly := P;
end;

function RFrac(X : Float; Coef : TVector; Deg1, Deg2 : Integer) : Float;
var
  I    : Integer;
  P, Q : Float;
begin
  P := Coef[Deg1];
  for I := Pred(Deg1) downto 0 do
    P := P * X + Coef[I];
  Q := 0.0;
  for I := (Deg1 + Deg2) downto Succ(Deg1) do
    Q := (Q + Coef[I]) * X;
  RFrac := P / (1.0 + Q);
end;

function RootPol1(A, B : Float; var X : Float) : Integer;
begin
  X := 0.0;

  if B <> 0.0 then
    begin
      if A <> 0.0 then X := - A / B;
      RootPol1 := 1;
      Exit;
    end;

  if A = 0.0 then     { 0 + 0X = 0 }
    RootPol1 := - 1
  else                { A + 0X = 0 }
    RootPol1 := - 2;
end;

function RootPol2(Coef, Xr, Xi : TVector) : Integer;
var
  Delta, F, Q : Float;

begin
  Xr[1] := 0.0; Xi[1] := 0.0;
  Xr[2] := 0.0; Xi[2] := 0.0;

  if Coef[2] = 0.0 then
    begin
      RootPol2 := RootPol1(Coef[0], Coef[1], Xr[1]);
      Exit;
    end;

  if Coef[0] = 0.0 then
    begin
      { 0 is root. Eq. becomes linear }
      if RootPol1(Coef[1], Coef[2], Xr[1]) = 1 then
        { Linear eq. has 1 solution }
        RootPol2 := 2
      else
        { Linear eq. is undetermined or impossible }
        RootPol2 := 1;
      Exit;
    end;

  Delta := Sqr(Coef[1]) - 4.0 * Coef[0] * Coef[2];

  { 2 real roots }
  if Delta > 0.0 then
    begin
      RootPol2 := 2;

      { Algorithm for minimizing roundoff errors }
      { See `Numerical Recipes'                  }
      if Coef[1] >= 0.0 then
        Q := - 0.5 * (Coef[1] + Sqrt(Delta))
      else
        Q := - 0.5 * (Coef[1] - Sqrt(Delta));

      Xr[1] := Q / Coef[2];
      Xr[2] := Coef[0] / Q;

      Exit;
    end;

  { Double real root }
  if Delta = 0.0 then
    begin
      RootPol2 := 2;
      Xr[1] := - 0.5 * Coef[1] / Coef[2];
      Xr[2] := Xr[1];
      Exit;
    end;

  { 2 complex roots }
  RootPol2 := 0;
  F := 0.5 / Coef[2];
  Xr[1] := - F * Coef[1];
  Xi[1] := Abs(F) * Sqrt(- Delta);
  Xr[2] := Xr[1];
  Xi[2] := - Xi[1];
end;

function RootPol3(Coef, Xr, Xi : TVector) : Integer;
const
  OneThird  = 0.333333333333333333;  { 1 / 3       }
  TwoPiDiv3 = 2.09439510239319549;   { 2 Pi / 3    }
  Sqrt3Div2 = 0.866025403784438647;  { Sqrt(3) / 2 }

var
  A, AA, B, C   : Float;
  Q, QQQ, R, RR : Float;
  S, T, U       : Float;
  I             : Integer;
  Cf            : TVector;

begin
  for I := 1 to 3 do
    begin
      Xr[I] := 0.0;
      Xi[I] := 0.0;
    end;

  if Coef[3] = 0.0 then
    begin
      RootPol3 := RootPol2(Coef, Xr, Xi);
      Exit;
    end;

  if Coef[0] = 0.0 then
    begin
      DimVector(Cf, 2);

      { 0 is root. Equation becomes quadratic }
      Cf[0] := Coef[1]; Cf[1] := Coef[2]; Cf[2] := Coef[3];

      { Solve quadratic equation }
      RootPol3 := RootPol2(Cf, Xr, Xi) + 1;

      Exit;
    end;

  if Coef[3] = 1.0 then
    begin
      A := Coef[2] * OneThird;
      B := Coef[1];
      C := Coef[0];
    end
  else
    begin
      A := Coef[2] / Coef[3] * OneThird;
      B := Coef[1] / Coef[3];
      C := Coef[0] / Coef[3];
    end;

  AA := A * A;

  Q := AA - OneThird * B;
  R := A * (AA - 0.5 * B) + 0.5 * C;
  RR := Sqr(R); QQQ := Q * Sqr(Q);

  if RR < QQQ then  { 3 real roots }
    begin
      RootPol3 := 3;
      S := Sqrt(Q);
      T := R / (Q * S);
      T := PiDiv2 - ArcTan(T / Sqrt(1.0 - T * T));  { ArcCos(T) }
      T := OneThird * T;
      S := - 2.0 * S;
      Xr[1] := S * Cos(T) - A;
      Xr[2] := S * Cos(T + TwoPiDiv3) - A;
      Xr[3] := S * Cos(T - TwoPiDiv3) - A;
    end
  else     { 1 real root }
    begin
      RootPol3 := 1;
      S := - Sgn(R) * Power(Abs(R) + Sqrt(RR - QQQ), OneThird);
      if S = 0.0 then T := 0.0 else T := Q / S;
      U := S + T;
      Xr[1] := U - A;          { Real root }
      Xr[2] := - 0.5 * U - A;
      Xi[2] := Sqrt3Div2 * ABS(S - T);
      Xr[3] := Xr[2]; Xi[3] := - Xi[2];
    end;
end;

function RootPol4(Coef, Xr, Xi : TVector) : Integer;
var
  A, AA, B, C, D     : Float;
  Q , R , S          : Float;
  K , KK, L, M       : Float;
  I, N1, N2          : Integer;
  Cf, Yr, Yi, Zr, Zi : TVector;

  function HighestRealRoot(Deg : Integer; Xr, Xi : TVector) : Float;
  { Find the highest real root among the roots of a polynomial }
  var
    I : Integer;
    R : Float;
  begin
    R := - MAXNUM;
    for I := 1 to Deg do
      if (Xi[I] = 0.0) and (Xr[I] > R) then
        R := Xr[I];
    HighestRealRoot := R;
  end;

begin
  for I := 1 to 4 do
    begin
      Xr[I] := 0.0;
      Xi[I] := 0.0;
    end;

  if Coef[4] = 0 then
    begin
      RootPol4 := RootPol3(Coef, Xr, Xi);
      Exit;
    end;

  DimVector(Cf, 3);

  if Coef[0] = 0.0 then
    begin
      { 0 is root. Equation becomes cubic }
      Cf[0] := Coef[1]; Cf[1] := Coef[2]; Cf[2] := Coef[3];

      { Solve cubic equation }
      RootPol4 := RootPol3(Cf, Xr, Xi) + 1;

      Exit;
    end;

  if Coef[4] = 1.0 then
    begin
      A := Coef[3] * 0.25;
      B := Coef[2];
      C := Coef[1];
      D := Coef[0];
    end
  else
    begin
      A := Coef[3] / Coef[4] * 0.25;
      B := Coef[2] / Coef[4];
      C := Coef[1] / Coef[4];
      D := Coef[0] / Coef[4];
    end;

  AA := A * A;

  Q := B - 6.0 * AA;
  R := C + A * (8.0 * AA - 2.0 * B);
  S := D - A * C + AA * (B - 3.0 * AA);

  { Compute coefficients of cubic equation }
  Cf[3] := 1.0;
  Cf[2] := 0.5 * Q;
  Cf[1] := 0.25 * (Sqr(Cf[2]) - S);

  { Solve cubic equation and set KK = highest real root }
  if (R = 0.0) and (Cf[1] < 0.0) then
    begin
      { Eq. becomes quadratic with 2 real roots }
      Cf[0] := Cf[1]; Cf[1] := Cf[2]; Cf[2] := 1.0;
      N1 := RootPol2(Cf, Xr, Xi);
      KK := HighestRealRoot(2, Xr, Xi);
    end
  else
    begin
      Cf[0] := - 0.015625 * Sqr(R);
      N1 := RootPol3(Cf, Xr, Xi);
      KK := HighestRealRoot(3, Xr, Xi);
    end;

  K := Sqrt(KK);
  if K = 0.0 then
    R := Sqrt(Sqr(Q) - 4.0 * S)
  else
    begin
      Q := Q + 4.0 * KK;
      R := 0.5 * R / K;
    end;

  L := 0.5 * (Q - R);
  M := 0.5 * (Q + R);

  { Solve quadratic equation: Y^2 + 2KY + L = 0 }
  DimVector(Yr, 2); DimVector(Yi, 2);
  Cf[0] := L; Cf[1] := 2.0 * K; Cf[2] := 1.0;
  N1 := RootPol2(Cf, Yr, Yi);

  { Solve quadratic equation: Z^2 - 2KZ + M = 0 }
  DimVector(Zr, 2); DimVector(Zi, 2);
  Cf[0] := M; Cf[1] := -Cf[1];
  N2 := RootPol2(Cf, Zr, Zi);

  { Transfer roots into vectors Xr and Xi }
  Xr[1] := Yr[1] - A; Xi[1] := Yi[1];
  Xr[2] := Yr[2] - A; Xi[2] := Yi[2];
  Xr[3] := Zr[1] - A; Xi[3] := Zi[1];
  Xr[4] := Zr[2] - A; Xi[4] := Zi[2];

  RootPol4 := N1 + N2;
end;

function SetRealRoots(Deg : Integer; Xr, Xi : TVector; Tol : Float) : Integer;
var
  I, N : Integer;
begin
  for I := 1 to Deg do
    if (Xi[I] <> 0.0) and (Abs(Xi[I]) < Tol * Abs(Xr[I])) then
      Xi[I] := 0.0;

  { Count real roots }
  N := 0;
  for I := 1 to Deg do
    if Xi[I] = 0.0 then
      Inc(N);

  SetRealRoots := N;
end;

procedure SortRoots(Deg : Integer; Xr, Xi : TVector);
var
  I, J, K, Nr, Nc : Integer;
  R, X, Y         : TVector;

  procedure Sort(X : TVector; N : Integer);
  { Sort vector X (insertion sort) }
  var
    I, J, K : Integer;
    A       : Float;
  begin
    for I := 1 to Pred(N) do
      begin
        K := I;
        A := X[I];
        for J := Succ(I) to N do
          if X[J] < A then
            begin
              K := J;
              A := X[J];
            end;
        Swap(X[I], X[K]);
      end;
  end;

begin
  { Count real and complex roots }
  Nr := 0; Nc := 0;
  for I := 1 to Deg do
    if Xi[I] = 0.0 then Inc(Nr) else Inc(Nc);

  DimVector(R, Nr);
  DimVector(X, Nc);
  DimVector(Y, Nc);

  { Store real roots in R and complex roots in (X,Y) }
  J := 0; K := 0;
  for I := 1 to Deg do
    if Xi[I] = 0.0 then
      begin
        Inc(J);
        R[J] := Xr[I];
      end
    else
      begin
        Inc(K);
        X[K] := Xr[I];
        Y[K] := Xi[I];
      end;

  { Sort vector R (insertion sort) }
  if Nr > 0 then Sort(R, Nr);

  { Transfer real roots into elements 1..Nr }
  for I := 1 to Nr do
    begin
      Xr[I] := R[I];
      Xi[I] := 0.0;
    end;

  { Transfer complex roots into elements (Nr+1)..Deg }
  for I := 1 to Nc do
    begin
      J := I + Nr;
      Xr[J] := X[I];
      Xi[J] := Y[I];
    end;
end;

end.
