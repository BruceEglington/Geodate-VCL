unit uFunc;

interface

uses
  FMath, Matrices;

  function Func1(X : TVector) : Float;
  function Func2(X : TVector) : Float;
  function Func3(X : TVector) : Float;
  function Func4(X : TVector) : Float;
  function Func5(X : TVector) : Float;
  function Func6(X : TVector) : Float;
  function Func7(X : TVector) : Float;
  function Func8(X : TVector) : Float;
  function Func9(X : TVector) : Float;
  function Func10(X : TVector) : Float;

implementation

  function Func1(X : TVector) : Float;
{ --------------------------------------------------------------------
  Example taken from 'Numerical Recipes'
  True minimum is at (-2.0, +/-0.89442719)
  -------------------------------------------------------------------- }
  var
    A, AA, B, BB : Float;
  begin
    A := X[2] * X[2] * (3.0 - X[1]) - X[1] * X[1] * (3.0 + X[1]);
    B := 2.0 + X[1];
    AA := Sqr(A);
    BB := Sqr(B);
    Func1 := 10.0 * AA + BB / (1.0 + BB);
  end;

  function Func2(X : TVector) : Float;
{ --------------------------------------------------------------------
  Example taken from 'Numerical Recipes'
  True minimum is at (0, 0, 0, 0), F = 1.0
  -------------------------------------------------------------------- }
  const
    NVAR = 4;
    RAD  = 0.3;
    AUG  = 2.0;
    Wid  : array[1..NVAR] of Float = (1.0, 3.0, 10.0, 30.0);
  var
    J : Integer;
    Q, R, Rad2, Sumd, Sumr : Float;
  begin
    Sumd := 0.0;
    Sumr := 0.0;
    Rad2 := Sqr(RAD);
    for J := 1 to NVAR do
      begin
        Q := X[J] * Wid[J];
        if Q >= 0 then R := Int(Q + 0.5) else R := Int(Q - 0.5);
        Sumr := Sumr + Sqr(Q);
        Sumd := Sumd + Sqr(Q - R);
      end;
    if Sumd > Rad2 then
      Func2 := 1.0 + Sumr * (1.0 + AUG)
    else
      Func2 := 1.0 + Sumr * (1.0 + AUG * Sumd / Rad2);
  end;

  function Func3(X : TVector) : Float;
{ --------------------------------------------------------------------
  Rosenbrock function.

  True minimum is at (1, 1), F = 0

  Ref: H. Rosenbrock, Comput. J., 1960, 3, 175
  -------------------------------------------------------------------- }
  begin
    Func3 := 100.0 * Sqr(X[2] - Sqr(X[1])) + Sqr(1.0 - X[1]);
  end;

  function Func4(X : TVector) : Float;
{ --------------------------------------------------------------------
  Powell function.

  True minimum is at (0, 0, 0, 0), F = 0
  Note that the hessian matrix is singular!

  Ref: M.J.D. Powell, Comput. J., 1962, 5, 147
  -------------------------------------------------------------------- }
  begin
    Func4 := Sqr(X[1] + 10.0 * X[2]) + 5.0 * Sqr(X[3] - X[4])
           + Sqr(Sqr(X[2] - 2.0 * X[3])) + 10.0 * Sqr(Sqr(X[1] - X[4]));
  end;

  function Func5(X : TVector) : Float;
{ --------------------------------------------------------------------
  Another Powell function.

  Multiple minima at x1 = x2 = x3 = +/- Sqrt(4*n+1), n integer, F = -3

  Ref: M.J.D. Powell, Comput. J., 1964, 7, 155

  NB: The original reference maximizes F. Here we shall minimize -F.
  -------------------------------------------------------------------- }
  begin
    Func5 := - 1.0 / (1.0 + Sqr(X[1] - X[2])) - Sin(PIDIV2 * X[2] * X[3])
             - Expo(- Sqr((X[1] + X[3]) / X[2] - 2.0));
  end;

  function Func6(X : TVector) : Float;
{ --------------------------------------------------------------------
  Fletcher & Powell function.

  True minimum is at (1, 0, 0), F = 0

  Ref: R. Fletcher & M.J.D. Powell, Comput. J., 1964, 7, 155
  -------------------------------------------------------------------- }
  var
    R, Theta : Float;
  begin
    R := Pythag(X[1], X[2]);
    Theta := ArcTan2(X[2], X[1]) / TWOPI;
    Func6 := 100.0 * (Sqr(X[3] - 10.0 * Theta) + Sqr(R - 1.0)) + Sqr(X[3]);
  end;

  function Func7(X : TVector) : Float;
{ --------------------------------------------------------------------
  Colville function (Extension of Rosenbrock function)

  True minimum is at (1, 1, 1, 1), F = 0

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  -------------------------------------------------------------------- }
  begin
    Func7 := 100.0 * Sqr(X[2] - Sqr(X[1])) + Sqr(1.0 - X[1]) +
              90.0 * Sqr(X[4] - Sqr(X[3])) + Sqr(1.0 - X[3]) +
              10.1 * ((Sqr(X[2] - 1.0) + Sqr(X[4] - 1.0))) +
              19.8 * (X[2] - 1.0) * (X[4] - 1.0);
  end;

  function Func8(X : TVector) : Float;
{ --------------------------------------------------------------------
  Griewank function.

  True minimum is at (0, 0), F = 0

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  -------------------------------------------------------------------- }
  begin
    Func8 := (Sqr(X[1]) + Sqr(X[2])) / 200.0
            - Cos(X[1]) * Cos(X[2] / SQRT2) + 1.0;
  end;

  function Func9(X : TVector) : Float;
{ --------------------------------------------------------------------
  Chichinadze function.

  True minimum is at (5.90133, 0.5), F = -43.3159

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  -------------------------------------------------------------------- }
  const
    FIVEPI   = 15.707963267948966193;   { 5 * Pi      }
    INVSQRT5 = 0.44721359549995793928;  { 1 / Sqrt(5) }
  begin
    Func9 := X[1] * (X[1] - 12.0) + 11.0
             + 10.0 * Cos(PIDIV2 * X[1]) + 8.0 * Sin(FIVEPI * X[1])
             - INVSQRT5 * Expo(- 0.5 * Sqr(X[2] - 0.5));
  end;

  function Func10(X : TVector) : Float;
{ --------------------------------------------------------------------
  Rastrigin function.

  True minimum is at (0, 0), F = -2

  Ref: R. J. Van Iwaarden, PhD Thesis, U. Denver, 1996
  -------------------------------------------------------------------- }
  begin
    Func10 := Sqr(X[1]) + Sqr(X[2]) - Cos(12.0 * X[1]) - Cos(18.0 * X[2]);
  end;

end.
