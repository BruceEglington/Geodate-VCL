{ **********************************************************************
  *                         Program MINFUNC.PAS                        *
  *                             Version 2.0d                           *
  *                     (c) J. Debord, February 2003                   *
  **********************************************************************
  This program demonstrates the use of simulated annealing and BFGS
  methods for minimizing a function of several variables.

  BFGS (like Marquardt or Simplex methods) is a local optimization
  method which works only at the vicinity of a minimum. By contrast,
  simulated annealing can escape from such a local minimum and find
  the global one. This is obtained by accepting that the function
  increases sometimes during the process. The probability of acceptation
  is controlled by a parameter called temperature: a higher temperature
  means a higher probability of acceptation. At each temperature, the
  algorithm generates a random point from the current one (using a
  uniform distribution in our implementation). The new point is always
  accepted if the function decreases, otherwise the probability of
  acceptation depends on the temperature. The process is repeated for
  a given number of points, then the temperature is decreased. The
  algorithm stops when the parameters do not vary by more than a
  user-defined limit.

  Simulated annealing is used here to determine an approximation
  to the global minimum, which is then refined by BFGS.

  Note: It may be necessary to restart the program if the algorithm does
  not produce the expected results, since the random number generator
  is re-initialized at each start of the program.
  ********************************************************************** }

program minfunc;

uses
  fmath, matrices, optim, simopt, pastring;

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

  procedure Pause;
  begin
    WriteLn;
    Write('Press <Enter> to continue');
    ReadLn;
    WriteLn;
  end;

  procedure WriteResult(Method  : String;
                        Nvar    : Integer;
                        Hessian : Boolean;
                        X       : TVector;
                        H_inv   : TMatrix;
                        F_min   : Float);
{ --------------------------------------------------------------------
  Outputs results to screen.
  Hessian indicates if the Hessian matrix has been evaluated
  -------------------------------------------------------------------- }
  var
    I, J : Integer;
  begin
    WriteLn;
    WriteLn(Method, ' ', StrChar(44 - Length(Method), '*'));
    WriteLn('Coordinates of minimum         Function value');
    WriteLn;
    for I := 1 to Nvar do
      begin
        Write(X[I]:14:8);
        if I = 1 then Write(' ':17, F_min:14);
        WriteLn;
      end;

    if Hessian then
      begin
        WriteLn;
        WriteLn('Inverse Hessian matrix :');
        WriteLn;
        for I := 1 to Nvar do
          begin
            for J := 1 to Nvar do
              Write(H_inv[I,J]:14:8);
            WriteLn;
          end;
      end;
  end;

const
  NFUNC   = 10;  { Number of functions }
  MAXNVAR = 4;   { Maximum number of variables }

const
  FuncName : array[1..NFUNC] of String[70] =
  ('Numerical Recipes Example 1: Minimum at (-2.0, +/-0.89442719), F = 0 ',
   'Numerical Recipes Example 2: Minimum at (0, 0, 0, 0), F = 1          ',
   'Rosenbrock function: Minimum at (1, 1), F = 0                        ',
   'Powell function: Minimum at (0, 0, 0, 0), F = 0  (Singular Hessian)  ',
   'Another Powell function: Minimum at x1=x2=x3= +/- Sqrt(4*n+1), F = -3',
   'Fletcher & Powell function: Minimum at (1, 0, 0), F = 0              ',
   'Colville function: Minimum at (1, 1, 1, 1), F = 0                    ',
   'Griewank function: Minimum at (0, 0), F = 0                          ',
   'Chichinadze function: Minimum at (5.90133, 0.5), F = -43.3159        ',
   'Rastrigin function: Minimum at (0, 0), F = -2                        ');

const
  Nvar : array[1..NFUNC] of Integer =
         (2, 4, 2, 4, 3, 3, 4, 2, 2, 2);  { Number of variables }

var
  Func          : array[1..NFUNC] of TFuncNVar;  { Functions }
  X, Xmin, Xmax : TVector;                       { Variables and limit values }
  H_inv         : TMatrix;                       { Inverse hessian matrix }
  F_min         : Float;                         { Function value at minimum }
  ErrCode       : Integer;                       { Error code }
  I, J          : Integer;                       { Loop variables }

begin
  WriteLn;

  { Initialize function array }
  Func[ 1] := Func1;
  Func[ 2] := Func2;
  Func[ 3] := Func3;
  Func[ 4] := Func4;
  Func[ 5] := Func5;
  Func[ 6] := Func6;
  Func[ 7] := Func7;
  Func[ 8] := Func8;
  Func[ 9] := Func9;
  Func[10] := Func10;

  { Allocate arrays }
  DimVector(X, MAXNVAR);
  DimVector(Xmin, MAXNVAR);
  DimVector(Xmax, MAXNVAR);
  DimMatrix(H_inv, MAXNVAR, MAXNVAR);

  for I := 1 to NFUNC do
    begin
      { Initialize limits and pick starting point }
      for J := 1 to Nvar[I] do
        begin
          Xmin[J] := - 10.0;
          Xmax[J] := 10.0;
          X[J] := Xmin[J] + Random * (Xmax[J] - Xmin[J]);
        end;

      Writeln(FuncName[I]);

      { Approximate global minimum with simulated annealing }
      ErrCode := SimAnn(Func[I], X, Xmin, Xmax, 1, Nvar[I], 1000, 1.0E-4, F_min);

      if ErrCode = OPT_OK then
        WriteResult('Simulated Annealing', Nvar[I], False, X, H_inv, F_min);

      { Refine minimum with BFGS }
      ErrCode := BFGS(Func[I], NumGradient, X, 1, Nvar[I], 1000, 1.0E-10, F_min, H_inv);

      if ErrCode = OPT_OK then
        WriteResult('BFGS', Nvar[I], True, X, H_inv, F_min);

      Pause;
    end;
end.

