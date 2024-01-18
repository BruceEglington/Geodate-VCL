{ **********************************************************************
  *                           Program MIN1.PAS                         *
  *                             Version 1.2d                           *
  *                     (c) J. Debord, February 2003                   *
  **********************************************************************
  This program demonstrates the golden search method for finding the
  minimum of a function of one variable.
  ********************************************************************** }

program min1;

uses
  fmath, fspec, optim;

const
  NFUNC = 3;  { Number of functions }

{$F+}

function Func1(X : Float) : Float;
{ First function to be minimized. The minimum is at (Ln(2), -1/4) }
begin
  Func1 := Exp(- 2.0 * X) - Exp(- X);
end;

function Func2(X : Float) : Float;
{ Second function to be minimized.
  The minimum for X > 0 is at ~ (1.46163, 0.88560) }
begin
  Func2 := Gamma(X);
end;

function Func3(X : Float) : Float;
{ Third function to be minimized.
  The minimum for X > 0 is at (Exp(-1), Exp(-Exp(-1))) }
begin
  Func3 := Power(X, X);
end;

{$F-}

const
  FuncName : array[1..NFUNC] of String[18] =
  ('Exp(-2X) - Exp(-X)',
   'Gamma(X)          ',
   'X^X               ');

var
  Func                  : array[1..NFUNC] of TFunc;
  TrueMinX, TrueMinY    : array[1..NFUNC] of Float;
  A, B, Tol, Xmin, Ymin : Float;
  I, MaxIter            : Integer;

begin
  A := 0.1;
  B := 0.2;
  MaxIter := 1000;
  Tol := 1.0E-5;

  { Initialize function array }
  Func[1] := Func1;
  Func[2] := Func2;
  Func[3] := Func3;

  { Compute true minimum }
  TrueMinX[1] := LN2;
  TrueMinX[2] := 1.46163;
  TrueMinX[3] := Exp(-1.0);

  for I := 1 to NFUNC do
    TrueMinY[I] := Func[I](TrueMinX[I]);

  Writeln;
  Writeln('Minimization of functions of one variable');
  Writeln('-----------------------------------------');
  Writeln;

  for I := 1 to NFUNC do
    begin
      GoldSearch(Func[I], A, B, MaxIter, Tol, Xmin, Ymin);
      Writeln(FuncName[I], ' : Exact      : Xmin = ', TrueMinX[I]:8:5,
                           '   Ymin = ', TrueMinY[I]:8:5);
      Writeln('                     GoldSearch : Xmin = ', Xmin:8:5,
                           '   Ymin = ', Ymin:8:5);
      Writeln;
    end;
end.
