{ **********************************************************************
  *                         Program HESSIAN.PAS                        *
  *                            Version 1.5d                            *
  *                     (c) J. Debord, January 2002                    *
  **********************************************************************
  This program computes the Gradient (vector of first partial derivatives)
  and Hessian (matrix of second partial derivatives) of a function of
  several variables, using numerical differentiation.

  If f is a function of n variables defined by y = f(x(1), x(2),... x(n))
  the gradient vector G and the hessian matrix H are such that :

                  dy                         dýy
          G(i) = -----          H(i,j) = -----------          i = 1..n
                 dx(i)                   dx(i) dx(j)          j = 1..n

  The program assumes that the hessian matrix is symmetrical, i.e.
  H(j,i) = H(i,j). This occurs when the second partial derivatives are
  continuous, which is usually the case in applied mathematics.
  ********************************************************************** }

program hessian;

uses
  fmath, matrices, optim;

{ Define number of variables }
const
  Nvar = 3;

{ Global variables }
var
  X : TVector;  { Function variables }
  Y : Float;    { Function value }
  G : TVector;  { Gradient vector }
  H : TMatrix;  { Hessian matrix }

{ ----------------------------------------------------------------------
  Define your function here (cf the definition of type TFuncNVar in
  OPTIM.PAS).
  ---------------------------------------------------------------------- }

function Func(X : TVector) : Float;
begin
  Func := Exp(X[1] * X[2] * X[3]);
end;

procedure WriteResult;
{ Output results to screen }
var
  I, J : Integer;
begin
  WriteLn;
  Writeln('Variables :', #10);
  for I := 1 to Nvar do
    Writeln(X[I]:10:4);
  Writeln(#10, 'Function :', #10);
  Writeln(Y:10:4);
  Writeln(#10, 'Gradient :', #10);
  for I := 1 to Nvar do
    Writeln(G[I]:10:4);
  Writeln(#10, 'Hessian :', #10);
  for I := 1 to Nvar do
    begin
      for J := 1 to Nvar do
        Write(H[I,J]:10:4);
      Writeln;
    end;
end;

begin
  { Allocate arrays }
  DimVector(X, Nvar);
  DimVector(G, Nvar);
  DimMatrix(H, Nvar, Nvar);

  { Set the values of the variables for which
    gradient and hessian must be evaluated }
  X[1] := 0.5;
  X[2] := 1.0;
  X[3] := 2.0;

  { Evaluate function }
  Y := Func(X);

  { Compute Gradient and Hessian }
  NumHessGrad(Func, X, 1, Nvar, G, H);

  { Display results }
  WriteResult;
end.



