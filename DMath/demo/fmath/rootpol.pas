{ **********************************************************************
  *                         Program ROOTPOL.PAS                        *
  *                            Version 1.0d                            *
  *                      (c) J. Debord, July 2004                      *
  **********************************************************************
  This program solves a polynomial equation analytically, up to degree 4

  The example polynomial is:
  x^3 - x^2 - 8 x + 12

  for which the roots are -3, 2, 2

  Due to roundoff errors, the root (-2) is computed with a small
  imaginary part. A call to function SetRealRoots allows to overcome
  this problem.
  ********************************************************************** }

program RootPol;

uses
  FMath, Matrices, Polynom;

var
 Coef, Xr, Xi      : TVector;
 Deg, I, J, Nc, Nr : Integer;
 A                 : Float;

begin
  Deg := 3;

  DimVector(Coef, Deg);
  DimVector(Xr, Deg);
  DimVector(Xi, Deg);

  Coef[0] := 12;
  Coef[1] := -8;
  Coef[2] := -1;
  Coef[3] :=  1;

  Writeln;
  Writeln('Roots of polynomial:');
  for I := 0 to Deg do
    begin
      A := Abs(Coef[I]);
      if A <> 1.0 then Write(Round(A));
      if I > 0 then Write('X');
      if I > 1 then Write('^', I);
      if I < Deg then
        if Coef[I+1] > 0.0 then Write(' + ') else Write(' - ');
    end;
  Writeln;
  Writeln;

  { Solve polynomial. Nr is the number of real roots }
  Nr := RootPol3(Coef, Xr, Xi);

  { Set the small imaginary parts to zero }
  Nr := SetRealRoots(Deg, Xr, Xi, 1.0E-8);

  SortRoots(Deg, Xr, Xi);

  if Nr > 0 then
    begin
      Writeln(Nr, ' real root(s):');
      Writeln;

      for I := 1 to Nr do
        Writeln('X[', I, '] = ', Xr[I]);

      Writeln;
    end;

  Nc := Deg - Nr;
  if Nc > 0 then
    begin
      Writeln(Nc, ' complex root(s):');
      Writeln;

      for I := 1 to Nc do
        begin
          J := I + Nr;
          Write('X[', J, '] = ', Xr[J]);
          if Xi[J] > 0.0 then Write(' + ') else Write(' - ');
          Writeln(Abs(Xi[J]), ' * i');
        end;
    end;

  Readln;
end.
