{ **********************************************************************
  *                        Program POLYROOT.PAS                        *
  *                            Version 1.0d                            *
  *                     (c) J. Debord, March 2004                      *
  **********************************************************************
  This program solves a polynomial equation by computing the
  eigenvalues of the companion matrix.

  The example polynomial is:
  x^6 - 21 x^5 + 175 x^4 - 735 x^3 + 1624 x^2 - 1764 x + 720

  for which the roots are 1,2 ... 6
  ********************************************************************** }

program polyroot;

uses
  fmath, matrices, eigen;

var
  Deg        : Integer;  { Degree of polynomial }
  Coef       : TVector;  { Coefficients of polynomial }
  X_Re, X_Im : TVector;  { Real and imaginary parts of roots }
  N          : Integer;  { Number of real roots }
  I          : Integer;  { Loop variable }

begin
  Deg := 6;

  DimVector(Coef, Deg);
  DimVector(X_Re, Deg);
  DimVector(X_Im, Deg);

  Coef[0] :=   720.0;
  Coef[1] := -1764.0;
  Coef[2] :=  1624.0;
  Coef[3] :=  -735.0;
  Coef[4] :=   175.0;
  Coef[5] :=   -21.0;
  Coef[6] :=     1.0;

  { Solve polynomial }
  N := RootPol(Coef, Deg, X_Re, X_Im);

  { Display results }
  Writeln;
  Writeln('Roots of polynomial:');
  Writeln;
  Writeln('  #                  Real part                          Imag. part');
  Writeln('-------------------------------------------------------------------------');
  for I := 1 to N do
    Writeln(I:3, X_Re[I]:35, X_Im[I]:35);
end.

