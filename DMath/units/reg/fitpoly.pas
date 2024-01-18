{ **********************************************************************
  *                          Unit FITPOLY.PAS                          *
  *                            Version 1.3                             *
  *                   (c) J. Debord, September 2001                    *
  **********************************************************************
  This unit fits a polynomial :

                       y = b0 + b1.x + b2.x^2 + ...

  ********************************************************************** }

unit fitpoly;

{$F+}

interface

uses
  fmath, matrices, polynom, regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : TVector) : Float;

function FitModel(Method : Integer; X, Y, W : TVector; N : Integer;
                  B : TVector; V : TMatrix) : Integer;

procedure InitModel(CstPar : TVector);


implementation

var
  Deg : Integer;  { Degree of polynomial }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function.
    -------------------------------------------------------------------- }
  var
    Name, S : String;
    I : Integer;
  begin
    Name := 'y = b0 + b1.x';
    for I := 2 to Deg do
      begin
        Str(I, S);
        Name := Name + ' + b' + S + '.x^' + S;
      end;
    FuncName := Name;
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted.
    -------------------------------------------------------------------- }
  begin
    FirstParam := 0;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted.
    -------------------------------------------------------------------- }
  begin
    LastParam := Deg;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter.
    -------------------------------------------------------------------- }
  var
    S : String;
  begin
    Str(I, S);
    ParamName := 'b' + S;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X.
    B is the vector of parameters (coefficients of polynomial).
    -------------------------------------------------------------------- }
  begin
    RegFunc := Poly(X, B, Deg);
  end;

  function FitModel(Method : Integer; X, Y, W : TVector; N : Integer;
                    B : TVector; V : TMatrix) : Integer;
  { --------------------------------------------------------------------
    Fit of polynomial.
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
             V      = variance-covariance matrix of parameters
    -------------------------------------------------------------------- }
  begin
    case Method of
      0 : FitModel := PolFit(X, Y, N, Deg, B, V);
      1 : FitModel := WPolFit(X, Y, W, N, Deg, B, V);
    else
      FitModel := -1;
    end;
  end;

  procedure InitModel(CstPar : TVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit.
    --------------------------------------------------------------------
    CstPar[0] = Degree of polynomial
    -------------------------------------------------------------------- }
  var
    D : Integer;
  begin
    D := Round(CstPar[0]);
    if D > 1 then Deg := D;
  end;

begin
  Deg := 2;
end.
