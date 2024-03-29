{ **********************************************************************
  *                          Unit FITMULT.PAS                          *
  *                            Version 1.2                             *
  *                   (c) J. Debord, September 2001                    *
  **********************************************************************
  This unit fits the multiple linear equation:

                       y = b0 + b1.x1 + b2.x2 + ...

  ********************************************************************** }

unit fitmult;

{$F+}

interface

uses
  fmath, matrices, regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X, B : TVector) : Float;

function FitModel(Method : Integer; X : TMatrix; Y, W : TVector;
                  N : Integer; B : TVector; V : TMatrix) : Integer;

procedure InitModel(CstPar : TVector);


implementation

var
  Nvar     : Integer;  { Number of independent variables }
  ConsTerm : Boolean;  { Flags the presence of a constant term b0 }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  var
    Name, S : String;
    I : Integer;
  begin
    Name := 'y = ';
    if ConsTerm then
      Name := Name + 'b0 + ';
    Name := Name + 'b1.x1';
    for I := 2 to Nvar do
      begin
        Str(I, S);
        Name := Name + ' + b' + S + '.x' + S;
      end;
    FuncName := Name;
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    if ConsTerm then
      FirstParam := 0
    else
      FirstParam := 1;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    LastParam := Nvar;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  var
    S : String;
  begin
    Str(I, S);
    ParamName := 'b' + S;
  end;

  function RegFunc(X, B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at observation X
    B is the vector of parameters.
    -------------------------------------------------------------------- }
  var
    I : Integer;
    Y : Float;
  begin
    if ConsTerm then Y := B[0] else Y := 0.0;
    for I := 1 to Nvar do
      Y := Y + B[I] * X[I];
    RegFunc := Y;
  end;

  function FitModel(Method : Integer; X : TMatrix; Y, W : TVector;
                    N : Integer; B : TVector; V : TMatrix) : Integer;
  { --------------------------------------------------------------------
    Multiple linear regression
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X      = matrix of independent variables
             Y      = vector of dependent variable
             W      = vector of weights
             N      = number of observations
    Output : B      = estimated regression parameters
             V      = variance-covariance matrix of parameters
    -------------------------------------------------------------------- }
  begin
    case Method of
      0 : FitModel := MulFit(X, Y, N, Nvar, ConsTerm, B, V);
      1 : FitModel := WMulFit(X, Y, W, N, Nvar, ConsTerm, B, V);
    else
      FitModel := -1;
    end;
  end;

  procedure InitModel(CstPar : TVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit
    --------------------------------------------------------------------
    CstPar[0] = number of independent variables
    CstPar[1] = 1 to include a constant term (b0)
    -------------------------------------------------------------------- }
  begin
    Nvar := Round(CstPar[0]);
    ConsTerm := (CstPar[1] = 1);
  end;

begin
  Nvar := 2;
  ConsTerm := True;
end.
