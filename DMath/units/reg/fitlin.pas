{ **********************************************************************
  *                          Unit FITLIN.PAS                           *
  *                            Version 1.1                             *
  *                   (c) J. Debord, September 2001                    *
  **********************************************************************
  This unit fits a linear function :

                               y = a + b.x

  ********************************************************************** }

unit fitlin;

{$F+}

interface

uses
  fmath, matrices, regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : TVector) : Float;

function FitModel(Method : Integer; X, Y, W : TVector; N : Integer;
                  B : TVector; V : TMatrix) : Integer;


implementation

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  begin
    FuncName := 'y = a + b.x';
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    FirstParam := 0;
  end;

  function LastParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the last parameter to be fitted
    -------------------------------------------------------------------- }
  begin
    LastParam := 1;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'a';
      1 : ParamName := 'b';
    end;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B[0] = a     B[1] = b
    -------------------------------------------------------------------- }
  begin
    RegFunc := B[0] + B[1] * X;
  end;

  function FitModel(Method : Integer; X, Y, W : TVector; N : Integer;
                    B : TVector; V : TMatrix) : Integer;
  { --------------------------------------------------------------------
    Fit the straight line
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
             V      = variance-covariance matrix of the parameters
    -------------------------------------------------------------------- }
  begin
    case Method of
      0 : FitModel := LinFit(X, Y, N, B, V);
      1 : FitModel := WLinFit(X, Y, W, N, B, V);
    else
      FitModel := -1;
    end;
  end;

end.
