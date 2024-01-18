{ **********************************************************************
  *                         Unit FITIEXPO.PAS                          *
  *                            Version 1.4                             *
  *                    (c) J. Debord, February 2004                    *
  **********************************************************************
  This unit fits the increasing exponential :

                       y = Ymin + A.[1 - exp(-k.x)]

  ********************************************************************** }

unit fitiexpo;

interface

uses
  fmath, matrices, stat, regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : TVector) : Float;

procedure DerivProc(X : Float; B, D : TVector);

function FitModel(Method : Integer; X, Y, W : TVector;
                  N : Integer; B : TVector) : Integer;

procedure InitModel(CstPar : TVector);

implementation

var
  ConsTerm : Boolean;  { Flags the presence of a constant term Ymin }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  begin
    if ConsTerm then
      FuncName := 'y = Ymin + A[1 - exp(-k.x)]'
    else
      FuncName := 'y = A[1 - exp(-k.x)]';
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term Ymin, 1 otherwise)
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
    LastParam := 2;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'Ymin';
      1 : ParamName := 'A';
      2 : ParamName := 'k';
    end;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B[0] = Ymin     B[1] = A     B[2] = k
    -------------------------------------------------------------------- }
  begin
    if ConsTerm then
      RegFunc := B[0] + B[1] * (1.0 - Expo(- B[2] * X))
    else
      RegFunc := B[1] * (1.0 - Expo(- B[2] * X));
  end;

  procedure DerivProc(X : Float; B, D : TVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point X
    with respect to the parameters B. The results are returned in D.
    D[I] contains the derivative with respect to the I-th parameter.
    -------------------------------------------------------------------- }
  var
    E : Float;
  begin
    E := Expo(- B[2] * X);  { exp(-k.x) }
    D[0] := 1.0;            { dy/dYmin = 1 }
    D[1] := 1.0 - E;        { dy/dA = 1 - exp(-k.x) }
    D[2] := B[1] * X * E;   { dy/dk = A.x.exp(-k.x) }
  end;

  function FitModel(Method : Integer; X, Y, W : TVector;
                    N : Integer; B : TVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of the increasing exponential by linear regression:
    Ln(1 - z/A) = -k.x with z = y - Ymin
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    Z  : Float;         { y - Ymin }
    Y1 : TVector;       { Transformed ordinates }
    W1 : TVector;       { Weights }
    A  : TVector;       { Linear regression parameters }
    V  : TMatrix;       { Variance-covariance matrix }
    K  : Integer;       { Loop variable }
    ErrCode : Integer;  { Error code }
  begin
    DimVector(Y1, N);
    DimVector(W1, N);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);

    { Estimation of Ymin }
    if ConsTerm then
      B[0] := 0.9 * Min(Y, 1, N);

    { Estimation of A }
    B[1] := 1.1 * Max(Y, 1, N);

    for K := 1 to N do
      begin
        if ConsTerm then Z := Y[K] - B[0] else Z := Y[K];
        Y1[K] := Log(1.0 - Z / B[1]);
        W1[K] := Sqr(Z - B[1]);
        if Method = 1 then W1[K] := W1[K] * W[K];
      end;

    ErrCode := WLinFit(X, Y1, W1, N, A, V);

    if ErrCode = MAT_OK then
      B[2] := - A[1];

    FitModel := ErrCode;
  end;

  procedure InitModel(CstPar : TVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit
    --------------------------------------------------------------------
    CstPar[0] = 1 to include a constant term (Ymin)
    -------------------------------------------------------------------- }
  begin
    ConsTerm := (CstPar[0] = 1);
  end;

  begin
    ConsTerm := False;
  end.
