{ **********************************************************************
  *                         Unit FITLOGIS.PAS                          *
  *                           Version 1.6d                             *
  *                   (c) J. Debord, February 2004                     *
  **********************************************************************
  This unit fits the logistic function :

                                      B - A
                        y = A + -----------------
                                1 + exp(-a.x + b)

  and the generalized logistic function :
  
                                        B - A
                        y = A + ---------------------
                                [1 + exp(-a.x + b)]^n
  ********************************************************************** }

unit fitlogis;

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
  ConsTerm : Boolean;  { Flags the presence of a constant term A }
  General  : Boolean;  { Selects the generalized function }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function.
    -------------------------------------------------------------------- }
  var
    S : String;
  begin
    if ConsTerm then
      S := 'y = A + (B - A) / [1 + exp(-a.x + b)]'
    else
      S := 'y = B / [1 + exp(-a.x + b)]';
    if General then
      S := S + '^n';
    FuncName := S;
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term A, 1 otherwise)
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
    if General then
      LastParam := 4
    else
      LastParam := 3;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter.
    -------------------------------------------------------------------- }
  begin
    case I of
      0 : ParamName := 'A';
      1 : ParamName := 'B';
      2 : ParamName := 'a';
      3 : ParamName := 'b';
      4 : ParamName := 'n';
    end;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X.
    B is the vector of parameters, such that :
    B[0] = A     B[1] = B     B[2] = a     B[3] = b     B[4] = n
    -------------------------------------------------------------------- }
  var
    D : Float;
  begin
    D := 1.0 + Expo(- B[2] * X + B[3]);
    if General then D := Power(D, B[4]);
    if ConsTerm then
      RegFunc := B[0] + (B[1] - B[0]) / D
    else
      RegFunc := B[1] / D;
  end;

  procedure DerivProc(X : Float; B, D : TVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point X
    with respect to the parameters B. The results are returned in D.
    D[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  var
    C, Q, R, S : Float;
  begin
    C := B[0] - B[1];              { A - B }
    Q := Expo(- B[2] * X + B[3]);  { exp(-ax+b) }
    R := 1.0 / (1.0 + Q);          { 1 / [1 + exp(-ax+b)] }
    if General then
      S := Power(R, B[4])          { 1 / [1 + exp(-ax+b)]^n }
    else
      S := R;

    D[0] := 1.0 - S;  { dy/dA = 1 - 1 / [1 + exp(-ax+b)]^n }
    D[1] := S;        { dy/dB = 1 / [1 + exp(-ax+b)]^n }

    { dy/db = n.(A-B).exp(-ax+b) / [1 + exp(-ax+b)]^(n+1) }
    D[3] := C * Q * R * S;
    if General then D[3] := B[4] * D[3];

    { dy/da = n.(B-A).x.exp(-ax+b) / [1 + exp(-ax+b)]^(n+1) }
    D[2] := - X * D[3];

    { dy/dn = (A-B).Ln[1+exp(-ax+b)] / [1 + exp(-ax+b)]^n }
    if General then
      D[4] := - C * Log(R) * S;
  end;

  procedure SortPoints(X, Y : TVector; N : Integer);
  { ----------------------------------------------------------------------
    Sort points by increasing X values
    ---------------------------------------------------------------------- }
  var
    I, J, K : Integer;
    A : Float;
  begin
    for I := 1 to Pred(N) do
      begin
        K := I;
        A := X[I];
        for J := Succ(I) to N do
          if X[J] < A then
            begin
              K := J;
              A := X[J];
            end;
        Swap(X[I], X[K]);
        Swap(Y[I], Y[K]);
      end;
  end;

  function FitModel(Method : Integer; X, Y, W : TVector;
                    N : Integer; B : TVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of a logistic function by linear regression:
    Ln[(B - A)/(y - A) - 1] = -ax + b
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    XX : TVector;       { Transformed X coordinates }
    YY : TVector;       { Transformed Y coordinates }
    WW : TVector;       { Weights }
    A : TVector;        { Linear regression parameters }
    V : TMatrix;        { Variance-covariance matrix }
    P : Integer;        { Number of points for linear regression }
    K : Integer;        { Loop variable }
    ErrCode : Integer;  { Error code }
    D : Float;          { B - A }
  begin
    DimVector(XX, N);
    DimVector(YY, N);
    DimVector(WW, N);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);

    SortPoints(X, Y, N);

    if ConsTerm then
      B[0] := Y[1]
    else
      B[0] := 0.0;
    B[1] := Y[N];

    P := 0;
    D := B[1] - B[0];
    for K := 1 to N do
      if (X[K] > X[1]) and (X[K] < X[N]) then
        begin
          Inc(P);
          XX[P] := X[K];
          YY[P] := Log(D / (Y[K] - B[0]) - 1.0);
          WW[P] := Sqr((Y[K] - B[0]) * (Y[K] - B[1]) / D);
          if Method = 1 then WW[P] := WW[P] * W[K];
        end;

    ErrCode := WLinFit(XX, YY, WW, P, A, V);

    if ErrCode = MAT_OK then
      begin
        B[2] := - A[1];
        B[3] := A[0];
      end;

    if General then B[4] := 1.0;

    FitModel := ErrCode;
  end;

  procedure InitModel(CstPar : TVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit.
    --------------------------------------------------------------------
    CstPar[0] = 1 to include a constant term (A)
    CstPar[1] = 1 to select the generalized logistic
    -------------------------------------------------------------------- }
  begin
    ConsTerm := (CstPar[0] = 1);
    General  := (CstPar[1] = 1);
  end;

begin
  ConsTerm := False;
  General := False;
end.
