{ **********************************************************************
  *                          Unit FITFRAC.PAS                          *
  *                            Version 1.3                             *
  *                   (c) J. Debord, September 2001                    *
  **********************************************************************
  This unit fits a rational fraction :

                           p0 + p1.x + p2.x^2 + ...
                       y = ------------------------
                           1 + q1.x + q2.x^2 + ...

  ********************************************************************** }

unit fitfrac;

{$F+}

interface

uses
  fmath, matrices, polynom, regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : TVector) : Float;

procedure DerivProc(X, Y : Float; B, D : TVector);

function FitModel(Method : Integer; X, Y, W : TVector;
                  N : Integer; B : TVector) : Integer;

procedure InitModel(CstPar : TVector);


implementation

var
  Deg1     : Integer;  { Degree of numerator }
  Deg2     : Integer;  { Degree of denominator }
  ConsTerm : Boolean;  { Flags the presence of a constant term p0 }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  var
    Name, S : String;
    I : Integer;
  begin
    Name := 'y = (';
    if ConsTerm then
      Name := Name + 'p0 + ';
    Name := Name + 'p1.x';
    for I := 2 to Deg1 do
      begin
        Str(I, S);
        Name := Name + ' + p' + S + '.x^' + S;
      end;
    Name := Name + ') / (1 + q1.x';
    for I := (Deg1 + 2) to (Deg1 + Deg2) do
      begin
        Str(I - Deg1, S);
        Name := Name + ' + q' + S + '.x^' + S;
      end;
    Name := Name + ')';
    FuncName := Name;
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if there is a constant term p0, 1 otherwise)
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
    LastParam := Deg1 + Deg2;
  end;

  function ParamName(I : Integer) : String;
  { --------------------------------------------------------------------
    Returns the name of the I-th parameter
    -------------------------------------------------------------------- }
  var
    S : String;
  begin
    if I <= Deg1 then
      begin
        Str(I, S);
        ParamName := 'p' + S;
      end
    else
      begin
        Str(I - Deg1, S);
        ParamName := 'q' + S;
      end;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B[0] = p0
    B[1] = p1            B[2] = p2         ...

    B[Deg1 + 1] = q1     B[Deg1 + 2] = q2  ...
    -------------------------------------------------------------------- }
  begin
    RegFunc := RFrac(X, B, Deg1, Deg2);
  end;

  procedure DerivProc(X, Y : Float; B, D : TVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point (X,Y)
    with respect to the parameters B. The results are returned in D.
    D[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  var
    I : Integer;
    Den : Float;
  begin
    { Compute denominator (1 + q1.x + q2.x^2 + ...) }
    Den := 0.0;
    for I := (Deg1 + Deg2) downto Succ(Deg1) do
      Den := (Den + B[I]) * X;
    Den := 1.0 + Den;

    { dy/dp0 = 1 / (1 + q1.x + q2.x^2 + ...) }
    D[0] := 1.0 / Den;

    { dy/dpi = x^i / (1 + q1.x + q2.x^2 + ...) }
    for I := 1 to Deg1 do
      D[I] := D[I - 1] * X;

    { dy/dq1 = -x.y / (1 + q1.x + q2.x^2 + ...) }
    D[Deg1 + 1] := - X * Y / Den;

    { dy/dqi = -x^i.y / (1 + q1.x + q2.x^2 + ...) }
    for I := (Deg1 + 2) to (Deg1 + Deg2) do
      D[I] := D[I - 1] * X;
  end;

  function FitModel(Method : Integer; X, Y, W : TVector;
                    N : Integer; B : TVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of a rational fraction by linear regression:
    y = p0 + p1.x + p2.x^2 + ... - q1.(x.y) - q2.(x^2.y) - ...
    --------------------------------------------------------------------
    Input :  Method = 0 for unweighted regression, 1 for weighted
             X, Y   = point coordinates
             W      = weights
             N      = number of points
    Output : B      = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    I, J : Integer; { Loop variables }
    M : Integer;    { Index of last fitted parameter }
    U : TMatrix;    { Matrix of independent variables }
    V : TMatrix;    { Variance-covariance matrix }
  begin
    M := LastParam;
    DimMatrix(U, M, N);
    DimMatrix(V, M, M);

    for J := 1 to N do
      begin
        U[1,J] := X[J];
        for I := 2 to Deg1 do
          U[I,J] := U[I - 1,J] * X[J];
        U[Deg1 + 1,J] := - X[J] * Y[J];
        for I := (Deg1 + 2) to M do
          U[I,J] := U[I - 1,J] * X[J];
      end;

    case Method of
      0 : FitModel := MulFit(U, Y, N, M, ConsTerm, B, V);
      1 : FitModel := WMulFit(U, Y, W, N, M, ConsTerm, B, V);
    else
      FitModel := -1;
    end;

    if not ConsTerm then B[0] := 0.0;
  end;

  procedure InitModel(CstPar : TVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit
    --------------------------------------------------------------------
    CstPar[0] = Degree of numerator
    CstPar[1] = Degree of denominator
    CstPar[2] = 1 to include a constant term (p0)
    -------------------------------------------------------------------- }
  var
    D1, D2 : Integer;
  begin
    D1 := Round(CstPar[0]);
    D2 := Round(CstPar[1]);
    if D1 > 0 then Deg1 := D1;
    if D2 > 0 then Deg2 := D2;
    ConsTerm := (CstPar[2] = 1);
  end;

begin
  Deg1 := 1;
  Deg2 := 1;
  ConsTerm := True;
end.
