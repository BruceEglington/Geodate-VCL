 (**********************************************************************
  *                          Unit FITMINT.PAS                          *
  *                            Version 1.0                             *
  *                    (c) J. Debord, January 2004                     *
  **********************************************************************
  This unit fits the Integrated Michaelis-Menten equation:

  y = H * {s0 - Km * W[s0 / Km * exp(s0 / Km - ks * e0 * t)]}

  y  = instrument response (e. g. absorbance) at time t
  H  = proportionality constant (e. g. extinction coefficient)
  s0 = initial substrate concentration
  Km = Michaelis constant
  ks = bimolecular rate constant = kcat / Km
  e0 = total enzyme concentration

  W is Lambert's function (reciprocal of x * exp(x))

  The independent variable x may be t, s0 or e0.

  H may be fitted or held constant.
 ***********************************************************************)

unit FitMint;

interface

uses
  FMath, Matrices, Regress;

function FuncName : String;

function FirstParam : Integer;

function LastParam : Integer;

function ParamName(I : Integer) : String;

function RegFunc(X : Float; B : TVector) : Float;

procedure DerivProc(X, Y : Float; B, D : TVector);

function FitModel(X, Y : TVector; N : Integer; B : TVector) : Integer;

procedure InitModel(CstPar : TVector);


implementation

type
  TVarInd = (Var_T, Var_S, Var_E);  { Identifies the independent variable }

var
  VarInd : TVarInd;  { Independent variable }
  S0     : Float;    { Initial substrate conc. (if constant) }
  E0     : Float;    { Total enzyme conc. (if constant) }
  Tmax   : Float;    { Incubation time (if constant) }
  H      : Float;    { Initial value of the proportionality constant }
  Fit_H  : Boolean;  { Indicates if H must be fitted }

  function FuncName : String;
  { --------------------------------------------------------------------
    Returns the name of the regression function
    -------------------------------------------------------------------- }
  begin
    FuncName := 'y = H * {s0 - Km * W [s0 / Km * exp(s0 / Km - ks * e0 * t)]}';
  end;

  function FirstParam : Integer;
  { --------------------------------------------------------------------
    Returns the index of the first parameter to be fitted
    (0 if H is fitted, 1 otherwise)
    -------------------------------------------------------------------- }
  begin
    if Fit_H then
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
      0 : ParamName := 'H';
      1 : ParamName := 'Km';
      2 : ParamName := 'ks';
    end;
  end;

  procedure Ident(VarInd : TVarInd; X : Float; var T, S, E : Float);
  { --------------------------------------------------------------------
    Identifies variables:
    T = time, S = substrate conc., E = enzyme conc.
    -------------------------------------------------------------------- }
  begin
    case VarInd of
      Var_T : begin T := X;    S := S0; E := E0; end;
      Var_S : begin T := Tmax; S := X;  E := E0; end;
      Var_E : begin T := Tmax; S := S0; E := X;  end;
    end;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  { --------------------------------------------------------------------
    Computes the regression function at point X
    B is the vector of parameters, such that :

    B[0] = H        B[1] = Km        B[2] = ks
    -------------------------------------------------------------------- }
  var
    T, S, E, R, Z : Float;
  begin
    Ident(VarInd, X, T, S, E);
    if not Fit_H then
      B[0] := H;
    R := S / B[1];          { s0 / Km }
    Z := R - B[2] * E * T;  { s0 / Km - ks * e0 * t }
    if Z < MINLOG then
      RegFunc := B[0] * S
    else
      RegFunc := B[0] * (S - B[1] * LambertW(R * Exp(Z), True, False));
  end;

  procedure DerivProc(X, Y : Float; B, D : TVector);
  { --------------------------------------------------------------------
    Computes the derivatives of the regression function at point (X,Y)
    with respect to the parameters B. The results are returned in D.
    D[I] contains the derivative with respect to the I-th parameter
    -------------------------------------------------------------------- }
  var
    T, S, E, L, R : Float;
  begin
    Ident(VarInd, X, T, S, E);
    if not Fit_H then
      B[0] := H;

    { dy/dH = y / H }
    D[0] := Y / B[0];

    { dy/dKm = (y / Km) * L / (1 + L)
      L = (s0 - y / H) / Km is Lambert's function }
    L := (S - D[0]) / B[1];
    R := L / (1.0 + L);
    D[1] := Y / B[1] * R;

    { dy/dks = H * Km * e0 * t * L / (1 + L) }
    D[2] := B[0] * B[1] * E * T * R;
  end;

  function FitModel(X, Y : TVector; N : Integer; B : TVector) : Integer;
  { --------------------------------------------------------------------
    Approximate fit of the integrated Michaelis equation
    by linear regression:

    p + Km * ln(1 - p / s0) = Km * ks * e0 * t    (p = y / H)
    --------------------------------------------------------------------
    Input :  X, Y = point coordinates (X = t, s0 or e0)
             N    = number of points
    Output : B    = estimated regression parameters
    -------------------------------------------------------------------- }
  var
    I      : Integer;  { Loop variables }
    P      : TVector;  { Product concentrations }
    XX, YY : TVector;  { Transformed variables }
    A      : TVector;  { Linear regression parameters }
    V      : TMatrix;  { Variance-covariance matrix }
  begin
    DimVector(P, N);
    DimVector(XX, N);
    DimVector(YY, N);
    DimVector(A, 1);
    DimMatrix(V, 1, 1);

    for I := 1 to N do
      P[I] := Y[I] / H;

    case VarInd of
      Var_T : for I := 1 to N do
                begin
                  XX[I] := Ln(1.0 - P[I] / S0) / X[I];
                  YY[I] := P[I] / X[I];
                end;
      Var_S : for I := 1 to N do
                begin
                  XX[I] := Ln(1.0 - P[I] / X[I]);
                  YY[I] := P[I];
                end;
      Var_E : for I := 1 to N do
                begin
                  XX[I] := Ln(1.0 - P[I] / S0) / X[I];
                  YY[I] := P[I] / E0;
                end;
    end;

    FitModel := LinFit(XX, YY, N, A, V);

    B[0] := H;
    B[1] := A[1];  { Km }
    case VarInd of
      Var_T : B[2] := A[0] / (B[1] * E0);         { ks }
      Var_S : B[2] := A[0] / (B[1] * E0 * Tmax);
      Var_E : B[2] := A[0] / (B[1] * Tmax);
    end;
  end;

  procedure InitModel(CstPar : TVector);
  { --------------------------------------------------------------------
    Initializes the global variables of the unit
    --------------------------------------------------------------------
    CstPar[0] = s0   (0 if s0 varies)
    CstPar[1] = e0   (0 if e0 varies)
    CstPar[2] = Tmax (0 if t varies)
    CstPar[3] = H
    CstPar[4] = 1 if H must be fitted
    -------------------------------------------------------------------- }
  begin
    S0    := CstPar[0];
    E0    := CstPar[1];
    Tmax  := CstPar[2];
    H     := CstPar[3];
    Fit_H := (CstPar[4] = 1);
    if S0 = 0.0 then
      VarInd := Var_S
    else if E0 = 0.0 then
      VarInd := Var_E
    else
      VarInd := Var_T;
  end;

begin
  S0     := 0.0;
  E0     := 1.00;
  Tmax   := 1.00;
  H      := 1.00;
  Fit_H  := True;
  VarInd := Var_S;
end.
