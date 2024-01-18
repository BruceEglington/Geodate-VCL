{ **********************************************************************
  *                          Unit MODELS.PAS                           *
  *                            Version 1.8                             *
  *                    (c) J. Debord, January 2004                     *
  **********************************************************************
                Library of regression and variance models
  ********************************************************************** }

unit Models;

interface

uses
  FMath,
  Matrices,
  Regress,
  FitLin,
  FitMult,
  FitPoly,
  FitFrac,
  FitExpo,
  FitIExpo,
  FitExLin,
  FitPower,
  FitMich,
  FitMint,
  FitHill,
  FitLogis,
  FitPka;

{ ---------------------------------------------------------------------
  Highest index of regression models
  --------------------------------------------------------------------- }
const
  MAXMODEL = 12;

{ ---------------------------------------------------------------------
  Highest index of variance models
  --------------------------------------------------------------------- }
const
  MAXVARMODEL = 5;

{ ---------------------------------------------------------------------
  Definition of regression models
  --------------------------------------------------------------------- }
const
  REG_LIN   = 0;   { Linear }
  REG_MULT  = 1;   { Multiple linear }
  REG_POL   = 2;   { Polynomial }
  REG_FRAC  = 3;   { Rational fraction }
  REG_EXPO  = 4;   { Sum of exponentials }
  REG_IEXPO = 5;   { Increasing exponential }
  REG_EXLIN = 6;   { Exponential + linear }
  REG_POWER = 7;   { Power }
  REG_MICH  = 8;   { Michaelis }
  REG_MINT  = 9;   { Integrated Michaelis }
  REG_HILL  = 10;  { Hill }
  REG_LOGIS = 11;  { Logistic }
  REG_PKA   = 12;  { Acid/Base titration curve }

{ ---------------------------------------------------------------------
  Definition of variance models
  --------------------------------------------------------------------- }
const
  VAR_CONST = 0;  { Constant }
  VAR_LIN   = 1;  { Linear }
  VAR_POL2  = 2;  { 2nd degree polynomial }
  VAR_POL3  = 3;  { 3rd degree polynomial }
  VAR_EXPO  = 4;  { Exponential }
  VAR_POWER = 5;  { Power }

{ ---------------------------------------------------------------------
  Names of regression models
  --------------------------------------------------------------------- }

const
  MODELNAME : array[0..MAXMODEL] of String =
{$IFDEF FRENCH}
  ('Lineaire',
   'Lineaire multiple',
   'Polynomial',
   'Fraction rationnelle',
   'Somme d''exponentielles',
   'Exponentielle croissante',
   'Exponentielle + lineaire',
   'Puissance',
   'Michaelis',
   'Michaelis integree',
   'Hill',
   'Logistique',
   'Titrage acide/base');
{$ELSE}
  ('Linear',
   'Multiple linear',
   'Polynomial',
   'Rational fraction',
   'Sum of exponentials',
   'Increasing exponential',
   'Exponential + linear',
   'Power',
   'Michaelis',
   'Integrated Michaelis',
   'Hill',
   'Logistic',
   'Acid/Base titration curve');
{$ENDIF}

{ ---------------------------------------------------------------------
  Names of variance models
  --------------------------------------------------------------------- }

const
  VARMODELNAME : array[0..MAXVARMODEL] of String =
{$IFDEF FRENCH}
  ('Constante',
   'Lineaire',
   'Polynome de degre 2',
   'Polynome de degre 3',
   'Exponentielle',
   'Puissance');
{$ELSE}
  ('Constant',
   'Linear',
   '2nd degree polynomial',
   '3rd degree polynomial',
   'Exponential',
   'Power');
{$ENDIF}

function FuncName : String;
{ --------------------------------------------------------------------
  Returns the name of the regression function
  -------------------------------------------------------------------- }

function FirstParam : Integer;
{ --------------------------------------------------------------------
  Returns the index of the first fitted parameter
  -------------------------------------------------------------------- }

function LastParam : Integer;
{ --------------------------------------------------------------------
  Returns the index of the last fitted parameter
  -------------------------------------------------------------------- }

function ParamName(I : Integer) : String;
{ --------------------------------------------------------------------
  Returns the name of the I-th fitted parameter
  -------------------------------------------------------------------- }

function RegFunc(X : Float; B : TVector) : Float;
{ --------------------------------------------------------------------
  Computes the regression function for one independent variable
  B is the vector of parameters
  -------------------------------------------------------------------- }

function RegFuncNVar(X, B : TVector) : Float;
{ --------------------------------------------------------------------
  Computes the regression function for several independent variables
  B is the vector of parameters
  -------------------------------------------------------------------- }

procedure DerivProc(RegFunc : TRegFunc; X, Y : Float; B, D : TVector);
{ --------------------------------------------------------------------
  Computes the derivatives of the regression function at point (X,Y)
  with respect to the parameters B. The results are returned in D.
  D^[I] contains the derivative with respect to the I-th parameter.
  -------------------------------------------------------------------- }

procedure InitModel(Reg_Model, Var_Model : Integer; CstPar : TVector);
{ --------------------------------------------------------------------
  Initializes the regression and variance models. Constant parameters
  (e.g. degree of polynomial) are passed in vector CstPar.
  -------------------------------------------------------------------- }

function WLSFit(X            : TVector;
                U            : TMatrix;
                Y            : TVector;
                N            : Integer;
                Init         : Boolean;
                MaxIter      : Integer;
                Tol          : Float;
                Theta, B     : TVector;
                B_min, B_max : TVector;
                V            : TMatrix;
                Ycalc, S     : TVector;
                var Test     : TRegTest) : Integer;
{ ----------------------------------------------------------------------
  Fits the regression function and computes the regression tests
  ----------------------------------------------------------------------
  Input :  X, U         = vector or matrix of independent variable(s)
           Y            = vector of dependent variable
           N            = number of observations
           Init         = TRUE to compute initial parameter estimates
                          FALSE to use the current values
           MaxIter      = maximum number of iterations
                          (if 0 the parameters will not be refined)
           Tol          = required parameter precision
           Theta        = variance parameters
           B            = initial parameters values
           B_min, B_max = parameter bounds
  --------------------------------------------------------------------
  Output : Theta = updated variance parameters
                   (residual variance stored in Theta^[0])
           B     = regression parameters
           V     = variance-covariance matrix
           Ycalc = estimated Y values
           S     = standard deviations of Y
           Test  = regression tests
  --------------------------------------------------------------------
  Possible results = OPT_OK         : no error
                     OPT_SING       : singular matrix
                     OPT_BIG_LAMBDA : too high Marquardt's parameter
                     OPT_NON_CONV   : non-convergence
  -------------------------------------------------------------------- }

function VarFuncName : String;
{ --------------------------------------------------------------------
  Returns the name of the variance function
  -------------------------------------------------------------------- }

function LastVarParam : Integer;
{ ----------------------------------------------------------------------
  Returns the index of the last variance parameter (upper bound of Theta)
  ---------------------------------------------------------------------- }

function VarFunc(Y : Float; Theta : TVector) : Float;
{ --------------------------------------------------------------------
  Computes the variance of an observation Y. The parameters are
  Theta^[1], Theta^[2],... The true variance is Theta^[0] * VarFunc,
  where Theta^[0] (equal to the residual variance Vr) is estimated by
  the regression program.
  -------------------------------------------------------------------- }

implementation

var
  RegModel : Integer;  { Index of regression model }
  VarModel : Integer;  { Index of variance model }

  function FuncName : String;
  begin
    case RegModel of
      REG_LIN   : FuncName := fitlin.FuncName;
      REG_MULT  : FuncName := fitmult.FuncName;
      REG_POL   : FuncName := fitpoly.FuncName;
      REG_FRAC  : FuncName := fitfrac.FuncName;
      REG_EXPO  : FuncName := fitexpo.FuncName;
      REG_IEXPO : FuncName := fitiexpo.FuncName;
      REG_EXLIN : FuncName := fitexlin.FuncName;
      REG_POWER : FuncName := fitpower.FuncName;
      REG_MICH  : FuncName := fitmich.FuncName;
      REG_MINT  : FuncName := fitmint.FuncName;
      REG_HILL  : FuncName := fithill.FuncName;
      REG_LOGIS : FuncName := fitlogis.FuncName;
      REG_PKA   : FuncName := fitpka.FuncName;
    end;
  end;

  function FirstParam : Integer;
  begin
    case RegModel of
      REG_LIN   : FirstParam := fitlin.FirstParam;
      REG_MULT  : FirstParam := fitmult.FirstParam;
      REG_POL   : FirstParam := fitpoly.FirstParam;
      REG_FRAC  : FirstParam := fitfrac.FirstParam;
      REG_EXPO  : FirstParam := fitexpo.FirstParam;
      REG_IEXPO : FirstParam := fitiexpo.FirstParam;
      REG_EXLIN : FirstParam := fitexlin.FirstParam;
      REG_POWER : FirstParam := fitpower.FirstParam;
      REG_MICH  : FirstParam := fitmich.FirstParam;
      REG_MINT  : FirstParam := fitmint.FirstParam;
      REG_HILL  : FirstParam := fithill.FirstParam;
      REG_LOGIS : FirstParam := fitlogis.FirstParam;
      REG_PKA   : FirstParam := fitpka.FirstParam;
    end;
  end;

  function LastParam : Integer;
  begin
    case RegModel of
      REG_LIN   : LastParam := fitlin.LastParam;
      REG_MULT  : LastParam := fitmult.LastParam;
      REG_POL   : LastParam := fitpoly.LastParam;
      REG_FRAC  : LastParam := fitfrac.LastParam;
      REG_EXPO  : LastParam := fitexpo.LastParam;
      REG_IEXPO : LastParam := fitiexpo.LastParam;
      REG_EXLIN : LastParam := fitexlin.LastParam;
      REG_POWER : LastParam := fitpower.LastParam;
      REG_MICH  : LastParam := fitmich.LastParam;
      REG_MINT  : LastParam := fitmint.LastParam;
      REG_HILL  : LastParam := fithill.LastParam;
      REG_LOGIS : LastParam := fitlogis.LastParam;
      REG_PKA   : LastParam := fitpka.LastParam;
    end;
  end;

  function ParamName(I : Integer) : String;
  begin
    case RegModel of
      REG_LIN   : ParamName := fitlin.ParamName(I);
      REG_MULT  : ParamName := fitmult.ParamName(I);
      REG_POL   : ParamName := fitpoly.ParamName(I);
      REG_FRAC  : ParamName := fitfrac.ParamName(I);
      REG_EXPO  : ParamName := fitexpo.ParamName(I);
      REG_IEXPO : ParamName := fitiexpo.ParamName(I);
      REG_EXLIN : ParamName := fitexlin.ParamName(I);
      REG_POWER : ParamName := fitpower.ParamName(I);
      REG_MICH  : ParamName := fitmich.ParamName(I);
      REG_MINT  : ParamName := fitmint.ParamName(I);
      REG_HILL  : ParamName := fithill.ParamName(I);
      REG_LOGIS : ParamName := fitlogis.ParamName(I);
      REG_PKA   : ParamName := fitpka.ParamName(I);
    end;
  end;

  function RegFunc(X : Float; B : TVector) : Float;
  begin
    case RegModel of
      REG_LIN   : RegFunc := fitlin.RegFunc(X, B);
      REG_POL   : RegFunc := fitpoly.RegFunc(X, B);
      REG_FRAC  : RegFunc := fitfrac.RegFunc(X, B);
      REG_EXPO  : RegFunc := fitexpo.RegFunc(X, B);
      REG_IEXPO : RegFunc := fitiexpo.RegFunc(X, B);
      REG_EXLIN : RegFunc := fitexlin.RegFunc(X, B);
      REG_POWER : RegFunc := fitpower.RegFunc(X, B);
      REG_MICH  : RegFunc := fitmich.RegFunc(X, B);
      REG_MINT  : RegFunc := fitmint.RegFunc(X, B);
      REG_HILL  : RegFunc := fithill.RegFunc(X, B);
      REG_LOGIS : RegFunc := fitlogis.RegFunc(X, B);
      REG_PKA   : RegFunc := fitpka.RegFunc(X, B);
    end;
  end;

  function RegFuncNVar(X, B : TVector) : Float;
  begin
    case RegModel of
      REG_MULT : RegFuncNVar := fitmult.RegFunc(X, B);
    end;
  end;

  procedure DerivProc(RegFunc : TRegFunc; X, Y : Float; B, D : TVector);
  begin
    case RegModel of
      REG_FRAC  : fitfrac.DerivProc(X, Y, B, D);
      REG_EXPO  : fitexpo.DerivProc(X, B, D);
      REG_IEXPO : fitiexpo.DerivProc(X, B, D);
      REG_EXLIN : fitexlin.DerivProc(X, B, D);
      REG_POWER : fitpower.DerivProc(X, Y, B, D);
      REG_MICH  : fitmich.DerivProc(X, Y, B, D);
      REG_MINT  : fitmint.DerivProc(X, Y, B, D);
      REG_HILL  : fithill.DerivProc(X, Y, B, D);
      REG_LOGIS : fitlogis.DerivProc(X, B, D);
      REG_PKA   : fitpka.DerivProc(X, B, D);
    else
      NumDeriv(RegFunc, X, Y, B, D);
    end;
  end;

  procedure InitModel(Reg_Model, Var_Model : Integer; CstPar : TVector);
  begin
    RegModel := Reg_Model;
    VarModel := Var_Model;
    case RegModel of
      REG_MULT  : fitmult.InitModel(CstPar);
      REG_POL   : fitpoly.InitModel(CstPar);
      REG_FRAC  : fitfrac.InitModel(CstPar);
      REG_EXPO  : fitexpo.InitModel(CstPar);
      REG_IEXPO : fitiexpo.InitModel(CstPar);
      REG_MINT  : fitmint.InitModel(CstPar);
      REG_LOGIS : fitlogis.InitModel(CstPar);
    end;
  end;

  function FitModel(Method : Integer;
                    X      : TVector;
                    U      : TMatrix;
                    Y, W   : TVector;
                    N      : Integer;
                    B      : TVector;
                    V      : TMatrix) : Integer;
{ --------------------------------------------------------------------
  Fits the regression model by unweighted linear least squares. For
  nonlinear models, this is only an approximate fit, to be refined by
  the nonlinear regression procedure WLSFit
  --------------------------------------------------------------------
  Input :  Method = 0 for unweighted regression, 1 for weighted
           X, U   = vector or matrix of independent variable(s)
           Y      = vector of dependent variable
           W      = weights
           N      = number of observations
  --------------------------------------------------------------------
  Output : B      = estimated regression parameters
           V      = unscaled variance-covariance matrix (for linear
                    and polynomial models only). The true matrix will
                    be Vr * V, where Vr is the residual variance.
  --------------------------------------------------------------------
  The function returns 0 if no error occurred
  -------------------------------------------------------------------- }
  begin
    case RegModel of
      REG_LIN   : FitModel := fitlin.FitModel(Method, X, Y, W, N, B, V);
      REG_MULT  : FitModel := fitmult.FitModel(Method, U, Y, W, N, B, V);
      REG_POL   : FitModel := fitpoly.FitModel(Method, X, Y, W, N, B, V);
      REG_FRAC  : FitModel := fitfrac.FitModel(Method, X, Y, W, N, B);
      REG_EXPO  : FitModel := fitexpo.FitModel(Method, X, Y, W, N, B);
      REG_IEXPO : FitModel := fitiexpo.FitModel(Method, X, Y, W, N, B);
      REG_EXLIN : FitModel := fitexlin.FitModel(X, Y, N, B);
      REG_POWER : FitModel := fitpower.FitModel(Method, X, Y, W, N, B);
      REG_MICH  : FitModel := fitmich.FitModel(Method, X, Y, W, N, B);
      REG_MINT  : FitModel := fitmint.FitModel(X, Y, N, B);
      REG_HILL  : FitModel := fithill.FitModel(Method, X, Y, W, N, B);
      REG_LOGIS : FitModel := fitlogis.FitModel(Method, X, Y, W, N, B);
      REG_PKA   : FitModel := fitpka.FitModel(X, Y, N, B);
    end;
  end;

  function WLSFit(X            : TVector;
                  U            : TMatrix;
                  Y            : TVector;
                  N            : Integer;
                  Init         : Boolean;
                  MaxIter      : Integer;
                  Tol          : Float;
                  Theta, B     : TVector;
                  B_min, B_max : TVector;
                  V            : TMatrix;
                  Ycalc, S     : TVector;
                  var Test     : TRegTest) : Integer;
  var
    Method  : Integer;  { regression method }
    W       : TVector;  { Weights }
    Xk      : TVector;  { Vector of variables for observation k }
    Sr      : Float;    { Residual standard deviation }
    ErrCode : Integer;  { Error code }
    K       : Integer;  { Loop variable }
  begin
    DimVector(W, N);
    DimVector(Xk, LastParam);

    { Determine regression method }
    if VarModel = VAR_CONST then Method := 0 else Method := 1;

    { Compute weights if necessary }
    if Method = 1 then
      for K := 1 to N do
        W[K] := 1.0 / VarFunc(Y[K], Theta);

    { Compute initial parameter estimates if necessary }
    if Init then
      ErrCode := FitModel(Method, X, U, Y, W, N, B, V)
    else
      ErrCode := 0;

    { Refine parameters if necessary }
    if not(RegModel in [REG_LIN, REG_MULT, REG_POL]) and
       (MaxIter > 0) and (ErrCode = 0) then
      if VarModel = VAR_CONST then
        ErrCode := NLFit(RegFunc, DerivProc,
                         X, Y, N, FirstParam, LastParam,
                         MaxIter, Tol, B, B_min, B_max, V)
      else
        ErrCode := WNLFit(RegFunc, DerivProc,
                          X, Y, W, N, FirstParam, LastParam,
                          MaxIter, Tol, B, B_min, B_max, V);

    if ErrCode = 0 then
      begin
        { Estimate Y values }
        if RegModel = REG_MULT then
          for K := 1 to N do
            begin
              CopyVectorFromCol(Xk, U, FirstParam, LastParam, K);
              Ycalc[K] := RegFuncNVar(Xk, B);
            end
        else
          for K := 1 to N do
            Ycalc[K] := RegFunc(X[K], B);

        { Compute regression tests and update variance-covariance matrix }
        if VarModel = VAR_CONST then
          RegTest(Y, Ycalc, N, FirstParam, LastParam, V, Test)
        else
          WRegTest(Y, Ycalc, W, N, FirstParam, LastParam, V, Test);

        { Store residual variance in Theta[0] }
        Theta[0] := Test.Vr;

        { Compute standard deviations }
        Sr := Sqrt(Test.Vr);
        for K := 1 to N do
          S[K] := Sr;
        if VarModel <> VAR_CONST then
          for K := 1 to N do
            S[K] := S[K] / Sqrt(W[K]);
      end;

    WLSFit := ErrCode;
  end;

  function VarFuncName : String;
  begin
    case VarModel of
      VAR_CONST : VarFuncName := 'v = e0';
      VAR_LIN   : VarFuncName := 'v = e0.(1 + e1.y)';
      VAR_POL2  : VarFuncName := 'v = e0.(1 + e1.y + e2.y^2)';
      VAR_POL3  : VarFuncName := 'v = e0.(1 + e1.y + e2.y^2 + e3.y^3)';
      VAR_EXPO  : VarFuncName := 'v = e0.exp(e1.y)';
      VAR_POWER : VarFuncName := 'v = e0.y^e1';
    end;
  end;

  function VarFunc(Y : Float; Theta : TVector) : Float;
  begin
    case VarModel of
      VAR_CONST : VarFunc := 1.0;
      VAR_LIN   : VarFunc := 1.0 + Theta[1] * Y;
      VAR_POL2  : VarFunc := 1.0 + Y * (Theta[1] + Theta[2] * Y);
      VAR_POL3  : VarFunc := 1.0 + Y * (Theta[1] + Y * (Theta[2] + Theta[3] * Y));
      VAR_EXPO  : VarFunc := Exp(Theta[1] * Y);
      VAR_POWER : VarFunc := Power(Y, Theta[1]);
    end;
  end;

  function LastVarParam : Integer;
  begin
    case VarModel of
      VAR_CONST : LastVarParam := 0;
      VAR_LIN   : LastVarParam := 1;
      VAR_POL2  : LastVarParam := 2;
      VAR_POL3  : LastVarParam := 3;
      VAR_EXPO  : LastVarParam := 1;
      VAR_POWER : LastVarParam := 1;
    end;
  end;

begin
  RegModel := 0;
  VarModel := 0;
end.
