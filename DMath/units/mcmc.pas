{ **********************************************************************
  *                             Unit MCMC.PAS                          *
  *                             Version 1.4d                           *
  *                     (c) J. Debord, February 2003                   *
  **********************************************************************
  Simulation by Markov Chain Monte Carlo (MCMC) with the
  Metropolis-Hastings algorithm.

  This algorithm simulates the probability density function (pdf) of a
  vector X. The pdf P(X) is written as:

                       P(X) = C * Exp(- F(X) / T)

  Simulating P by the Metropolis-Hastings algorithm is equivalent to
  minimizing F by simulated annealing at the constant temperature T.
  The constant C is not used in the simulation.

  The series of random vectors generated during the annealing step
  constitutes a Markov chain which tends towards the pdf to be simulated.

  It is possible to run several cycles of the algorithm.
  The variance-covariance matrix of the simulated distribution is
  re-evaluated at the end of each cycle and used for the next cycle.
  ********************************************************************** }

unit mcmc;

interface

uses
  fmath, matrices, randnum;


{ **********************************************************************
  Metropolis-Hastings parameters
  ********************************************************************** }

var
  MH_NCycles  : Integer;  { Number of cycles }
  MH_MaxSim   : Integer;  { Max nb of simulations at each cycle }
  MH_SavedSim : Integer;  { Nb of simulations to be saved }

{ **********************************************************************
  Simulation routine
  ********************************************************************** }

  function Hastings(Func           : TFuncNVar;
                    T              : Float;
                    X              : TVector;
                    V              : TMatrix;
                    Lbound, Ubound : Integer;
                    Xmat           : TMatrix;
                    X_min          : TVector;
                    var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Simulation of a probability density function by the
  Metropolis-Hastings algorithm
  ----------------------------------------------------------------------
  Input parameters :  Func   = Function such that the pdf is
                                 P(X) = C * Exp(- Func(X) / T)
                      T      = Temperature
                      X      = Initial mean vector
                      V      = Initial variance-covariance matrix
                      Lbound,
                      Ubound = Indices of first and last variables
  ----------------------------------------------------------------------
  Output parameters : Xmat  = Matrix of simulated vectors, stored
                              columnwise, i.e.
                              Xmat[Lbound..Ubound, 1..MH_SavedSim]
                      X     = Mean of distribution
                      V     = Variance-covariance matrix of distribution
                      X_min = Coordinates of minimum of F(X)
                                (mode of the distribution)
                      F_min = Value of F(X) at minimum
  ----------------------------------------------------------------------
  Possible results : MAT_OK     : No error
                     MAT_NOT_PD : The variance-covariance matrix
                                  is not positive definite
  ---------------------------------------------------------------------- }

implementation

  function CalcSD(V              : TMatrix;
                  Lbound, Ubound : Integer;
                  S              : TVector) : Integer;
{ ----------------------------------------------------------------------
  Computes the standard deviations for independent random numbers
  from the variance-covariance matrix.
  ---------------------------------------------------------------------- }
  var
    I, ErrCode : Integer;
  begin
    I := LBound;
    ErrCode := 0;
    repeat
      if V[I,I] > 0.0 then
        S[I] := Sqrt(V[I,I])
      else
        ErrCode := MAT_NOT_PD;
      Inc(I);
    until (ErrCode <> 0) or (I > Ubound);
    CalcSD := ErrCode;
  end;

  function Accept(DeltaF, T : Float) : Boolean;
{ ----------------------------------------------------------------------
  Checks if a variation DeltaF of the function at temperature T is
  acceptable.
  ---------------------------------------------------------------------- }
  var
    X : Float;
  begin
    if DeltaF < 0.0 then
      Accept := True
    else
      begin
        X := DeltaF / T;
        if X > MAXLOG then  { Exp(-X) ~ 0 }
          Accept := False
        else
          Accept := (Exp(- X) > RanMar);
      end;
  end;

  function HastingsCycle(Func           : TFuncNVar;
                         T              : Float;
                         X              : TVector;
                         V              : TMatrix;
                         Lbound, Ubound : Integer;
                         Indep          : Boolean;
                         Xmat           : TMatrix;
                         X_min          : TVector;
                         var F_min      : Float) : Integer;
{ ----------------------------------------------------------------------
  Performs one cycle of the Metropolis-Hastings algorithm
  ---------------------------------------------------------------------- }
  var
    F, F1         : Float;    { Function values }
    DeltaF        : Float;    { Variation of function }
    Sum           : Float;    { Statistical sum }
    X1            : TVector;  { New coordinates }
    L             : TMatrix;  { Cholesky factor of var-cov matrix }
    S             : TVector;  { Standard deviations }
    I, J, K       : Integer;  { Loop variables }
    Iter          : Integer;  { Iteration count }
    FirstSavedSim : Integer;  { Index of first simulation to be saved }
    ErrCode       : Integer;  { Error code }
  begin
    { Dimension arrays }
    DimVector(S, Ubound);
    DimVector(X1, Ubound);
    DimMatrix(L, Ubound, Ubound);

    { Compute SD's or Cholesky factor from var-cov matrix }
    if Indep then
      ErrCode := CalcSD(V, Lbound, Ubound, S)
    else
      ErrCode := Cholesky(V, Lbound, Ubound, L);

    HastingsCycle := ErrCode;
    if ErrCode = MAT_NOT_PD then Exit;

    { Compute initial function value }
    F := Func(X);

    { Perform MH_MaxSim simulations at constant temperature }
    FirstSavedSim := MH_MaxSim - MH_SavedSim + 1;
    Iter := 1;
    K := 1;

    repeat
      { Generate new vector }
      if Indep then
        RanMultIndep(X, S, Lbound, Ubound, X1)
      else
        RanMult(X, L, Lbound, Ubound, X1);

      { Compute new function value }
      F1 := Func(X1);
      DeltaF := F1 - F;

      { Check for acceptance }
      if Accept(DeltaF, T) then
        begin
          if IsConsole then Write('.');  { Only for command-line programs }

          CopyVector(X, X1, Lbound, Ubound);
          if Iter >= FirstSavedSim then
            begin
              { Save simulated vector into column K of matrix Xmat }
              CopyColFromVector(Xmat, X1, Lbound, Ubound, K);
              Inc(K);
            end;

          if F1 < F_min then
            begin
              { Update minimum }
              CopyVector(X_min, X1, Lbound, Ubound);
              F_min := F1;
            end;

          F := F1;
          Inc(Iter);
        end;
    until Iter > MH_MaxSim;

    { Update mean vector }
    for I := Lbound to Ubound do
      begin
        Sum := 0.0;
        for K := 1 to MH_SavedSim do
          Sum := Sum + Xmat[I,K];
        X[I] := Sum / MH_SavedSim;
      end;

    { Update variance-covariance matrix }
    for I := Lbound to Ubound do
      for J := I to Ubound do
        begin
          Sum := 0.0;
          for K := 1 to MH_SavedSim do
            Sum := Sum + (Xmat[I,K] - X[I]) * (Xmat[J,K] - X[J]);
          V[I,J] := Sum / MH_SavedSim;
        end;
    for I := Succ(Lbound) to Ubound do
      for J := Lbound to Pred(I) do
        V[I,J] := V[J,I];
  end;

  function Hastings(Func           : TFuncNVar;
                    T              : Float;
                    X              : TVector;
                    V              : TMatrix;
                    Lbound, Ubound : Integer;
                    Xmat           : TMatrix;
                    X_min          : TVector;
                    var F_min      : Float) : Integer;
  var
    K, ErrCode : Integer;
    Indep : Boolean;
  begin
    { Initialize the Marsaglia random number generator
      using the standard Pascal generator }
    Randomize;
    RMarIn(Random(10000), Random(10000));

    K := 1;
    Indep := True;
    F_min := MAXNUM;

    repeat
      ErrCode := HastingsCycle(Func, T, X, V, Lbound, Ubound,
                               Indep, Xmat, X_min, F_min);
      Indep := False;
      Inc(K);
    until (ErrCode <> 0) or (K > MH_NCycles);

    Hastings := ErrCode;
  end;

begin
  MH_NCycles  := 5;
  MH_MaxSim   := 100;
  MH_SavedSim := 100;
end.