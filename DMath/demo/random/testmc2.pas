{ **********************************************************************
  *                         Program TESTMC2.PAS                        *
  *                             Version 1.2d                           *
  *                      (c) J. Debord, April 2002                     *
  **********************************************************************
  This program simulates a multi-lognormal distribution by Markov Chain
  Monte Carlo (MCMC) using the Metropolis-Hastings algorithm.

  Although MCMC is best used when there is no direct way to simulate
  the distribution, it is used here for demonstration purposes since
  its results can be compared to those of the direct method (program
  RANMULL.PAS).

  The pdf P(X) of the multi-lognormal distribution is such that:

                        P(X) = C * Exp(- F(X) / T)

  where C is a constant (not used in the simulation), T = 2 and

                                                           N
        F(X) = [Ln(X) - M0]' * V0^(-1) * [Ln(X) - M0] + 2 Sum Ln X(i)
                                                          i=1

        M0 and V0 are the transformed mean vector and
        variance-covariance matrix, such that:

          M0(i) = Ln M(i) - V0(i,i) / 2

          V0(i,j) = Ln[1 + V0(i,j) / (M(i) * M(j))]

        where M is the mean vector and V the variance-covariance
        matrix of the original log-normal distribution. N is the
        number of variables.

  The mean vector M and the variance-covariance matrix V are stored
  in a data file with the following structure:

    Line 1            : Title of study
    Line 2            : Number of variables (N), e.g. 2 for bi-lognormal
    Line 3 to (N + 2) : Means and standard deviations
    Next lines        : Correlation coefficients, in
                        lower triangular matrix form

  The file RANMULL.DAT is an example data file.

  The results are stored in the output file TESTMC2.TXT
  ********************************************************************** }

program testmc2;

uses
  fmath, matrices, mcmc;

const
  TEMP = 2.0;   { Temperature }

var
  Title   : String;   { Title of study }
  N       : Integer;  { Number of variables }
  M       : TVector;  { Mean vector of original distribution }
  V       : TMatrix;  { Variance-covariance matrix of original distribution }
  M0      : TVector;  { Transformed mean vector }
  V0      : TMatrix;  { Transformed variance-covariance matrix }
  V0_inv  : TMatrix;  { Inverse of transformed variance-covariance matrix }
  Xmat    : TMatrix;  { Matrix of simulated vectors }
  Msim    : TVector;  { Mean of simulated distribution }
  Vsim    : TMatrix;  { Variance-covariance matrix of simulated distribution }
  X_min   : TVector;  { Coordinates of the minimum of F(X)
                        = mode of simulated distribution }
  F_min   : Float;    { Value of F(X) at minimum }
  I       : Integer;  { Loop variable }
  ErrCode : Integer;  { Error code }


  procedure ReadParam(FileName : String; var Title : String; var N : Integer;
                      var M : TVector; var V : TMatrix);
  var
    F    : Text;     { Data file }
    I, J : Integer;  { Loop variables }
    S    : TVector;  { Standard deviations }
    R    : Float;    { Correlation coefficient }
  begin
    Assign(F, FileName);
    Reset(F);

    Readln(F, Title);
    Readln(F, N);

    DimVector(M, N);
    DimVector(S, N);
    DimMatrix(V, N, N);

    { Read means and standard deviations. Compute variances }
    for I := 1 to N do
      begin
        Read(F, M[I], S[I]);
        V[I,I] := Sqr(S[I]);
      end;

    { Read correlation coefficients and compute covariances }
    for I := 2 to N do
      for J := 1 to Pred(I) do
        begin
          Read(F, R);
          V[I,J] := R * S[I] * S[J];
          V[J,I] := V[I,J];
        end;

    { Compute the transformed mean vector and variance-covariance matrix }

    Close(F);
  end;

  function TransMat(M : TVector; V : TMatrix; N : Integer;
                    M0 : TVector; V0, V0_inv : TMatrix) : Integer;
  { Computes the transformed mean vector and variance-covariance matrix }
  var
    I, J : Integer;
  begin
    for I := 1 to N do
      begin
        V0[I,I] := Ln(V[I,I] / Sqr(M[I]) + 1.0);
        M0[I] := Ln(M[I]) - 0.5 * V0[I,I];
      end;

    for I := 2 to N do
      for J := 1 to Pred(I) do
        begin
          V0[I,J] := Ln(V[I,J] / (M[I] * M[J]) + 1.0);
          V0[J,I] := V0[I,J];
        end;

    { Compute the inverse of the transformed variance-covariance matrix }
    TransMat := InvMat(V0, 1, N, V0_inv);
  end;

  {$F+}
  function ObjFunc(X : TVector) : Float;
  { Computes the function F(X) }
  var
    Sum1, Sum2, Sum3 : Float;
    I, J             : Integer;
    L                : TVector;  { Ln(X) }
    D                : TVector;  { Differences Ln(X) - M0 }
  begin
    DimVector(L, N);
    DimVector(D, N);

    for I := 1 to N do
      begin
        if X[I] > 0.0 then L[I] := Ln(X[I]) else L[I] := MINLOG;
        D[I] := L[I] - M0[I];
      end;

    Sum1 := 0.0;
    for I := 1 to N do
      Sum1 := Sum1 + V0_inv[I,I] * Sqr(D[I]);

    Sum2 := 0.0;
    for I := 2 to N do
      for J := 1 to Pred(I) do
        Sum2 := Sum2 + V0_inv[I,J] * D[I] * D[J];

    Sum3 := 0.0;
    for I := 1 to N do
      Sum3 := Sum3 + L[I];

    ObjFunc := Sum1 + 2.0 * (Sum2 + Sum3);
  end;
  {$F-}

  procedure WriteResults(Title : String; M : TVector;
                         V : TMatrix; N : Integer);
  var
    I, J : Integer;
    S    : TVector;
    R    : Float;
  begin
    WriteLn;
    WriteLn(Title);
    WriteLn;

    WriteLn('      Mean      S.D.');
    WriteLn('--------------------');

    DimVector(S, N);
    for I := 1 to N do
      begin
        S[I] := Sqrt(V[I,I]);
        Writeln(M[I]:10:4, S[I]:10:4);
      end;

    WriteLn;
    WriteLn('Correlation matrix:');
    WriteLn;

    for I := 2 to N do
      begin
        for J := 1 to Pred(I) do
          begin
            R := V[I,J] / (S[I] * S[J]);
            Write(R:10:4);
          end;
        WriteLn;
      end;
  end;

  procedure WriteOutputFile(Title : String; Xmat : TMatrix; N : Integer);
  var
    F    : Text;
    I, J : Integer;
  begin
    Assign(F, 'testmc2.txt');
    Rewrite(F);

    WriteLn(F, Title);
    Write(F, ' Iter');
    for I := 1 to N do
      Write(F, '        X', I);
    WriteLn(F);

    for I := 1 to MH_SavedSim do
      begin
        Write(F, I:5);
        for J := 1 to N do
          Write(F, Xmat[J,I]:10:4);
        WriteLn(F);
      end;

    Close(F);
  end;

begin
  ReadParam('ranmull.dat', Title, N, M, V);

  DimVector(M0, N);
  DimMatrix(V0, N, N);
  DimMatrix(V0_inv, N, N);

  if TransMat(M, V, N, M0, V0, V0_inv) = MAT_SINGUL then
    begin
      WriteLn('Variance-covariance matrix is singular!');
      Exit;
    end;

  DimVector(Msim, N);
  DimVector(X_min, N);
  DimMatrix(Vsim, N, N);
  DimMatrix(Xmat, N, MH_SavedSim);

  { Initialize the mean vector and the variance-covariance matrix.
    For the sake of demonstration we start at a distance from the
    true mean and with enhanced standard deviations. }
  for I := 1 to N do
    begin
      Msim[I] := 2.0 * M[I];
      Vsim[I,I] := 2.0 * V[I,I];
    end;

  { Perform Metropolis-Hastings simulations }
  Write('Running. Please wait...');
  ErrCode := Hastings(ObjFunc, TEMP, Msim, Vsim,
                      1, N, Xmat, X_min, F_min);
  if ErrCode = 0 then
    begin
      WriteResults('Original distribution', M, V, N);
      WriteResults('Simulated distribution', Msim, Vsim, N);
      WriteOutputFile(Title, Xmat, N);
    end
  else
    WriteLn('Variance-covariance matrix is not positive definite!');
end.

