{ **********************************************************************
  *                          Program NIST.PAS                          *
  *                            Version 1.8d                            *
  *                     (c) J. Debord, April 2002                      *
  **********************************************************************
  This programs evaluates the regression routines in MODELS.PAS with the
  reference data sets provided by the National Institute of Standards
  and Technology (NIST). These data are available on the Internet at:
  http://www.nist.gov/itl/div898/strd/general/dataarchive.html

  The following data sets are used:

  Norris         : Linear
  Longley        : Multilinear, 6 indep. variables
  Pontius        : Polynomial, degree=2
  Wampler 1 to 5 : Polynomial, degree=5, increasing noise level
  Filippelli     : Polynomial, degree=10
  Kirby 2        : Fraction, degree num=2 + const term, denom=2
  Thurber        : Fraction, degree num=3 + const term, denom=3
  Hahn 1         : Fraction, degree num=3 + const term, denom=3
  MGH 17         : Sum of 2 exponentials + const term
  Lanczos 1 to 3 : Sum of 3 exponentials, increasing noise level
  Box BOD        : Increasing exponential
  Misra 1a       : Increasing exponential
  Daniel-Wood    : Power
  Ratkowsky 2    : Logistic

  For each model, the following data are stored in file NIST.DAT,
  with all the significant digits reported by the NIST:

  * The reference parameter values with their standard deviations
  * The residual standard deviation
  * For linear and polynomial models: R^2 and F

  The program reads the input file, fits the models and writes the
  results in the output file NIST.OUT. The relative deviations between
  the reference values and the program values are computed and stored.

  According to the NIST, a good regression program should find at least
  4-5 significant digits of the reference values.

  Notes:

  1) Be sure to run the program at the highest precision level available
     to your compiler. In particular, the Filippelli test is passed only
     at the extended precision level ($DEFINE EXTENDEDREAL)

  2) The NIST does not provide reference data for the Michaelis, Hill and
     pKa models, nor for weighted regression.
  ********************************************************************** }

program nist;

uses
  fmath,
  matrices,
  optim,
  regress,
  pastring,
  models;

const
  NDATASETS = 19;  { Max. index of data set }

  DATANAME : array[0..NDATASETS] of String =
  ('Norris',
   'Longley',
   'Pontius',
   'Wampler 1',
   'Wampler 2',
   'Wampler 3',
   'Wampler 4',
   'Wampler 5',
   'Filippelli',
   'Kirby 2',
   'Thurber',
   'Hahn 1',
   'MGH 17',
   'Lanczos 1',
   'Lanczos 2',
   'Lanczos 3',
   'Box BOD',
   'Misra 1a',
   'Daniel-Wood',
   'Ratkowsky 2');

   MAXCSTPAR = 2;      { Max. index of constant parameter }
   MAXITER   = 5000;   { Maximal number of iterations }
   TOL       = 1E-08;  { Convergence criterion }

var
  Index        : Integer;  { Index of data set }
  N            : Integer;  { Number of observations }
  X            : TVector;  { Vector of independent variable }
  U            : TMatrix;  { Matrix of independent variable }
  Y            : TVector;  { Vector of dependent variable }
  B_ref        : TVector;  { Reference values for regression parameters }
  S_ref        : TVector;  { Reference values for parameter SD's }
  Sr_ref       : Float;    { Reference values for residual SD }
  R2_ref       : Float;    { Reference values for R^2 }
  F_ref        : Float;    { Reference values for F }
  Theta        : TVector;  { Variance param. for weighted reg., not used here }
  B            : TVector;  { regression parameters }
  B_min, B_max : TVector;  { Parameter bounds }
  V            : TMatrix;  { Variance-covariance matrix }
  Ycalc, S     : TVector;  { Estimated Y values and their SD's }
  RegTest      : TRegTest; { regression tests }
  ErrCode      : Integer;  { Error code }
  OutF         : Text;     { Output file }
  
  procedure ExtractModelData(ModelLine : String; var Model : Integer;
                             CstPar : TVector);
{ ----------------------------------------------------------------------
  Extracts the model data from the ModelLine string
  ---------------------------------------------------------------------- }
  var
    L : Integer;       { Length of string }
    P : Integer;       { Position of character in string }
    C : Char;          { Character }
    S1, S2 : String;   { Substrings }
    N1, N2 : Integer;  { Numeric parameters }
    Cst : Integer;     { Presence of constant term in model }
    Code : Integer;    { Error code }
  begin
    { Identify model }
    if Pos('Linear', ModelLine) > 0 then
      Model := REG_LIN
    else if Pos('Multilinear', ModelLine) > 0 then
      Model := REG_MULT
    else if Pos('Polynomial', ModelLine) > 0 then
      Model := REG_POL
    else if Pos('Rational', ModelLine) > 0 then
      Model := REG_FRAC
    else if Pos('Exponential', ModelLine) > 0 then
      Model := REG_EXPO
    else if Pos('Increasing exponential', ModelLine) > 0 then
      Model := REG_IEXPO
    else if Pos('Power', ModelLine) > 0 then
      Model := REG_POWER
    else if Pos('Logistic', ModelLine) > 0 then
      Model := REG_LOGIS;

    { Initialize substrings }
    S1 := ''; S2 := '';

    { Find the 1st comma }
    L := Length(ModelLine);
    P := 0;
    repeat
      Inc(P);
      C := ModelLine[P];
    until (C = ',') or (P = L);

    { Extract substring between 2 commas, or from comma to slash,
      or from comma to end of string. This substring corresponds
      to the first numeric parameter }
    if P < L then
      repeat
        Inc(P);
        C := ModelLine[P];
        if (C <> ',') and (C <> '/') then
          S1 := S1 + C;
      until (C = ',') or (C = '/') or (P = L);

    { Extract substring from '/' to 2nd comma. This substring
      corresponds to the second numeric parameter }
    if C = '/' then
      repeat
        Inc(P);
        C := ModelLine[P];
        if C <> ',' then
          S2 := S2 + C;
      until (C = ',') or (P = L);

    { Convert substrings to numeric parameters }
    Val(S1, N1, Code);
    Val(S2, N2, Code);

    { Check for constant term }
    if Pos('no constant term', ModelLine) > 0 then
      Cst := 0
    else if Pos('constant term', ModelLine) > 0 then
      Cst := 1;

    { Store constant parameters in CstPar }
    case Model of
      REG_MULT  : begin
                    CstPar[0] := N1;   { Number of indep. var. }
                    CstPar[1] := Cst;  { Presence of constant term }
                  end;
      REG_POL   : CstPar[0] := N1;     { Degree of polynomial }
      REG_FRAC  : begin
                    CstPar[0] := N1;   { Degree of numerator }
                    CstPar[1] := N2;   { Degree of denominator }
                    CstPar[2] := Cst;  { Presence of constant term }
                  end;
      REG_EXPO  : begin
                    CstPar[0] := N1;   { Number of exponentials }
                    CstPar[1] := Cst;  { Presence of constant term }
                  end;
      REG_LOGIS : CstPar[0] := Cst;    { Presence of constant term }
    end;
  end;

  procedure ReadInputFile(Index : Integer; var N : Integer;
                          var X : TVector; var U : TMatrix;
                          var Y, B_ref, S_ref : TVector;
                          var Sr_ref, R2_ref, F_ref : Float);
{ ----------------------------------------------------------------------
  Reads input file and initializes model
  ---------------------------------------------------------------------- }
  var
    InF : Text;          { Input file }
    S : String;          { Line of text }
    ModelLine : String;  { Line containing the model specifications }
    Model : Integer;     { Index of regression model }
    CstPar : TVector;    { Constant parameters }
    M : Integer;         { Number of parameters }
    I, K : Integer;      { Loop variables }
  begin
    { Open input file }
    Assign(InF, 'nist.dat');
    Reset(InF);

    { Read lines until title line is found }
    repeat
      ReadLn(InF, S);
    until Pos(DATANAME[Index], S) > 0;

    { Read model line and initialize model }
    ReadLn(InF, ModelLine);
    DimVector(CstPar, MAXCSTPAR);
    ExtractModelData(ModelLine, Model, CstPar);
    InitModel(Model, VAR_CONST, CstPar);
    M := LastParam;

    { Read number of points }
    Read(InF, N);

    { Dimension new arrays }
    DimVector(X, N);
    DimMatrix(U, M, N);
    DimVector(Y, N);
    DimVector(B_ref, M);
    DimVector(S_ref, M);

    { Read observations }
    if Model = REG_MULT then
      for K := 1 to N do
        begin
          for I := 1 to M do
            Read(InF, U[I,K]);
          Read(InF, Y[K]);
        end
    else
      for K := 1 to N do
        Read(InF, X[K], Y[K]);

    { Read reference parameters and their SD's }
    for K := FirstParam to LastParam do
      Read(InF, B_ref[K], S_ref[K]);

    { Read reference values for regression tests }
    Read(InF, Sr_ref);
    if Model in [REG_LIN, REG_MULT, REG_POL] then
      Read(InF, R2_ref, F_ref);

    { Close input file }
    Close(InF);
  end;

  procedure DimArrays(N : Integer; var Theta, B, B_min, B_max : TVector;
                      var V : TMatrix; var Ycalc, S : TVector);
{ ----------------------------------------------------------------------
  Dimensions new arrays
  ---------------------------------------------------------------------- }
  begin
    DimVector(Theta, LastVarParam);
    DimVector(B, LastParam);
    DimVector(B_min, LastParam);
    DimVector(B_max, LastParam);
    DimMatrix(V, LastParam, LastParam);
    DimVector(Ycalc, N);
    DimVector(S, N);
  end;

  procedure SetParamBounds(B_min, B_max : TVector);
{ ----------------------------------------------------------------------
  Sets parameter bounds
  ---------------------------------------------------------------------- }
  var
    I : Integer;
  begin
    for I := FirstParam to LastParam do
      begin
        B_min[I] := - 1.0E+1;
        B_max[I] :=   1.0E+4;
      end;
  end;

  procedure WriteResults(Index, ErrCode : Integer; B_ref, S_ref, B : TVector;
                         V : TMatrix; Sr_ref, R2_ref, F_ref : Float;
                         var RegTest : TRegTest);
{ ----------------------------------------------------------------------
  Writes results to output file
  ---------------------------------------------------------------------- }
  const
    HEADER    = '           Value                     Reference             Rel.error';
    LINEWIDTH = 73;
  var
    Line1, Line2 : String;  { Separating lines }
    S : Float;              { Standard deviation of a parameter }
    Sr : Float;             { Residual error }
    I : Integer;            { Loop variable }
    Err : Float;            { Relative error }
  begin
    Line1 := StrChar(LINEWIDTH, '-');
    Line2 := StrChar(LINEWIDTH, '=');

    WriteLn(OutF, Line2);
    WriteLn(OutF, 'Data set: ', DATANAME[Index]);
    WriteLn(OutF, 'Function: ', FuncName);
    WriteLn(OutF, Line1);

    case ErrCode of
      MAT_OK :
        begin
          WriteLn(OutF, 'Param', HEADER);
          WriteLn(OutF, Line1);

          for I := FirstParam to LastParam do
            begin
              Err := (B[I] - B_ref[I]) / B_ref[I];
              WriteLn(OutF, RFill(ParamName(I), 7),
                      B[I], '     ', B_ref[I], '     ', Err:10);
            end;

          WriteLn(OutF, Line1);
          WriteLn(OutF, 'SD   ', HEADER);
          WriteLn(OutF, Line1);

          for I := FirstParam to LastParam do
            begin
              S := Sqrt(V[I,I]);
              Write(OutF, RFill(ParamName(I), 7), S, '     ', S_ref[I]);
              if S_ref[I] > 0.0 then
                Write(OutF, '     ', ((S - S_ref[I]) / S_ref[I]):10);
              WriteLn(OutF);
            end;

          WriteLn(OutF, Line1);
          WriteLn(OutF, 'Test ', HEADER);
          WriteLn(OutF, Line1);

          with RegTest do
            begin
              Sr := Sqrt(Vr);
              Write(OutF, 's      ', Sr, '     ', Sr_ref);
              if Sr_ref > 0.0 then
                Write(OutF, '     ', ((Sr - Sr_ref) / Sr_ref):10);
              WriteLn(OutF);
              if Index < 9 then
                WriteLn(OutF, 'R^2    ', R2, '     ', R2_ref,
                  '     ', ((R2 - R2_ref) / R2_ref):10);
              if (Index < 9) and (F_ref < 1E38) then
                WriteLn(OutF, 'F      ', F, '     ', F_ref,
                  '     ', ((F - F_ref) / F_ref):10);
            end;
        end;
      OPT_SING       : WriteLn(OutF, 'Singular matrix');
      OPT_BIG_LAMBDA : WriteLn(OutF, 'Too high Marquardt''s parameter');
      OPT_NON_CONV   : WriteLn(OutF, 'Non - convergence');
    end;
    WriteLn(OutF, Line2);
  end;

begin
  Assign(OutF, 'nist.out');
  Rewrite(OutF);
  
{ ---------------------------------------------------------------
  With linear regression models, the best results are obtained
  with the Singular Value Decomposition algorithm.
  --------------------------------------------------------------- }
  SetRegAlgo(SVD);

  for Index := 0 to NDATASETS do
    begin
      WriteLn('Data set: ', DATANAME[Index], ' - Please wait...');

      { Read data set and dimension arrays }
      ReadInputFile(Index, N, X, U, Y, B_ref, S_ref, Sr_ref, R2_ref, F_ref);
      DimArrays(N, Theta, B, B_min, B_max, V, Ycalc, S);
      SetParamBounds(B_min, B_max);

    { ---------------------------------------------------------------
      With nonlinear regression models, Marquardt's method gives the
      best results except for data set 11 (Hahn 1), for which it does
      not converge. In this case, simplex or Metropolis-Hastings may
      be used to obtain an approximation to the parameters, which is
      further refined by Marquardt's method.
      --------------------------------------------------------------- }
      if Index = 11 then
        begin
          { Start with simplex }
          SetOptAlgo(NL_SIMP);
          ErrCode := WLSFit(X, U, Y, N, True, MAXITER, TOL, Theta,
                            B, B_min, B_max, V, Ycalc, S, RegTest);
          { Then switch to Marquardt }
          SetOptAlgo(NL_MARQ);
          ErrCode := WLSFit(X, U, Y, N, False, MAXITER, TOL, Theta,
                            B, B_min, B_max, V, Ycalc, S, RegTest);
        end
      else
        begin
          SetOptAlgo(NL_MARQ);
          ErrCode := WLSFit(X, U, Y, N, True, MAXITER, TOL, Theta,
                            B, B_min, B_max, V, Ycalc, S, RegTest);
        end;

      WriteResults(Index, ErrCode, B_ref, S_ref, B, V,
                   Sr_ref, R2_ref, F_ref, RegTest);
    end;

  Close(OutF);
  WriteLn('Results written in file nist.out');
end.
