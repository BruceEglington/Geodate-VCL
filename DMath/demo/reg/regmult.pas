{ **********************************************************************
  *                        Program REGMULT.PAS                         *
  *                           Version 1.1d                             *
  *                     (c) J. Debord, July 2002                       *
  **********************************************************************
  This program performs a weighted multiple linear least squares fit :

                     y = b0 + b1 * x1 + b2 * x2 + ...

  The following parameters are passed on the command line :

    1st parameter = name of input file (default extension = .DAT)
    2nd parameter = 1 if the equation includes a constant term b0

  Input files are ASCII files with the following structure :

    Line 1     : Title of study
    Line 2     : Number of variables (must be >= 2 here !)
    Next lines : Names of variables x1, x2, ..., y
    Next line  : Number of observations (must be > number of variables !)

    The next lines contain the coordinates (x1, x2, ..., y) of the
    observations (1 observation by line). The coordinates must be
    separated by spaces or tabulations.

  The file INHIB.DAT is an example of data relating the inhibition of an
  enzyme to the physico-chemical properties of the inhibitors (J. DEBORD,
  P. N'DIAYE, J. C. BOLLINGER et al, J. Enzyme Inhib., 1997, 12, 13-26).
  The program parameters are : INHIB 1

  The program may be executed from Turbo Pascal's integrated environment,
  in which case the parameters are entered through the "Parameters" option
  of the menu, or from DOS (after compilation into an executable file),
  in which case the parameters are entered on the command line (e.g.
  REGMULT INHIB 1).
  ********************************************************************** }

program regmult;

uses
  fmath, matrices, regress, models, pastring;

var
  InFName      : String;      { Name of input file }
  Title        : String;      { Title of study }
  XName        : TStrVector;  { Names of independent variables }
  YName        : String;      { Name of dependent variable }
  N            : Integer;     { Number of observations }
  X            : TMatrix;     { Matrix of independent variables }
  Y            : TVector;     { Vector of dependent variable }
  Z            : TVector;     { Vector of independent variable (not used here) }
  Ycalc        : TVector;     { Expected Y values }
  S            : TVector;     { Standard deviations of Y values }
  CstPar       : TVector;     { Constant parameters }
  B            : TVector;     { Regression parameters }
  B_min, B_max : TVector;     { Parameter bounds (not used, but must be
                                declared in order to use the WLSFit routine ) }
  V            : TMatrix;     { Variance-covariance matrix of regression parameters }
  Theta        : TVector;     { Variance parameters }
  RegTest      : TRegTest;    { Regression tests }
  ErrCode      : Integer;     { Error code }

  procedure ReadCmdLine(var InFName : String; var CstPar : TVector);
{ ----------------------------------------------------------------------
  Reads command line parameters. Stores constant parameters in CstPar,
  such that :

    CstPar[0] = Number of independent variables
                (this one is set by ReadInputFile)
    CstPar[1] = 1 to include a constant term (b0)

  The contents of CstPar are defined in the unit FITMULT.PAS,
  in the subdirectory REG of the TP Math units directory.
  ---------------------------------------------------------------------- }
  var
    ErrCode, I : Integer;
  begin
    DimVector(CstPar, 1);

    { Name of input file }
    InFName := ParamStr(1);
    if Pos('.', InFName) = 0 then InFName := InFName + '.dat';

    { Presence of constant term }
    Val(ParamStr(2), I, ErrCode);
    if ErrCode <> 0 then I := 0;
    CstPar[1] := I;
  end;

  function ReadInputFile(InFName   : String;
                         var Title : String;
                         var XName : TStrVector;
                         var YName : String;
                         var N     : Integer;
                         var X     : TMatrix;
                         var Y     : TVector;
                         CstPar    : TVector) : Integer;
  var
    InF  : Text;     { Input file }
    Nvar : Integer;  { Nb of independent variables }
    I, K : Integer;  { Loop variables }
  begin
    Assign(InF, InFName);
    Reset(InF);

    ReadLn(InF, Title);
    ReadLn(InF, Nvar);  { Total number of variables }

    if Nvar < 2 then
      begin
        WriteLn('Data file must contain at least 2 variables !');
        ReadInputFile := - 1;
        Exit;
      end;

    Nvar := Pred(Nvar);
    DimVector(XName, Nvar);

    for I := 1 to Nvar do
      ReadLn(InF, XName[I]);

    ReadLn(InF, YName);
    ReadLn(InF, N);

    DimMatrix(X, Nvar, N);
    DimVector(Y, N);

    for K := 1 to N do
      begin
        for I := 1 to Nvar do
          Read(InF, X[I][K]);
        Read(InF, Y[K]);
      end;

    Close(InF);
    CstPar[0] := Nvar;
    ReadInputFile := 0;
  end;

  procedure WriteOutputFile(InFName, Title         : String;
                            XName                  : TStrVector;
                            YName                  : String;
                            N                      : Integer;
                            Y, CstPar, Ycalc, S, B : TVector;
                            V                      : TMatrix;
                            Test                   : TRegTest);
  var
    OutFName : String;   { Name of output file }
    OutF     : Text;     { Output file }
    Line1,
    Line2    : String;   { Separating lines }
    Nvar     : Integer;  { Nb of independent variables }
    Delta    : Float;    { Residual }
    Sr       : Float;    { Residual error }
    SB       : TVector;  { Standard deviations of parameters }
    T        : TVector;  { Student's t }
    Prob     : TVector;  { Probabilities }
    I, K     : Integer;  { Loop variables }
  begin
    Nvar := Round(CstPar[0]);

    DimVector(SB, LastParam);
    DimVector(T, LastParam);
    DimVector(Prob, LastParam);

    K := Pos('.', InFName);
    OutFName := Copy(InFName, 1, Pred(K)) + '.out';
    Assign(OutF, OutFName);
    Rewrite(OutF);

    Line1 := StrChar(73, '-');
    Line2 := StrChar(73, '=');

    WriteLn(OutF, Line2);
    WriteLn(OutF, 'Data file  : ', InFName);
    WriteLn(OutF, 'Study name : ', Title);
    for I := 1 to Nvar do
      WriteLn(OutF, 'x', I:1, '         : ', XName[I]);
    WriteLn(OutF, 'y          : ', YName);
    WriteLn(OutF, 'Function   : ', FuncName);

    { Perform tests on parameters }
    ParamTest(B, V, N, FirstParam, LastParam, SB, T, Prob);

    WriteLn(OutF, Line1);
    WriteLn(OutF, 'Parameter    Est.value         Std.dev.        t Student       Prob(>|t|)');
    WriteLn(OutF, Line1);

    for I := FirstParam to LastParam do
      if SB[I] > 0.0 then
        WriteLn(OutF, ParamName(I):5, B[I]:17:8, SB[I]:17:8, T[I]:17:2, Prob[I]:17:4)
      else
        WriteLn(OutF, ParamName(I):5, B[I]:17:8);

    WriteLn(OutF, Line1);
    WriteLn(OutF, 'Number of observations            : n   = ', N:5);

    with Test do
      begin
        Sr := Sqrt(Vr);
        WriteLn(OutF, 'Residual error                    : s   = ', Sr:10:8);
        if (R2 >= 0.0) and (R2 <= 1.0) then
          WriteLn(OutF, 'Coefficient of determination      : r2  = ', R2:10:8);
        if (R2a >= 0.0) and (R2a <= 1.0) then
          WriteLn(OutF, 'Adjusted coeff. of determination  : r2a = ', R2a:10:8);
        Write(OutF, 'Variance ratio (explained/resid.) : F   = ', F:10:4);
        WriteLn(OutF, '    Prob(>F) = ', Prob:6:4);
      end;

    WriteLn(OutF, Line1);
    WriteLn(OutF, '  i        Y obs.       Y calc.      Residual      Std.dev.      Std.res.');
    WriteLn(OutF, Line1);

    for K := 1 to N do
      begin
        Delta := Y[K] - Ycalc[K];
        WriteLn(OutF, K:3, Y[K]:14:4, Ycalc[K]:14:4, Delta:14:4, S[K]:14:4, (Delta / S[K]):14:4);
      end;
    WriteLn(OutF, Line2);

    Close(OutF);
    WriteLn('Results written to file ', OutFName);
  end;

{ *************************** Main program ***************************** }

begin
  { Read command line parameters }
  ReadCmdLine(InFName, CstPar);

  { Read input file }
  if ReadInputFile(InFName, Title, XName, YName, N, X, Y, CstPar) <> 0 then
    begin
      WriteLn('Error reading file ', InFName);
      Halt;
    end;

  { Initialize regression and variance models.
    See MODELS.PAS in the REG subdirectory for a list of available models }
  InitModel(REG_MULT,
            VAR_CONST,  { Here we use a constant variance }
            CstPar);

  { Set the regression algorithm which must be GAUSS_JORDAN or SVD.
    The default algorithm is SVD. Comment off the following line if
    you wish to change the algorithm. }

  { SetRegAlgo(GAUSS_JORDAN); }

  { Dimension arrays.
    Note: the variance parameters Theta[1]..Theta[LastVarParam]
    must be supplied if we use a non-constant variance model }
  DimVector(Theta, LastVarParam);
  DimVector(B, LastParam);
  DimMatrix(V, LastParam, LastParam);
  DimVector(Ycalc, N);
  DimVector(S, N);

  { Perform regression. The numbers 1 and 0.1 denote the maximal number
    of iterations and the tolerance on the parameters. They are purely
    formal values here since the multiple linear regression does not use
    an iterative minimization algorithm. }
  ErrCode := WLSFit(Z, X, Y, N, True, 1, 0.1, Theta, B,
                    B_min, B_max, V, Ycalc, S, RegTest);

  { Write results }
  case ErrCode of
    MAT_OK       : WriteOutputFile(InFName, Title, XName, YName,
                                   N, Y, CstPar, Ycalc, S, B, V, RegTest);
    MAT_SINGUL   : WriteLn('Singular matrix !');
    MAT_NON_CONV : WriteLn('Non-convergence of SVD algorithm !');
  end;
end.
