
{=============================================================================}
{=                                                                           =}
{= Function to compute a least squares fit using the                         =}
{= Modified Gram-Schmidt (MGS) method.                                       =}
{=                                                                           =}
{= Written by Mark Vaughan; last update 11-30-94, 11:27 GMT                  =}
{=    Converted (upgraded?) to Delphi   09-26-97                             =}
{=    Adapted to use the TPMath library 02-24-99                             =}
{=                                                                           =}
{= This code is released into the public domain.  all the usual disclaimers  =}
{= apply:  no fees, NO guarantees, use at your own risk, the user assumes    =}
{= all responsibility, etc.                                                  =}
{=                                                                           =}
{= The comments use extended ASCII characters; they'll look fine under DOS,  =}
{= but, depending on the font being used, they may appear scrambled under    =}
{= Windows.  If you're using Windows, the MSLineDraw font displayes all      =}
{= characters correctly.  This font is freely available at Microsoft's web   =}
{= site.                                                                     =}
{=                                                                           =}
{= The following is from page 152 of "Matrix Computations" by Golub          =}
{= and Van Loan (first edition, ISBN 0-8018-3010-9)                          =}
{=    "Algorithm 6.2-2: Modified Gram-Schmidt.  Given A î R[m x n] with      =}
{= rank(A) = n, the following algorithm computes the factorization A = QR    =}
{= where Q î R[m x n] has orthonormal columns and R î R[n x n] is upper      =}
{= triangular.  A is OVERWRITTEN by Q.                                       =}
{=                                                                           =}
{= for k = 1...n                                                             =}
{=                                                                           =}
{=             ÚÄÄ       ÄÄ¿«                                                =}
{=             ³ m         ³                                                 =}
{=   r[k,k] := ³ ä  a[i,k]ı³                                                 =}
{=             ³i=1        ³                                                 =}
{=             ÀÄÄ       ÄÄÙ                                                 =}
{=                                                                           =}
{=   for i = 1...m                                                           =}
{=     a[i,k] := a[i,k] / r[k,k]                                             =}
{=                                                                           =}
{=   for j = (k+1)...n                                                       =}
{=               ÚÄÄ             ÄÄ¿                                         =}
{=               ³ m               ³                                         =}
{=     r[k,j] := ³ ä  a[i,k]úa[i,j]³                                         =}
{=               ³i=1              ³                                         =}
{=               ÀÄÄ             ÄÄÙ                                         =}
{=     for i = 1...m do                                                      =}
{=       a[i,j] := a[i,j] - a[i,k]úr[k,j]                                    =}
{=                                                                           =}
{= This algorithm requires mnı flops."                                       =}
{=                                                                           =}
{= ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ =}
{=                                                                           =}
{= The functions we'll be fitting will (generally) be the powers of x, so    =}
{= that a given row, n, of A will be                                         =}
{=       ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿       =}
{=       ³ a[n,1] ³ a[n,2] ³ a[n,3] ³ a[n,4] ³ . . . . . ³ a[n, m+1] ³       =}
{=       ÃÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´       =}
{=       ³  0     ³        ³    2   ³    3   ³           ³      m    ³       =}
{=       ³ x  = 1 ³   x    ³   x    ³   x    ³ . . . . . ³     x     ³       =}
{=       ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ       =}
{=                                                                           =}
{= To calculate the least squares solution to Ax = b                         =}
{=                                                                           =}
{=   - use the Gram-Schmidt algorithm to factor A = QR                       =}
{=                                                    Â           Â      Â   =}
{=   - multiply both sides of the matrix equation by Q , so that Q QR = Q b  =}
{=                              Â                   Â               Â        =}
{=   - since Q is orthonormal, Q Q = I.  Therefore Q QR = IR = R = Q b .     =}
{=                  Â                                                        =}
{=   - solve R = Q b  by back-substitution.                                  =}
{=                                                                           =}
{=============================================================================}

{$IFDEF VER80}    {= Delphi 1.0 =}
  {$DEFINE UsingDelphi}
{$ENDIF}

{$IFDEF VER90}    {= Delphi 2.0 =}
  {$DEFINE UsingDelphi}
{$ENDIF}

{$IFDEF VER100}   {= Delphi 3.0 =}
  {$DEFINE UsingDelphi}
{$ENDIF}

{$IFDEF VER120}   {= Delphi 4.0 =}
  {$DEFINE UsingDelphi}
{$ENDIF}

{$IFDEF VER130}   {= Delphi 5.0 =}
  {$DEFINE UsingDelphi}
{$ENDIF}

{$IFDEF VER140}   {= Delphi 6.0 =}
  {$DEFINE UsingDelphi}
{$ENDIF}

unit MGS_LSQ;


INTERFACE

uses fMath, matrices;

{$IFNDEF UsingDelphi}
Type
  cardinal = word;
{$ENDIF}

Procedure mgsPolynomialFit (X_values  : TVector;
                            Y_values  : TVector;
                            DataDimension : integer;
                            degree : integer;
                            FitCoefficients : TVector);

Procedure CalcFitValues(startNDX, stopNDX, degree : cardinal;
                        VAR x, fit : TVector;
                        VAR C : TVector);

Function RMSerror(VAR Fit, Data : TVector;
                  startNDX, stopNDX : cardinal) : float;


IMPLEMENTATION

uses
{$IFDEF UsingDelphi}
  SysUtils,
{$ENDIF}
  polynom;


Procedure WriteMatrix(M : TMatrix;
                      rows, columns, precision : integer;
                      AFileName : string);
  Var
    F : System.Text;
    i, j : integer;
  BEGIN
    System.Assign(F, AFileName);
    rewrite(F);
    if (M = NIL) then
      writeln(F, 'NIL')
    else
      for i := 0 to rows do
        BEGIN
          for j := 0 to columns do
{$IFDEF UsingDelphi}
            write(F, '  ', FloatToStrF(M[i,j], ffExponent, precision, 2));
{$ELSE}
            write(F, '  ', M[i,j]);
{$ENDIF}
          writeln(F);
        END;
    System.close(F);
  END;

Procedure WriteVector(Var V : TVector;
                      NumElements, precision : integer;
                      AFileName : string);
  Var
    F : System.Text;
    i : integer;
  BEGIN
    System.Assign(F, AFileName);
    rewrite(F);
    for i := 0 to NumElements do
{$IFDEF UsingDelphi}
      writeln(F, ' ', FloatToStrF(V[i], ffExponent, precision, 2));
{$ELSE}
      writeln(F, ' ', V[i]);
{$ENDIF}
    System.close(F);
  END;

Procedure CalcFitValues(startNDX, stopNDX, degree : cardinal;
                        VAR x, fit : TVector;
                        VAR C : TVector);
  Var
    count : cardinal;
  BEGIN    {==CalcFitValues==}
    for count := startNDX to stopNDX do
      fit[count] := Poly(x[count], C, degree);
  END;     {==CalcFitValues==}

Function RMSerror(VAR Fit, Data : TVector;
                  startNDX, stopNDX : cardinal) : float;
  Var
    count : cardinal;
{$IFNDEF UsingDelphi}
    result1 : float;
{$ENDIF}
  BEGIN   {==RMSerror==}
    result := 0;
    for count := startNDX to stopNDX do
      result := result + sqr(Fit[count] - Data[count]);
    result := sqrt(result / (stopNDX-startNDX+1));
{$IFNDEF UsingDelphi}
    RMSerror := result;
{$ENDIF}
  END;    {==RMSerror==}

Procedure BuildPowersOfXMatrix( VAR X_values : TVector;
                                degree  : word;
                                NumRows : integer;
                                VAR M : TMatrix);
  Var
    count, columns  :  integer;
  BEGIN   {==PowersOfXMatrix==}
    for count := 0 to NumRows do
      BEGIN
        M[count,0] := 1;                {= x^0 =}
        M[count,1] := X_values[count];  {= x^1 =}
        M[count,2] := sqr(M[count,1]);  {= x^2 =}
        for columns := 3 to degree do
          M[count,columns] := X_values[count]*M[count,columns-1];
      END;
  END;    {==PowersOfXMatrix==}

Procedure mgsPolynomialFit (X_values  : TVector;
                            Y_values  : TVector;
                            DataDimension : integer;
                            degree : integer;
                            FitCoefficients : TVector);
  var
    rows,
    columns  :  word;
    A, R     :  TMatrix;
  BEGIN    {==mgsPolynomialFit==}
    columns := degree;
    rows := DataDimension;

    DimMatrix(A, rows, columns);
    BuildPowersOfXMatrix(X_values, degree, rows, A);
    DimMatrix(R, columns, columns);

    QR_Decomp(A, 0, rows, columns, R);
    QR_Solve(A, R, Y_values, 0, rows, columns, FitCoefficients);
  END;     {==mgsPolynomialFit==}

END.

