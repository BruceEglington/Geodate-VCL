{ **********************************************************************
  *                            Unit STAT.PAS                           *
  *                            Version 1.5d                            *
  *                    (c) J. Debord, February 2003                    *
  **********************************************************************
                           Statistical functions
  ********************************************************************** }

unit stat;

interface

uses
  fmath, randnum, matrices;

{ ----------------------------------------------------------------------
  Common input parameters : X       : Vector of statistical variable
                            Lbound,
                            Ubound  : Indices of first and last
                                              elements of X
                            W       : Vector of weights
  ---------------------------------------------------------------------- }

procedure QSort(X : TVector; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Sorts the elements of vector X in increasing order (quick sort)
  ---------------------------------------------------------------------- }

procedure DQSort(X : TVector; Lbound, Ubound : Integer);
{ ----------------------------------------------------------------------
  Sorts the elements of vector X in decreasing order (quick sort)
  ---------------------------------------------------------------------- }

function Median(X : TVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Sorts vector X is ascending order and returns its median value
  ---------------------------------------------------------------------- }

function Sum(X : TVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of the elements of vector X
  ---------------------------------------------------------------------- }

function SumSqr(X : TVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of squared elements of vector X
  ---------------------------------------------------------------------- }

function SumSqrDif(X : TVector; Lbound, Ubound : Integer;
                   A : Float) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of squared differences between
  the elements of vector X and the constant A
  ---------------------------------------------------------------------- }

function SumSqrDifVect(X, Y : TVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of squared differences between two vectors
  ---------------------------------------------------------------------- }

function SumWSqr(X, W : TVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of weighted squared elements of vector X
  ---------------------------------------------------------------------- }

function SumWSqrDif(X, W : TVector; Lbound, Ubound : Integer;
                    A : Float) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of weighted squared differences between
  the elements of vector X and the constant A
  ---------------------------------------------------------------------- }

function SumWSqrDifVect(X, Y, W : TVector;
                        Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the sum of weighted squared differences between two vectors
  ---------------------------------------------------------------------- }

function Average(X : TVector; Lbound, Ubound : Integer) : Float;
{ ----------------------------------------------------------------------
  Returns the average value of vector X
  ---------------------------------------------------------------------- }

function Variance(X : TVector; Lbound, Ubound : Integer;
                  Avg : Float) : Float;
{ ----------------------------------------------------------------------
  Returns the variance of vector X, with average Avg
  ---------------------------------------------------------------------- }

function EstVar(X : TVector; Lbound, Ubound : Integer;
                Avg : Float) : Float;
{ ----------------------------------------------------------------------
  Returns the estimated variance of the population
  to which vector X belongs
  ---------------------------------------------------------------------- }

function Skewness(X : TVector; Lbound, Ubound : Integer;
                  Avg, Sigma : Float) : Float;
{ ----------------------------------------------------------------------
  Returns the skewness of vector X,
  with average Avg and standard deviation Sigma
  ---------------------------------------------------------------------- }

function Kurtosis(X : TVector; Lbound, Ubound : Integer;
                  Avg, Sigma : Float) : Float;
{ ----------------------------------------------------------------------
  Returns the kurtosis of vector X,
  with average Avg and standard deviation Sigma
  ---------------------------------------------------------------------- }

implementation

  procedure QSort(X : TVector; Lbound, Ubound : Integer);
  { Quick sort in ascending order - Adapted from Borland's BP7 demo }
    procedure Sort(L, R : Integer);
    var
      I, J : Integer;
      U, V : Float;
    begin
      I := L;
      J := R;
      U := X[(L + R) div 2];
      repeat
        while X[I] < U do I := I + 1;
        while U < X[J] do J := J - 1;
        if I <= J then
          begin
            V := X[I]; X[I] := X[J]; X[J] := V;
            I := I + 1; J := J - 1;
          end;
      until I > J;
      if L < J then Sort(L, J);
      if I < R then Sort(I, R);
    end;

  begin
    Sort(Lbound, Ubound);
  end;

  procedure DQSort(X : TVector; Lbound, Ubound : Integer);
  { Quick sort in descending order - Adapted from Borland's BP7 demo }
    procedure Sort(L, R : Integer);
    var
      I, J : Integer;
      U, V : Float;
    begin
      I := L;
      J := R;
      U := X[(L + R) div 2];
      repeat
        while X[I] > U do I := I + 1;
        while U > X[J] do J := J - 1;
        if I <= J then
          begin
            V := X[I]; X[I] := X[J]; X[J] := V;
            I := I + 1; J := J - 1;
          end;
      until I > J;
      if L < J then Sort(L, J);
      if I < R then Sort(I, R);
    end;

  begin
    Sort(Lbound, Ubound);
  end;

  function Median(X : TVector; Lbound, Ubound : Integer) : Float;
  var
    N, N2 : Integer;
  begin
    N := Ubound - Lbound + 1;
    N2 := N div 2 + Lbound - 1;
    QSort(X, Lbound, Ubound);
    if Odd(N) then
      Median := X[N2 + 1]
    else
      Median := 0.5 * (X[N2] + X[N2 + 1]);
  end;

  function Sum(X : TVector; Lbound, Ubound : Integer) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + X[I];
    Sum := S;
  end;

  function SumSqr(X : TVector; Lbound, Ubound : Integer) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + Sqr(X[I]);
    SumSqr := S;
  end;

  function SumSqrDif(X : TVector; Lbound, Ubound : Integer;
                     A : Float) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + Sqr(X[I] - A);
    SumSqrDif := S;
  end;

  function SumSqrDifVect(X, Y : TVector; Lbound, Ubound : Integer) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + Sqr(X[I] - Y[I]);
    SumSqrDifVect := S;
  end;

  function SumWSqr(X, W : TVector; Lbound, Ubound : Integer) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + W[I] * Sqr(X[I]);
    SumWSqr := S;
  end;

  function SumWSqrDif(X, W : TVector; Lbound, Ubound : Integer; A : Float) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + W[I] * Sqr(X[I] - A);
    SumWSqrDif := S;
  end;

  function SumWSqrDifVect(X, Y, W : TVector;
                          Lbound, Ubound : Integer) : Float;
  var
    S : Float;
    I : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      S := S + W[I] * Sqr(X[I] - Y[I]);
    SumWSqrDifVect := S;
  end;

  function Average(X : TVector; Lbound, Ubound : Integer) : Float;
  begin
    Average := Sum(X, Lbound, Ubound) / (Ubound - Lbound + 1);
  end;

  function Variance(X : TVector; Lbound, Ubound : Integer;
                    Avg : Float) : Float;
  begin
    Variance := SumSqrDif(X, Lbound, Ubound, Avg) / (Ubound - Lbound + 1);
  end;

  function EstVar(X : TVector; Lbound, Ubound : Integer;
                  Avg : Float) : Float;
  begin
    EstVar := SumSqrDif(X, Lbound, Ubound, Avg) / (Ubound - Lbound);
  end;

  function Skewness(X : TVector; Lbound, Ubound : Integer;
                    Avg, Sigma : Float) : Float;
  var
    S, T : Float;
    I    : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      begin
        T := (X[I] - Avg) / Sigma;
        S := S + T * Sqr(T);
      end;
    Skewness := S / (Ubound - Lbound + 1);
  end;

  function Kurtosis(X : TVector; Lbound, Ubound : Integer;
                    Avg, Sigma : Float) : Float;
  var
    S, T : Float;
    I    : Integer;
  begin
    S := 0.0;
    for I := Lbound to Ubound do
      begin
        T := (X[I] - Avg) / Sigma;
        S := S + Sqr(Sqr(T));
      end;
    Kurtosis := S / (Ubound - Lbound + 1) - 3.0;
  end;

end.
