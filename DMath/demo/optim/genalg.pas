{ **********************************************************************
  *                          Program GENALG.PAS                        *
  *                             Version 1.1d                           *
  *             (c) Magali Camut & Jean Debord, February 2003          *
  **********************************************************************
  Minimization of a function of several variables using a genetic
  algorithm.

  Ref.: E. Perrin, A. Mandrille, M. Oumoun, C. Fonteix & I. Marc
        Optimisation globale par strategie d'evolution
        Technique utilisant la genetique des individus diploides
        Recherche operationnelle / Operations Research
        1997, 31, 161-201
  ********************************************************************** }

uses
  fmath, matrices, randnum;

const
  N      = 200;   { Size of population }
  L      = 2;     { Number of variables }
  M      = 0.1;   { Mutation rate }
  G      = 0.6;   { Survival rate }
  H      = 0.5;   { Proportion of homozygotes }
  Tol    = 1E-3;  { Required precision }
  MaxGen = 40;    { Max. number of generations }

var
  S        : Integer;           { Number of survivors }
  NumGen   : Integer;           { Generation counter }
  Pop      : array of TMatrix;  { Population }
  F        : TVector;           { Function values }
  Inf, Sup : TVector;           { Lower and upper bounds }
  Done     : Boolean;           { Convergence check }

procedure Initialize;
{ Initializes global variables and random number generator }
var
  I, J : Integer;
begin
  NumGen := 1;
  S := Round(N * G);

  DimVector(Inf, L);
  DimVector(Sup, L);
  for I := 1 to L do
    begin
      Inf[I] := -4.0;
      Sup[I] := 4.0;
    end;

  { Initialize Marsaglia's random number generator
    using Pascal generator }
  RMarIn(Random(10000), Random(10000));

  { Set initial population }
  SetLength(Pop, Succ(N), 5, Succ(L));
  for I := 1 to N do
    for J := 1 to L do
      begin
        Pop[I,2,J] := Inf[J] + RanMar * (Sup[J] - Inf[J]);  { 1st chromosome }
        Pop[I,3,J] := Inf[J] + RanMar * (Sup[J] - Inf[J]);  { 2nd chromosome }
        Pop[I,4,J] := RanMar;                               { Vector of dominances }
        Pop[I,1,J] := Pop[I,4,J] * Pop[I,2,J] +
                        (1.0 - Pop[I,4,J]) * Pop[I,3,J];    { Phenotype }
      end;

  DimVector(F, N);
end;

function Func(X : TVector) : Float;
{ Function to be minimized }
var
  I           : Integer;
  OutOfBounds : Boolean;
begin
  I := 1;
  repeat
    OutOfBounds := (X[I] < Inf[I]) or (X[I] > Sup[I]);
    Inc(I);
  until OutOfBounds or (I > L);

  if OutOfBounds then
    Func := MAXNUM
  else
    Func := Sqr(Sqr(X[1] - 2.0)) + Sqr(X[1] - 2.0 * X[2]);
end;

procedure CheckPrecision;
{ Checks if the new population verifies the stopping criterion }
var
  I, J : Integer;
  F    : Float;
begin
  Done := True;
  for I := 1 to (N - 1) do
    begin
      F := Func(Pop[I,1]);
      for J := (I + 1) to N do
        if (Abs(F - Func(Pop[J,1])) >= Tol * Abs(F))
          and (NumGen <= MaxGen) then
            Done := False;
    end;
end;

procedure Cross(Par1, Par2, Child : TMatrix);
{ Creates a child from the genotypes of the two parents }
var
  J : Integer;
begin
  for J := 1 to L do
    begin
      if RanMar < 0.5 then
        Child[2,J] := Par1[2,J]
      else
        Child[2,J] := Par1[3,J];

      if RanMar < 0.5 then
        Child[3,J] := Par2[2,J]
      else
        Child[3,J] := Par2[3,J];

      Child[4,J] := RanMar;

      Child[1,J] := Child[4,J] * Child[2,J] + (1.0 - Child[4,J]) * Child[3,J];
    end;
end;

procedure Mutation(X : TMatrix);
{ Applies the mutation operator to child X }
var
  I : Integer;
begin
  for I := 1 to L do
    begin
      X[2,I] := Inf[I] + RanMar * (Sup[I] - Inf[I]);
      X[3,I] := Inf[I] + RanMar * (Sup[I] - Inf[I]);
      X[4,I] := RanMar;
      X[1,I] := X[4,I] * X[2,I] + (1.0 - X[4,I]) * X[3,I];
    end;
end;

procedure Homozygote(X : TMatrix);
{ Applies the homozygote operator to child X }
var
  I : Integer;
begin
  for I := 1 to L do
    begin
      X[2,I] := X[1,I];
      X[3,I] := X[1,I];
    end;
end;

procedure Sort_population;
{ Sorts population according to function values }
var
  I, J, K, K1, K2 : Integer;
  A               : Float;
begin
  for I := 1 to Pred(N) do
    begin
      K := I;
      A := F[I];
      for J := Succ(I) to N do
        if F[J] < A then
          begin
            K := J;
            A := F[J];
          end;
      Swap(F[I], F[K]);
      for K1 := 1 to 4 do
        for K2 := 1 to L do
          Swap(Pop[I,K1,K2], Pop[K,K1,K2]);
    end;
end;

procedure Selection;
var
  I : Integer;
begin
  for I := 1 to N do
    F[I] := Func(Pop[I,1]);
  Sort_population;
end;

procedure Generate_new_population;
var
  Par1, Par2 : TMatrix;
  I          : Integer;
  F0         : Float;
begin
  DimMatrix(Par1, 4, L);
  DimMatrix(Par2, 4, L);

  Selection;

  for I := (S + 1) to N do
    begin
      Par1 := Pop[Random(S + 1)];
      Par2 := Pop[Random(S + 1)];

      F0 := Func(Pop[I,1]);
      while (F0 > Func(Par1[1])) and (F0 > Func(Par2[1])) do
        begin
          Cross(Par1, Par2, Pop[I]);
          F0 := Func(Pop[I,1]);
        end;
    end;

  for I := 1 to N do
    begin
      if RanMar <= M then Mutation(Pop[I]);
      if RanMar <= H then Homozygote(Pop[I]);
    end;
end;

begin
  Initialize;
  repeat
    Generate_new_population;
    NumGen := NumGen + 1;
    CheckPrecision;
  until Done;
  Writeln('Minimum     : ', F[1]);
  Writeln('Coordinates : ', Pop[1,1,1], Pop[1,1,2]);
  readln;
end.
