{ **********************************************************************
  *                         Program RANTEST.PAS                        *
  *                             Version 1.7d                           *
  *                     (c) J. Debord, February 2003                   *
  **********************************************************************
  This program picks 20000 random numbers and displays the next six,
  together with the correct values obtained with the default
  initialization, i.e. RMarIn(1802, 9373).
  ********************************************************************** }

program rantest;

uses
  fmath, randnum;

const
  Correct : array[1..6] of Integer
  = (921625997, 1094293978, 115775252, 499820504, -1929018715, 2008943384);

var
  I, R : Integer;

begin
  WriteLn;
  Writeln('  Test of Marsaglia random number generator');
  WriteLn('---------------------------------------------');
  WriteLn('       Correct           Actual');
  WriteLn('---------------------------------------------');

  { Pick 20000 random numbers }
  for I := 1 to 20000 do
    R := IRanMar;

  { Display 6 more numbers with correct values }
  for I := 1 to 6 do
    begin
      R := IRanMar;
      Write('  ', Correct[I]:12, '     ', R:12, '           ');
      if Correct[I] = R then WriteLn('OK') else WriteLn('BAD');
    end;
  WriteLn('---------------------------------------------');
end.

