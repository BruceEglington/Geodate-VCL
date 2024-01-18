{ **********************************************************************
  *                         Program TESTMACH.PAS                       *
  *                             Version 1.1d                           *
  *                     (c) J. Debord, February 2003                   *
  **********************************************************************
  This program displays the floating point type and the machine-
  dependent constants.
  ********************************************************************** }

program testmach;

uses
  fmath, fspec;

var
  N : Byte;

begin
  N := sizeof(Float);
  writeln;
  write('Float type = ');
  if N = sizeof(Single) then
    write('Single')
  else if N = sizeof(Double) then
    write('Double')
  else if N = sizeof(Extended) then
    write('Extended')
  else
    write('Real');
  writeln('       Size = ', N, ' bytes');
  writeln;
  writeln('MACHEP          = ', MACHEP);

  writeln;
  writeln('MINNUM          = ', MINNUM);
  writeln('Exp(MINLOG)     = ', Exp(MINLOG));

  writeln;
  writeln('MINLOG          = ', MINLOG);
  writeln('Ln(MINNUM)      = ', Ln(MINNUM));

  writeln;
  writeln('MAXNUM          = ', MAXNUM);
  writeln('Exp(MAXLOG)     = ', Exp(MAXLOG));

  writeln;
  writeln('MAXLOG          = ', MAXLOG);
  writeln('Ln(MAXNUM)      = ', Ln(MAXNUM));

  writeln;
  writeln('MAXFAC          =  ', MAXFAC);
  writeln('Fact(MAXFAC)    = ', Fact(MAXFAC));

  writeln;
  writeln('MAXGAM          = ', MAXGAM);
  writeln('Gamma(MAXGAM)   = ', Gamma(MAXGAM));

  writeln;
  writeln('MAXLGM          = ', MAXLGM);
  writeln('LnGamma(MAXLGM) = ', LnGamma(MAXLGM));

  readln;
end.

