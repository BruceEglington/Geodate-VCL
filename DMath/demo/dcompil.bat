:: Compilation with Delphi 32 bits
:: The Delphi bin subdirectory must be on the path
:: Tested with Delphi 6, but should work with other versions
:: Change the unit directory if necessary (default is ..\..\units)

:: ---------------------------------------------------------------
:: GUI applications
:: ---------------------------------------------------------------

cd fplot

dcc32 fplot.dpr -DEXTENDEDREAL -U..\..\units > nul

cd ..\fourier

dcc32 testfft.dpr -DEXTENDEDREAL -U..\..\units > nul

cd ..\mgs

dcc32 mgs_demo.dpr -DEXTENDEDREAL -U..\..\units > nul

cd ..\minfunc

dcc32 minfunc.dpr -DEXTENDEDREAL -U..\..\units > nul

cd ..\quadrit

dcc32 quadrit.dpr -DEXTENDEDREAL -U..\..\units > nul

cd ..\regnlin

dcc32 regnlin.dpr -DEXTENDEDREAL -U..\..\units;..\..\units\reg > nul

:: ---------------------------------------------------------------
:: Console applications
:: ---------------------------------------------------------------

cd ..\fmath

for %%a in (*.pas) do dcc32 %%a -cc -DEXTENDEDREAL -U..\..\units > nul

cd ..\matrices

for %%a in (*.pas) do dcc32 %%a -cc -DEXTENDEDREAL -U..\..\units > nul

cd ..\optim

for %%a in (*.pas) do dcc32 %%a -cc -DEXTENDEDREAL -U..\..\units > nul

cd ..\random

for %%a in (*.pas) do dcc32 %%a -cc -DEXTENDEDREAL -U..\..\units > nul

cd ..\reg

for %%a in (*.pas) do dcc32 %%a -cc -DEXTENDEDREAL -U..\..\units;..\..\units\reg > nul

