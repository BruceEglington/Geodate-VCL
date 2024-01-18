:: Compilation with Delphi 32 bits
:: The Delphi bin subdirectory must be on the path
:: Tested with Delphi 6, but should work with earlier versions

for %%a in (*.pas) do dcc32 %%a -DEXTENDEDREAL > nul

cd reg

for %%a in (*.pas) do dcc32 %%a -DEXTENDEDREAL -U.. > nul


