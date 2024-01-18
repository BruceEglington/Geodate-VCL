program fplot;

uses
  Forms,
  Unit1 in 'unit1.pas' {Form1},
  plotvar in '..\..\units\plotvar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
