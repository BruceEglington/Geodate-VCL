program MinFunc;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  uFunc in 'uFunc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
