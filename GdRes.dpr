program GdRes;

uses
  Forms,
  Gd_Res in 'Gd_Res.pas' {fmRes},
  dmGdRes in 'dmGdRes.pas' {dmGdwResults: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Date Viewer';
  Application.CreateForm(TfmRes, fmRes);
  Application.CreateForm(TdmGdwResults, dmGdwResults);
  Application.Run;
end.
