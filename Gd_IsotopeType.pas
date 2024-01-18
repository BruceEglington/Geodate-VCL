unit Gd_IsotopeType;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfmIsotopeSystemType = class(TForm)
    bbOK: TBitBtn;
    Panel1: TPanel;
    rbSmNd: TRadioButton;
    rbLuHf: TRadioButton;
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmIsotopeSystemType: TfmIsotopeSystemType;

implementation

uses
  gdw_varb;

{$R *.DFM}

procedure TfmIsotopeSystemType.bbOKClick(Sender: TObject);
begin
  if rbSmNd.Checked then
  begin
    IAnalTyp := 2;
  end;
  if rbLuHf.Checked then
  begin
    IAnalTyp := 7;
  end;
end;

procedure TfmIsotopeSystemType.FormShow(Sender: TObject);
begin
  if (AnalType in ['J']) then
  begin
    rbSmNd.Checked := false;
    rbLuHf.Checked := true;
    if (IAnalTyp = 2) then
    begin
      rbSmNd.Checked := true;
      rbLuHf.Checked := false;
    end else
    begin
      rbSmNd.Checked := false;
      rbLuHf.Checked := true;
    end;
  end;
end;

end.
