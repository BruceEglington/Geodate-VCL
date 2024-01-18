unit Gd_SWtAv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmSelectWtAv = class(TForm)
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSelectWtAv: TfmSelectWtAv;

implementation

{$R *.DFM}

uses
  GDW_varb, Gd_WtAv;
procedure TfmSelectWtAv.FormShow(Sender: TObject);
begin
  fmWtAv.ShowModal;
end;

procedure TfmSelectWtAv.bbCancelClick(Sender: TObject);
begin
  Close;
end;

end.
