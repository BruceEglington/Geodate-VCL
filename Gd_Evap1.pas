unit Gd_Evap1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, ImageCollection_dm;

type
  TfmZrEvap = class(TForm)
    bbOK: TBitBtn;
    Panel1: TPanel;
    bbCancel: TBitBtn;
    eZrEvapStr: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    VirtualImageList1: TVirtualImageList;
    procedure bbCancelClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmZrEvap: TfmZrEvap;

implementation

uses
  Gdw_varb, dmGdtmpDB;
{$R *.DFM}

procedure TfmZrEvap.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmZrEvap.bbOKClick(Sender: TObject);
var
  tmpStr : string[15];
  ICode  : integer;
begin
  tmpStr := eZrEvapStr.Text;
  Val(tmpStr,BlanketZErrVal,ICode);
  if (ICode <> 0) then
  begin
    MessageDlg('Error with value entered',mtWarning,[mbOK],0);
    ModalResult := mrCancel;
    Exit;
  end else
  begin
    Close;
  end;
end;

procedure TfmZrEvap.FormShow(Sender: TObject);
var
  tmpStr : string[8];
begin
  Str(BlanketZErrVal:8:4,tmpStr);
  eZrEvapStr.Text := tmpStr;
end;

end.
