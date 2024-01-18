unit Gd_Drv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  SVGIconVirtualImageList;

type
  TfmOptDir = class(TForm)
    lDirSys: TLabel;
    lDirData: TLabel;
    lDirTemp: TLabel;
    eDrive1: TEdit;
    eDrive2: TEdit;
    eDrive3: TEdit;
    bbOK: TBitBtn;
    Label1: TLabel;
    eFTable: TEdit;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmOptDir: TfmOptDir;

implementation

{$R *.DFM}
uses
  gdw_varb, dmGdtmpDB;

procedure TfmOptDir.bbOKClick(Sender: TObject);
begin
  {
  GdwNetFileDir := eNetFileDir.Text;
  }
  Drive1 := eDrive1.Text;
  Drive2 := eDrive2.Text;
  Drive3 := eDrive3.Text;
  cdsPath := eFTable.Text;
  if (GdwNetFileDir = '') then GdwNetFileDir := 'C:\';
  if (Drive1 = '') then Drive1 := 'C:\PROGRAMDATA\EGGSOFT\';
  if (Drive2 = '') then Drive2 := 'C:\PROGRAMDATA\EGGSOFT\GDW\DATA\';
  if (Drive3 = '') then Drive3 := 'C:\PROGRAMDATA\EGGSOFT\GDW\TEMP\';
  if (cdsPath = '') then cdsPath := 'C:\PROGRAMDATA\EGGSOFT\';
  Close;
end;

procedure TfmOptDir.FormShow(Sender: TObject);
begin
  {
  eNetFileDir.Text := GdwNetFileDir;
  }
  eDrive1.Text := Drive1;
  eDrive2.Text := Drive2;
  eDrive3.Text := Drive3;
  eFTable.Text := cdsPath;
end;

end.
