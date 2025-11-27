unit Gd_Drv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  ImageCollection_dm;

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
    Label2: TLabel;
    eFlexTemplatePath: TEdit;
    Label3: TLabel;
    eExportPath: TEdit;
    VirtualImageList1: TVirtualImageList;
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
  if (Drive1 = '') then Drive1 := 'C:\USERS\YourUser\Appdata\EggSoft\Geodate\temp\';
  if (Drive2 = '') then Drive2 := 'C:\USERS\YourUser\Appdata\EggSoft\Geodate\data\';
  if (Drive3 = '') then Drive3 := 'USERS\YourUser\Appdata\EggSoft\Geodate\temp\';
  if (cdsPath = '') then cdsPath := 'C:USERS\YourUser\Appdata\EggSoft\Geodate\data\';
  if (FlexTemplatePath = '') then FlexTemplatePath := 'C:\USERS\YourUser\Appdata\EggSoft\Geodate\Templates\';
  if (ExportPath = '') then ExportPath := 'C:\USERS\YourUser\Appdata\EggSoft\Geodate\temp\';
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
  eFlexTemplatePath.Text := FlexTemplatePath;
  eExportPath.Text := ExportPath;
end;

end.
