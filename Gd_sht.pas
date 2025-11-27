unit Gd_sht;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Buttons, OleCtrls, ExtCtrls, StdCtrls, AxCtrls,
  Vcl.Grids,
  VCL.FlexCel.Core, FlexCel.XlsAdapter, FlexCel.Render, FlexCel.Preview,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, FlexCel.VCLSupport,
  FlexCel.Core, ImageCollection_dm;

type
  TfmSheet = class(TForm)
    Panel1: TPanel;
    sbClose: TSpeedButton;
    sbSheet: TStatusBar;
    SaveDialogSprdSheet: TSaveDialog;
    bbSaveSheet: TBitBtn;
    Panel2: TPanel;
    Panel3: TPanel;
    FlexCelPreviewer1: TFlexCelPreviewer;
    VirtualImageList1: TVirtualImageList;
    procedure sbCloseClick(Sender: TObject);
    procedure bbSaveSheetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSheet: TfmSheet;

implementation

{$R *.DFM}

uses
  gdw_varb, gd_drv, dmGdtmpDB;

procedure TfmSheet.sbCloseClick(Sender: TObject);
begin
  try
  finally
    //FreeAndNil(SprdSheet);
  end;
  Close;
end;

procedure TfmSheet.bbSaveSheetClick(Sender: TObject);
const
  //Excel5Type = 4;
  //VisualComponentType = 5;
  Excel97Type = 11;
  //FormulaOne6Type = 12;
var
  //pFileType : smallint;
  //pBuf      : string;
  //pTitle    : string;
  tmpStr    : string[3];
PosDot : integer;
begin
  SaveDialogSprdSheet.InitialDir := TTPath;
  PosDot := Pos('.',ProjectName);
  ProjectName := Copy(ProjectName,1,PosDot-1);
  SaveDialogSprdSheet.FileName := ProjectName;
  if SaveDialogSprdSheet.Execute then
  begin
    Drive3 := ExtractFileDir(SaveDialogSprdSheet.FileName);
    TTPath := ExtractFilePath(SaveDialogSprdSheet.FileName);
    //case SaveDialogSprdSheet.FilterIndex of
    //  1 : pFileType := Excel97Type;
    //  2 : pFileType := Excel5Type;
    //end;
    //pBuf := SaveDialogSprdSheet.FileName;
    //SprdSheet.Write(pBuf,pFileType);
    //SprdSheet.Save(SaveDialogSprdSheet.FileName);
  end;
end;

end.

