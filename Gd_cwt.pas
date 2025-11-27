unit Gd_cwt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, ImageCollection_dm;

type
  TfmConcordiaWtType = class(TForm)
    Panel1: TPanel;
    rbConcordiaNormal: TRadioButton;
    rbConcordiaUpper: TRadioButton;
    rbConcordiaLower: TRadioButton;
    bbOK: TBitBtn;
    VirtualImageList1: TVirtualImageList;
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmConcordiaWtType: TfmConcordiaWtType;

implementation

uses
  gdw_varb, dmGdtmpDB;

{$R *.DFM}

procedure TfmConcordiaWtType.bbOKClick(Sender: TObject);
begin
  if rbConcordiaNormal.Checked then
  begin
    AnalType8 := 'N';
    Model := 1;
  end;
  if rbConcordiaUpper.Checked then
  begin
    AnalType8 := 'U';
    Model := 4;
  end;
  if rbConcordiaLower.Checked then
  begin
    AnalType8 := 'L';
    Model := 4;
  end;
end;

procedure TfmConcordiaWtType.FormShow(Sender: TObject);
begin
  if CharInSet(AnalType8,['N','E']) then
  begin
    rbConcordiaNormal.Checked := true;
    rbConcordiaUpper.Checked := false;
    rbConcordiaLower.Checked := false;
  end;
  if CharInSet(AnalType8,['U']) then
  begin
    rbConcordiaNormal.Checked := false;
    rbConcordiaUpper.Checked := true;
    rbConcordiaLower.Checked := false;
  end;
  if CharInSet(AnalType8,['L']) then
  begin
    rbConcordiaNormal.Checked := false;
    rbConcordiaUpper.Checked := false;
    rbConcordiaLower.Checked := true;
  end;
end;

end.
