unit GDErnOpt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Mask, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, SVGIconVirtualImageList;

type
  TfmGetErrorchronOption = class(TForm)
    Panel1: TPanel;
    rbbAugSqrt: TRadioButton;
    rbbNoAssum: TRadioButton;
    rbbVarInit: TRadioButton;
    rbbSepErr: TRadioButton;
    rbbNoAug: TRadioButton;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Panel4: TPanel;
    Label3: TLabel;
    bbOK: TBitBtn;
    bbHelp: TBitBtn;
    rbbUprItcpt: TRadioButton;
    rbbLwrItcpt: TRadioButton;
    eMSWD: TEdit;
    eCritF: TEdit;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbbAugSqrtClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmGetErrorchronOption: TfmGetErrorchronOption;

implementation

uses
  gdw_varb, dmGdtmpDB;

{$R *.DFM}

procedure TfmGetErrorchronOption.FormCreate(Sender: TObject);
begin
  rbbAugSqrt.Checked := true;
end;

procedure TfmGetErrorchronOption.FormShow(Sender: TObject);
begin
  bbHelp.Visible := false;
end;

procedure TfmGetErrorchronOption.rbbAugSqrtClick(Sender: TObject);
begin
  {
  rbbAugSqrt.Checked := false;
  rbbNoAssum.Checked := false;
  rbbVarInit.Checked := false;
  rbbUprItcpt.Checked := false;
  rbbLwrItcpt.Checked := false;
  rbbSepErr.Checked := false;
  rbbNoAug.Checked := false;
  if (Sender = rbbAugSqrt) then rbbAugSqrt.Checked := true;
  if (Sender = rbbNoAssum) then rbbNoAssum.Checked := true;
  if (Sender = rbbVarInit) then rbbVarInit.Checked := true;
  if (Sender = rbbUprItcpt) then rbbUprItcpt.Checked := true;
  if (Sender = rbbLwrItcpt) then rbbLwrItcpt.Checked := true;
  if (Sender = rbbSepErr) then rbbSepErr.Checked := true;
  if (Sender = rbbNoAug) then rbbNoAug.Checked := true;
  fmGetErrorchronOption.Refresh;
  }
end;

procedure TfmGetErrorchronOption.bbOKClick(Sender: TObject);
begin
  if rbbAugSqrt.Checked then Model := 1;
  if rbbNoAssum.Checked then Model := 2;
  if rbbVarInit.Checked then Model := 3;
  if rbbUprItcpt.Checked then
  begin
    Model := 4;
    AnalType8 := 'U';
  end;
  if rbbLwrItcpt.Checked then
  begin
    Model := 4;
    AnalType8 := 'L';
  end;
  if rbbSepErr.Checked then Model := 5;
  if rbbNoAug.Checked then Model := 6;
  if (Model <> 4) then AnalType8 := 'N';
  if ((Model < 1) or (Model > 6)) then Model := 1;
end;

end.
