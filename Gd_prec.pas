unit Gd_prec;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TfmPrecision = class(TForm)
    GroupBox1: TGroupBox;
    rbX: TRadioButton;
    rbY: TRadioButton;
    GroupBox2: TGroupBox;
    rb1Sigma: TRadioButton;
    rb2Sigma: TRadioButton;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    cbPrecAll: TCheckBox;
    VirtualImageList1: TVirtualImageList;
    rbZ: TRadioButton;
    procedure bbCancelClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure rb1SigmaClick(Sender: TObject);
    procedure rb2SigmaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PrecVar  : char;
    PrecMult : double;
    PrecAll  : boolean;
  end;

var
  fmPrecision: TfmPrecision;

implementation

uses GDW_varb, dmGdtmpDB;

{$R *.DFM}

procedure TfmPrecision.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmPrecision.bbOKClick(Sender: TObject);
begin
  if (rbX.Checked) then PrecVar := 'X';
  if (rbY.Checked) then PrecVar := 'Y';
  if (rbZ.Checked) then PrecVar := 'Z';
  PrecAll := cbPrecAll.Checked;
end;

procedure TfmPrecision.rb1SigmaClick(Sender: TObject);
begin
  PrecMult := 1.0;
end;

procedure TfmPrecision.rb2SigmaClick(Sender: TObject);
begin
  PrecMult := 2.0;
end;

procedure TfmPrecision.FormShow(Sender: TObject);
begin
  rbX.Caption := XRatioStr[IAnalTyp];
  rbY.Caption := YRatioStr[IAnalTyp];
  rbZ.Caption := ZRatioStr[IAnalTyp];
  PrecMult := 1.0;
  PrecAll := false;
end;

end.
