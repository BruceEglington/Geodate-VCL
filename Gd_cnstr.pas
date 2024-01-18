unit GD_Cnstr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, SVGIconVirtualImageList;

type
  TfmConstrain = class(TForm)
    Panel1: TPanel;
    lAge: TLabel;
    lXStr: TLabel;
    lYStr: TLabel;
    eAgeStr: TEdit;
    eXStr: TEdit;
    eYStr: TEdit;
    lMa: TLabel;
    bbOK: TBitBtn;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    procedure FormShow(Sender: TObject);
    procedure eAgeStrExit(Sender: TObject);
    procedure eAgeStrChange(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmConstrain: TfmConstrain;

implementation

uses
  gdw_varb, dmGdtmpDB;

{$R *.DFM}

procedure TfmConstrain.FormShow(Sender: TObject);
var
  tempStr  : string[11];
begin
  lXStr.Caption := XRatioStr[IAnalTyp];
  lYStr.Caption := YRatioStr[IAnalTyp];
  if (AnalType <> '8') then
  begin
    Str(XConstrain:11:6,tempStr);
    eXStr.Text := tempStr;
    Str(YConstrain:11:6,tempStr);
    eYStr.Text := tempStr;
  end;
  if (AnalType = '8') then
  begin
    XConstrain:=exp(DecayConst[5]*AgeConstrain*1.0e6)-1.0;
    YConstrain:=exp(DecayConst[4]*AgeConstrain*1.0e6)-1.0;
    Str(XConstrain:11:6,tempStr);
    eXStr.Text := tempStr;
    Str(YConstrain:11:6,tempStr);
    eYStr.Text := tempStr;
    lAge.Visible := true;
    eAgeStr.Visible := true;
    lMa.Visible := true;
    Str(AgeConstrain:8:3,tempStr);
    eAgeStr.Text := tempStr;
  end else
  begin
    lAge.Visible := false;
    eAgeStr.Visible := false;
    lMa.Visible := false;
  end;
end;

procedure TfmConstrain.eAgeStrExit(Sender: TObject);
var
  CodeVal : integer;
  tempStr : string[11];
begin
  Str(AgeConstrain:8:3,tempStr);
  eAgeStr.Text := tempStr;
  repeat
    tempStr := eAgeStr.Text;
    Val(tempStr, AgeConstrain, CodeVal);
    if (CodeVal <> 0) then
      MessageDlg('Incorrect value for constraining age', mtWarning,[mbOk], 0);
    if ((CodeVal = 0) and ((AgeConstrain < 0.0) or (AgeConstrain > 4570)))
    then begin
      MessageDlg('Incorrect value for constraining age', mtWarning,[mbOk], 0);
      CodeVal := -1;
    end;
  until (CodeVal = 0);
  if (CodeVal = 0) then
  begin
    XConstrain:=exp(DecayConst[5]*AgeConstrain*1.0e6)-1.0;
    YConstrain:=exp(DecayConst[4]*AgeConstrain*1.0e6)-1.0;
    Str(XConstrain:11:6,tempStr);
    eXStr.Text := tempStr;
    Str(YConstrain:11:6,tempStr);
    eYStr.Text := tempStr;
  end;
end;

procedure TfmConstrain.eAgeStrChange(Sender: TObject);
var
  CodeVal : integer;
  tempStr : string[11];
begin
    tempStr := eAgeStr.Text;
    Val(tempStr, AgeConstrain, CodeVal);
    if (CodeVal <> 0) then
      MessageDlg('Incorrect value for constraining age', mtWarning,[mbOk], 0);
    if ((CodeVal = 0) and ((AgeConstrain < 0.0) or (AgeConstrain > 4570)))
    then begin
      MessageDlg('Incorrect value for constraining age', mtWarning,[mbOk], 0);
      CodeVal := -1;
    end;
end;

procedure TfmConstrain.bbOKClick(Sender: TObject);
var
  CodeVal : integer;
  tempStr : string[11];
begin
  if (AnalType = '8') then
  begin
    repeat
      tempStr := eAgeStr.Text;
      Val(tempStr, AgeConstrain, CodeVal);
      if (CodeVal <> 0) then
        MessageDlg('Incorrect value for constraining age', mtWarning,[mbOk], 0);
      if ((CodeVal = 0) and ((AgeConstrain < 0.0) or (AgeConstrain > 4570)))
      then begin
        MessageDlg('Incorrect value for constraining age', mtWarning,[mbOk], 0);
        CodeVal := -1;
      end;
    until (CodeVal = 0);
  end;
  repeat
    tempStr := eXStr.Text;
    Val(tempStr, XConstrain, CodeVal);
    if (CodeVal <> 0) then
    begin
      MessageDlg('Incorrect value for X constraint', mtWarning,[mbOk], 0);
    end;
    tempStr := eYStr.Text;
    Val(tempStr, YConstrain, CodeVal);
    if (CodeVal <> 0) then
    begin
      MessageDlg('Incorrect value for Y constraint', mtWarning,[mbOk], 0);
    end;
  until (CodeVal = 0);
end;

end.
