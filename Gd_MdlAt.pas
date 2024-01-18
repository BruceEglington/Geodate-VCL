unit Gd_MdlAt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, Mask, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList;

type
  TfmGetMdlAt = class(TForm)
    bbOK: TBitBtn;
    Panel1: TPanel;
    eAt: TEdit;
    lAt: TLabel;
    bbCancel: TBitBtn;
    ISigma: TLabel;
    eSigma: TEdit;
    VirtualImageList1: TVirtualImageList;
    procedure bbCancelClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure eAtExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bDate  : boolean;
  end;

var
  fmGetMdlAt: TfmGetMdlAt;

implementation

{$R *.DFM}

uses
  GDW_varb, dmGdtmpDB;

var
  temp, temperror   : double;
  iCode  : integer;

procedure TfmGetMdlAt.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfmGetMdlAt.bbOKClick(Sender: TObject);
begin
  eAtExit(Sender);
  if (iCode = 0) then
  begin
    if (bDate) then
    begin
      Age := temp;
      AgeError := temperror;
      {
      ShowMessage('Date = '+meAt.Text);
      }
    end;
    if (not bDate) then
    begin
      InitRatio := temp;
      InitRatioError := temperror;
      {
      ShowMessage('Ratio = '+meAt.Text);
      }
    end;
  end;
end;

procedure TfmGetMdlAt.eAtExit(Sender: TObject);
var
  tmpStr, tmperrorStr : string[15];
begin
  iCode := 1;
  tmpStr := Trim(eAt.Text);
  tmperrorStr := Trim(eSigma.Text);
  {
  ShowMessage('Result = '+tmpStr);
  }
  repeat
    Val(tmpStr,temp,iCode);
    if (iCode <> 0) then
    begin
      if MessageDlg('Incorrect entry',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((bDate) and (temp < 0.0)) then
    begin
      iCode := -1;
      if MessageDlg('Negative value',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((not bDate) and (temp < 0.000001)) then
    begin
      iCode := -1;
      if MessageDlg('Value too small',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((bDate) and (temp > 4570.0)) then
    begin
      iCode := -2;
      if MessageDlg('Date too old',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((not bDate) and (temp > 100.0)) then
    begin
      iCode := -3;
      if MessageDlg('Ratio too large',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if (iCode <> 0) then
    begin
      MessageDlg(eAt.Text,mtWarning,[mbOK],0);
    end;
    Val(tmperrorStr,temperror,iCode);
    if (iCode <> 0) then
    begin
      if MessageDlg('Incorrect entry',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((bDate) and (temperror < 0.0)) then
    begin
      iCode := -1;
      if MessageDlg('Negative value for 1 sigma uncertainty',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((not bDate) and (temperror < 0.0)) then
    begin
      iCode := -1;
      if MessageDlg('Value too small for 1 sigma uncertainty',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((bDate) and (temperror > 500.0)) then
    begin
      iCode := -2;
      if MessageDlg('1 sigma age uncertainty too large',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if ((not bDate) and (temperror > 50.0)) then
    begin
      iCode := -3;
      if MessageDlg('1 sigma ratio uncertainty too large',mtWarning,[mbOK,mbCancel],0) = mrCancel
         then Exit;
    end;
    if (iCode <> 0) then
    begin
      MessageDlg(eSigma.Text,mtWarning,[mbOK],0);
    end;
  until (iCode = 0);
  {
  ShowMessage('Result 2 = '+tmpStr);
  }
end;

procedure TfmGetMdlAt.FormShow(Sender: TObject);
var
  tmpStr : string[15];
begin
  if (bDate) then
  begin
    lAt.Caption := 'Date (Ma)';
    Str(Age:6:2,tmpStr);
    eAt.Text := tmpStr;
    temp := Age;
    Str(AgeError:6:2,tmpStr);
    eSigma.Text := tmpStr;
    temperror := AgeError;
  end;
  if (not bDate) then
  begin
    if (Intercept > 0.0) then InitRatio := Intercept;
    lAt.Caption := 'Ratio';
    Str(InitRatio:12:6,tmpStr);
    eAt.Text := tmpStr;
    temp := InitRatio;
    Str(InitRatioError:12:6,tmpStr);
    eSigma.Text := tmpStr;
    temperror := InitRatioError;
  end;
end;

end.
