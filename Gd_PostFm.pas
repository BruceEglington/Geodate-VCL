unit Gd_PostFm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AxCtrls, OleCtrls, Buttons, ExtCtrls, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, ImageCollection_dm;

type
  TfmModelPbPostFm = class(TForm)
    Panel1: TPanel;
    sbClose: TSpeedButton;
    bbSaveSheet: TBitBtn;
    SaveDialogSprdSheet: TSaveDialog;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    eFormation: TEdit;
    eRequired: TEdit;
    bbCalculate: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    VirtualImageList1: TVirtualImageList;
    procedure bbCalculateClick(Sender: TObject);
    procedure bbSaveSheetClick(Sender: TObject);
    procedure sbCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmModelPbPostFm: TfmModelPbPostFm;

implementation

uses GDW_regp, GDW_varb, AllSorts, dmGdtmpDB;

{$R *.DFM}

procedure TfmModelPbPostFm.bbCalculateClick(Sender: TObject);
var
  //FormationAge, RequiredAge : double;
  MuRes, MuSmp, Pb64i, Pb74i, Pb64si, Pb74si : double;
  iCode, i : integer;
  tmpStr : string;
begin
  FormationAge := 0.0;
  tmpStr := eFormation.Text;
  ClearNull(tmpStr);
  eFormation.Text := tmpStr;
  if (eFormation.Text <> '') then
  begin
    Val(eFormation.Text,FormationAge,iCode);
    FormationAge := FormationAge*1.0e6;
  end;
  Val(eRequired.Text,RequiredAge,iCode);
  RequiredAge := RequiredAge*1.0e6;
  {
  SprdSheet.Row := 1;
  SprdSheet.Col := 1;
  SprdSheet.Text := 'Sample';
  SprdSheet.Col := 2;
  SprdSheet.Text := '206Pb/204Pb';
  SprdSheet.Col := 3;
  SprdSheet.Text := '207Pb/204Pb';
  SprdSheet.Col := 4;
  SprdSheet.Text := 'RFlg';
  SprdSheet.Col := 7;
  SprdSheet.Text := '206Pb/204Pb at date';
  SprdSheet.Col := 8;
  SprdSheet.Text := '207Pb/204Pb at date';
  SprdSheet.Col := 9;
  SprdSheet.Text := 'Formation Age';
  SprdSheet.Col := 10;
  SprdSheet.Text := 'Required Age';
  }
  for i := 1 to NumberOfPoints do
  begin
    if (eFormation.Text = '') then
      FormationAge := Ratio[i,3]*1.0e6;
    {
    SprdSheet.Row := i+1;
    SprdSheet.Col := 1;
    SprdSheet.Text := SmpNo[i];
    SprdSheet.Col := 2;
    SprdSheet.Number := Ratio[i,1];
    SprdSheet.Col := 3;
    SprdSheet.Number := Ratio[i,2];
    SprdSheet.Col := 4;
    SprdSheet.Text := RFlg[i];
    SprdSheet.Col := 9;
    SprdSheet.Number := FormationAge/1.0e6;
    SprdSheet.Col := 10;
    SprdSheet.Number := RequiredAge/1.0e6;
    }
    if ((FormationAge > 0.0) and (PFlg[i] = 'Y')) then
    begin
      if (RFlg[i] = 'Y') then
      begin
        Slope := (exp5t(FormationAge)-1.0)/(137.88*(exp8t(FormationAge)-1.0));
        MuRes := (Slope*(MuV[mu_choice,2]-Ratio[i,1])+Ratio[i,2]-MuV[mu_choice,3]);
        MuRes := MuRes/((exp5t(MuV[mu_choice,1])-exp5t(FormationAge))/137.88-Slope*(exp8t(MuV[mu_choice,1])-exp8t(FormationAge)));
        Pb64i := MuV[mu_choice,2]+MuRes*(exp8t(MuV[mu_choice,1])-exp8t(FormationAge));
        Pb74i := MuV[mu_choice,3]+(MuRes/137.88)*(exp5t(MuV[mu_choice,1])-exp5t(FormationAge));
        MuSmp := (Ratio[i,1]-Pb64i)/(exp8t(MuV[mu_choice,1])-exp8t(FormationAge));
        Pb64si := Pb64i+MuSmp*(exp8t(FormationAge)-exp8t(RequiredAge));
        Pb74si := Pb74i+(MuSmp/137.88)*(exp5t(FormationAge)-exp5t(RequiredAge));
        {
        SprdSheet.Col := 5;
        SprdSheet.Number := MuRes;
        SprdSheet.Col := 6;
        SprdSheet.Number := MuSmp;
        SprdSheet.Col := 7;
        SprdSheet.Number := Pb64si;
        SprdSheet.Col := 8;
        SprdSheet.Number := Pb74si;
        }
      end;
      if (RFlg[i] = 'N') then
      begin
        Slope := (exp5t(FormationAge)-1.0)/(137.88*(exp8t(FormationAge)-1.0));
        MuRes := (Slope*(MuV[mu_choice,2]-Ratio[i,1])+Ratio[i,2]-MuV[mu_choice,3]);
        MuRes := MuRes/((exp5t(MuV[mu_choice,1])-exp5t(FormationAge))/137.88-Slope*(exp8t(MuV[mu_choice,1])-exp8t(FormationAge)));
        Pb64i := MuV[mu_choice,2]+MuRes*(exp8t(MuV[mu_choice,1])-exp8t(FormationAge));
        Pb74i := MuV[mu_choice,3]+(MuRes/137.88)*(exp5t(MuV[mu_choice,1])-exp5t(FormationAge));
        MuSmp := (Ratio[i,1]-Pb64i)/(exp8t(MuV[mu_choice,1])-exp8t(FormationAge));
        Pb64si := Pb64i+MuSmp*(exp8t(FormationAge)-exp8t(RequiredAge));
        Pb74si := Pb74i+(MuSmp/137.88)*(exp5t(FormationAge)-exp5t(RequiredAge));
        Xtra[i] := MuSmp;
        Xtra1[i] := Pb64si;
        Xtra2[i] := Pb74si;
        {
        SprdSheet.Col := 5;
        SprdSheet.Number := MuRes;
        SprdSheet.Col := 6;
        SprdSheet.Number := MuSmp;
        SprdSheet.Col := 7;
        SprdSheet.Number := Pb64si;
        SprdSheet.Col := 8;
        SprdSheet.Number := Pb74si;
        }
      end;
    end;
  end;
end;

procedure TfmModelPbPostFm.bbSaveSheetClick(Sender: TObject);
const
  Excel5Type = 4;
  VisualComponentType = 5;
  Excel97Type = 11;
  FormulaOne6Type = 12;
begin
  SaveDialogSprdSheet.InitialDir := TTPath;
  SaveDialogSprdSheet.FileName := ProjectName;
  if SaveDialogSprdSheet.Execute then
  begin
    Drive3 := ExtractFileDir(SaveDialogSprdSheet.FileName);
    TTPath := ExtractFilePath(SaveDialogSprdSheet.FileName);
  end;
end;

procedure TfmModelPbPostFm.sbCloseClick(Sender: TObject);
var
  iCode : integer;
  tmpStr : string;
begin
  FormationAge := 0.0;
  tmpStr := eFormation.Text;
  ClearNull(tmpStr);
  eFormation.Text := tmpStr;
  if (eFormation.Text <> '') then
  begin
    FormationAge := -9999.99;
    Val(eFormation.Text,FormationAge,iCode);
    if (iCode=0) then FormationAge := FormationAge*1.0;
  end;
  RequiredAge := -9999.99;
  Val(eRequired.Text,RequiredAge,iCode);
  if (iCode=0) then RequiredAge := RequiredAge*1.0;
  Close;
end;

end.
