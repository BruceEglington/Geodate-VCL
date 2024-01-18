unit Gd_MSWD;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, Grids, DBGrids, StdCtrls, DBCtrls,midaslib, Data.DB,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TfmMSWD = class(TForm)
    Panel1: TPanel;
    ExitBtn: TSpeedButton;
    dbgMSWD: TDBGrid;
    bbSave: TBitBtn;
    DBNavigator1: TDBNavigator;
    VirtualImageList1: TVirtualImageList;
    procedure ExitBtnClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMSWD: TfmMSWD;

implementation

uses
  gdw_varb, dmGdMSWD, dmGdtmpDB;

{$R *.DFM}

procedure TfmMSWD.ExitBtnClick(Sender: TObject);
var
  XSt, YSt, XFn, YFn : double;
  a, b               : double;
  i, j, ISt, IFn     : integer;
  tmpN_Rep           : double;
  itmpN_Rep          : integer;
  tmpFAlpha          : double;
  tmpStr1, tmpStr2   : string;
begin
  //if AllowMessages then ShowMessage(' bme MSWD table opened');
  dmMSWD.cdsF.First;
  tmpFAlpha := dmMSWD.cdsFstAlpha.AsFloat;
  tmpN_Rep := dmMSWD.cdsFstNRep.AsFloat;
  itmpN_Rep := Round(tmpN_Rep);
  if AllowMessages then
  begin
    //ShowMessage('MSWD tmpFAlpha is '+FormatFloat('###0.000',tmpFAlpha));
    //ShowMessage('MSWD FAlpha is '+FormatFloat('###0.000',FAlpha));
    //ShowMessage('MSWD tmpN_Rep is '+FormatFloat('###0.000',tmpN_Rep));
    //ShowMessage('MSWD N_Rep is '+FormatFloat('###0.000',N_Rep));
    //ShowMessage('FAlpha - tmpFAlpha is '+FormatFloat('###0.00000',FAlpha-tmpFAlpha));
    //ShowMessage('N_Rep - tmpN_Rep is '+FormatFloat('###0.00000',N_Rep-tmpN_Rep));
  end;
  j := 1;
  repeat
    tmpFAlpha := dmMSWD.cdsFstAlpha.AsFloat;
    tmpN_Rep := dmMSWD.cdsFstNRep.AsFloat;
    itmpN_Rep := Round(tmpN_Rep);
    //if AllowMessages then ShowMessage('MSWD tmpFAlpha is '+FormatFloat('###0.000',tmpFAlpha));
    //if AllowMessages then ShowMessage('MSWD FAlpha is '+FormatFloat('###0.000',FAlpha));
    //if AllowMessages then ShowMessage('MSWD tmpN_Rep is '+FormatFloat('###0.000',tmpN_Rep));
    //if AllowMessages then ShowMessage('MSWD N_Rep is '+FormatFloat('###0.000',N_Rep));
    //if ((tmpFAlpha = FAlpha) and (itmpN_Rep = N_Rep)) then
    begin
      XSt := dmMSWD.cdsFstNSmp.AsFloat;
      YSt := dmMSWD.cdsFstFvalue.AsFloat;
      ISt := Round(XSt)-2;
      //if AllowMessages then ShowMessage('MSWD ISt is '+IntToStr(ISt));
      MSUM_Val[ISt] := YSt;
      repeat
        ISt := Round(XSt)-2;
        j := ISt+1;
        dmMSWD.cdsF.Next;
        tmpN_Rep := dmMSWD.cdsFstNRep.AsFloat;
        itmpN_Rep := Round(tmpN_Rep);
        if (itmpN_Rep = N_Rep) then
        begin
          XFn := dmMSWD.cdsFstNSmp.AsFloat;
          YFn := dmMSWD.cdsFstFvalue.AsFloat;
          IFn := Round(XFn)-2;
          //if AllowMessages then ShowMessage('MSWD XFn is '+FormatFloat('###0.00',XFn));
          //if AllowMessages then ShowMessage('MSWD IFn is '+IntToStr(IFn));
          if ((IFn-ISt) = 1) then
          begin
            MSUM_Val[IFn] := YFn;
            XSt := XFn;
            YSt := YFn;
          end else
          begin
            MSUM_Val[IFn] := YFn;
            if (IFn > ISt) then
            begin
              b := (MSUM_Val[IFn]-MSUM_Val[ISt])/(1.0*(IFn-ISt));
              a := MSUM_Val[ISt] - b*ISt;
              for i := (ISt+1) to (IFn-1) do
              begin
                MSUM_Val[i] := b*i+a;
                //if ((N_Rep = 999) and (i < 10) and (i > 1)) then
                //begin
                //  ShowMessage(IntToStr(tmpN_Rep)+'  '+IntToStr(i)+'  '+
                //  FormatFloat('##0.000',MSUM_Val[i]));
                //end;
              end;
            end;
            XSt := XFn;
            YSt := YFn;
          end;
        end;
      until ((itmpN_Rep > N_Rep) or (dmMSWD.cdsF.EOF));
    end;
    dmMSWD.cdsF.Next;
  until dmMSWD.cdsF.EOF;
  if (j = 1) then
  begin
    MessageDlg('Insufficient records in F definition table to assess MSWD values',mtWarning,[mbOK],0);
    MessageDlg('Possibly change Number of Replicates to 999',mtInformation,[mbOK],0);
  end;
  if (j > 1) then
  begin
    Close;
  end;
end;

procedure TfmMSWD.bbSaveClick(Sender: TObject);
begin
  dmMSWD.cdsF.First;
  dmMSWD.cdsF.SaveToFile(cdsPath+'TableF.cds');
end;

procedure TfmMSWD.FormCreate(Sender: TObject);
begin
  try
    dmMSWD.cdsF.FileName := cdsPath+'TableF.cds';
    dmMSWD.cdsF.LoadFromFile(dmMSWD.cdsF.FileName);
  except
    MessageDlg('Unable to open MSWD definition file',mtWarning,[mbOK],0);
  end;
end;

end.
