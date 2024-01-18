unit Gd_edit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, DBCtrls, Mask, System.UITypes,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TfmEdit = class(TForm)
    pTop: TPanel;
    pEdit: TPanel;
    lXXStr: TLabel;
    lYYStr: TLabel;
    StatusBar1: TStatusBar;
    lTitle: TLabel;
    lNRep: TLabel;
    lXStr: TLabel;
    lYStr: TLabel;
    lZStr: TLabel;
    bbClose: TBitBtn;
    bbRecalculate: TBitBtn;
    bbCancel: TBitBtn;
    bbErrorsPC: TBitBtn;
    bbPrecisions: TBitBtn;
    bbCorrelation: TBitBtn;
    bbEvap: TBitBtn;
    eTitle: TEdit;
    eNRep: TEdit;
    lValue: TLabel;
    lPrecision: TLabel;
    lErr: TLabel;
    lR: TLabel;
    dbnEdit: TDBNavigator;
    dbeXX: TDBEdit;
    dbeYY: TDBEdit;
    dbeXRatio: TDBEdit;
    dbeXPrec: TDBEdit;
    dbeXErr: TDBEdit;
    dbeSample_No: TDBEdit;
    dbcbXErrType: TDBComboBox;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBComboBox1: TDBComboBox;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    lSample: TLabel;
    eRec: TEdit;
    eRecCount: TEdit;
    Label1: TLabel;
    pLatLon: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    DBEdit7: TDBEdit;
    DBEdit8: TDBEdit;
    bbFlags: TBitBtn;
    bbClearMissing: TBitBtn;
    Label4: TLabel;
    pButtons: TPanel;
    Panel1: TPanel;
    pTitle: TPanel;
    pTreeView: TPanel;
    Panel2: TPanel;
    TreeView1: TTreeView;
    VirtualImageList1: TVirtualImageList;
    DBEdit9: TDBEdit;
    DBComboBox2: TDBComboBox;
    bbErrorsA: TBitBtn;
    Label5: TLabel;
    DBEdit10: TDBEdit;
    DBEdit11: TDBEdit;
    DBEdit12: TDBEdit;
    DBComboBox3: TDBComboBox;
    Label6: TLabel;
    DBEdit13: TDBEdit;
    DBEdit14: TDBEdit;
    Label7: TLabel;
    DBEdit16: TDBEdit;
    DBEdit15: TDBEdit;
    Label8: TLabel;
    procedure FormShow(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure bbEvapClick(Sender: TObject);
    procedure dbnEditClick(Sender: TObject; Button: TNavigateBtn);
    procedure bbRecalculateClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure bbPrecisionsClick(Sender: TObject);
    procedure bbCorrelationClick(Sender: TObject);
    procedure bbErrorsPCClick(Sender: TObject);
    procedure bbFlagsClick(Sender: TObject);
    procedure bbClearMissingClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure IsoRatCalcRatios(M : shortint);
  end;

var
  fmEdit: TfmEdit;

implementation

uses
  allsorts, gdw_varb, dmGdtmpDB, Gd_Evap1, Gd_prec, Gd_MSWD;

{$R *.DFM}

var
  iRec, iRecCount      : integer;

procedure TfmEdit.IsoRatCalcRatios(M : shortint);
begin
   if (dmGdwtmp.cdsEditYY.AsFloat<=0.0) then dmGdwtmp.cdsEditYY.AsFloat:=-1.0;
   case AnalType of
    '0' : tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    '1','2','7','9','E','F','G' : begin
      if ((CalcFac[M,1] > 0.000001) or (CalcFac[M,2] > 0.000001)) then
      begin
        tempratio1 := dmGdwtmp.cdsEditXX.AsFloat/dmGdwtmp.cdsEditYY.AsFloat
          *(CalcFac[M,1]+CalcFac[M,2]*dmGdwtmp.cdsEditYRatio.AsFloat);
      end;
    end;
    '3' : tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    '4' : begin
       tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    end;
    '5' : begin
       tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    end;
    '6' : begin
       tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    end;
    '8','A','H' : begin
       tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    end;
    'B','C','D' : begin
       tempratio1 := dmGdwtmp.cdsEditXRatio.AsFloat;
    end;
   end;
end;

procedure TfmEdit.TreeView1Click(Sender: TObject);
var
  tmpStr : string;
  i : integer;
  tx, ty : double;
begin
  i := TreeView1.Selected.AbsoluteIndex;
  tmpStr := TreeView1.Items.Item[i].Text;
  dmGdwtmp.cdsEdit.Locate('Sample_No',tmpStr,[]);
end;

procedure TfmEdit.FormShow(Sender: TObject);
var
  j      : integer;
  tmpStr : string;
  Node   : TTreeNode;
begin
  if (Title = '') then Title := ProjectName;
  lXXStr.Caption := Element[iAnalTyp,1];
  lYYStr.Caption := Element[iAnalTyp,2];
  lXStr.Caption := XRatioStr[iAnalTyp];
  lYStr.Caption := YRatioStr[iAnalTyp];
  lZStr.Caption := ZRatioStr[iAnalTyp];
  eTitle.Text := Title;
  bbEvap.Enabled := false;
  if (AnalType = 'H') then bbEvap.Enabled := true;
  if (N_Rep < 1) then N_Rep := 999;
  Str(N_Rep,tmpStr);
  eNRep.Text := tmpStr;
  with dmGdwtmp.cdsEdit do
  begin
    {
    MessageDlg('Emptying',mtInformation,[mbOK],0);
    }
    Active := true;
    dmGdwtmp.cdsEdit.EmptyDataSet;
  end;
  for j := 1 to NumberOfPoints do
  begin
    with dmGdwtmp.cdsEdit do
    begin
      Append;
      dmGdwtmp.cdsEditProject.AsString := '';
      dmGdwtmp.cdsEditSample_No.AsString := SmpNo[j];
      dmGdwtmp.cdsEditXX.AsFloat := Conc[j,1];
      dmGdwtmp.cdsEditYY.AsFloat := Conc[j,2];
      dmGdwtmp.cdsEditXRatio.AsFloat := Ratio[j,1];
      dmGdwtmp.cdsEditXPrec.AsFloat := XPrec[j];
      dmGdwtmp.cdsEditXWt.AsFloat := ErrorWt[j,1];
      dmGdwtmp.cdsEditYRatio.AsFloat := Ratio[j,2];
      dmGdwtmp.cdsEditYPrec.AsFloat := YPrec[j];
      dmGdwtmp.cdsEditYWt.AsFloat := ErrorWt[j,2];
      dmGdwtmp.cdsEditZRatio.AsFloat := Ratio[j,3];
      dmGdwtmp.cdsEditZPrec.AsFloat := ZPrec[j];
      dmGdwtmp.cdsEditZWt.AsFloat := ErrorWt[j,3];
      dmGdwtmp.cdsEditR.AsFloat := R[j];
      case ErrTyp[j] of
        '1' : begin
          dmGdwtmp.cdsEditXWtType.AsString := '%';
          dmGdwtmp.cdsEditYWtType.AsString := '%';
          dmGdwtmp.cdsEditZWtType.AsString := '%';
        end;
        '2' : begin
          dmGdwtmp.cdsEditXWtType.AsString := '%';
          dmGdwtmp.cdsEditYWtType.AsString := 'a';
          dmGdwtmp.cdsEditZWtType.AsString := 'a';
        end;
        '3' : begin
          dmGdwtmp.cdsEditXWtType.AsString := 'a';
          dmGdwtmp.cdsEditYWtType.AsString := '%';
          dmGdwtmp.cdsEditZWtType.AsString := 'a';
        end;
        '4' : begin
          dmGdwtmp.cdsEditXWtType.AsString := 'a';
          dmGdwtmp.cdsEditYWtType.AsString := 'a';
          dmGdwtmp.cdsEditZWtType.AsString := 'a';
        end;
      end;
      if (RFlg[j] = 'Y') then dmGdwtmp.cdsEditRFlag.AsString := 'Y'
                         else dmGdwtmp.cdsEditRFlag.AsString := 'N';
      if (PFlg[j] = 'Y') then dmGdwtmp.cdsEditPFlag.AsString := 'Y'
                         else dmGdwtmp.cdsEditPFlag.AsString := 'N';
      if ((Latitude[j] <> 0.0) and (Longitude[j] <> 0.0)) then
      begin
        dmGdwtmp.cdsEditLatitude.AsFloat := Latitude[j];
        dmGdwtmp.cdsEditLongitude.AsFloat := Longitude[j];
      end else
      begin
        dmGdwtmp.cdsEditLatitude.AsString := '';
        dmGdwtmp.cdsEditLongitude.AsString := '';
      end;
      Next;
    end;
  end;
  dmGdwtmp.cdsEdit.First;
  iRec := 1;
  iRecCount := dmGdwtmp.cdsEdit.RecordCount;
  eRec.Text := Int2Str(iRec);
  eRecCount.Text := Int2Str(iRecCount);
  TreeView1.Items.Clear;
  dmGdwtmp.cdsEdit.First;
  TreeView1.SetFocus;
  for j := 1 to NumberOfPoints do
  begin
    Node := TreeView1.Items.Add(TreeView1.Selected,dmGdwtmp.cdsEditSample_No.AsString);
    dmGdwtmp.cdsEdit.Next;
  end;
  dmGdwtmp.cdsEdit.First;
end;

procedure TfmEdit.bbClearMissingClick(Sender: TObject);
var
  i : integer;
  tX, tY : double;
begin
  dmGdwtmp.cdsEdit.First;
  for i := 1 to NumberOfPoints do
  begin
    tX := dmGdwtmp.cdsEditXRatio.AsFloat;
    tY := dmGdwtmp.cdsEditYRatio.AsFloat;
    dmGdwtmp.cdsEdit.Edit;
    if (tX = 0.0) or (tY = 0.0) then
    begin
      dmGdwtmp.cdsEditRFlag.AsString := 'N';
      dmGdwtmp.cdsEditPFlag.AsString := 'N';
      dmGdwtmp.cdsEdit.Post;
    end;
    dmGdwtmp.cdsEdit.Next;
  end;
end;

procedure TfmEdit.bbCloseClick(Sender: TObject);
var
  tmpStr : string;
  ICode  : integer;
  j      : integer;
begin
  ItemsHaveChanged := true;
  Title := eTitle.Text;
  tmpStr := eNRep.Text;
  Val(tmpStr,N_Rep,ICode);
  if ((N_Rep < 1) or (N_Rep > 999) or (ICode <> 0)) then
  begin
    ShowMessage('Fault converting # replicates. Data not stored');
    Exit;
  end;
  if (N_Rep < 1) then N_Rep := 999;
  if (N_Rep > 999) then N_Rep := 999;
  with dmGdwtmp.cdsEdit do
  begin
    //MessageDlg('First',mtInformation,[mbOK],0);
    First;
    j := 1;
    repeat
      SmpNo[j] := dmGdwtmp.cdsEditSample_No.AsString;
      Conc[j,1] := dmGdwtmp.cdsEditXX.AsFloat;
      Conc[j,2] := dmGdwtmp.cdsEditYY.AsFloat;
      Ratio[j,1] := dmGdwtmp.cdsEditXRatio.AsFloat;
      XPrec[j] := dmGdwtmp.cdsEditXPrec.AsFloat;
      ErrorWt[j,1] := dmGdwtmp.cdsEditXWt.AsFloat;
      Ratio[j,2] := dmGdwtmp.cdsEditYRatio.AsFloat;
      YPrec[j] := dmGdwtmp.cdsEditYPrec.AsFloat;
      ErrorWt[j,2] := dmGdwtmp.cdsEditYWt.AsFloat;
      Ratio[j,3] := dmGdwtmp.cdsEditZRatio.AsFloat;
      ZPrec[j] := dmGdwtmp.cdsEditZPrec.AsFloat;
      ErrorWt[j,3] := dmGdwtmp.cdsEditZWt.AsFloat;
      R[j] := dmGdwtmp.cdsEditR.AsFloat;
      if ((dmGdwtmp.cdsEditXWtType.AsString = '%') and
          (dmGdwtmp.cdsEditYWtType.AsString = '%')) then ErrTyp[j] := '1';
      if ((dmGdwtmp.cdsEditXWtType.AsString = '%') and
          (dmGdwtmp.cdsEditYWtType.AsString = 'a')) then ErrTyp[j] := '2';
      if ((dmGdwtmp.cdsEditXWtType.AsString = 'a') and
          (dmGdwtmp.cdsEditYWtType.AsString = '%')) then ErrTyp[j] := '3';
      if ((dmGdwtmp.cdsEditXWtType.AsString = 'a') and
          (dmGdwtmp.cdsEditYWtType.AsString = 'a')) then ErrTyp[j] := '4';
      if (dmGdwtmp.cdsEditRFlag.AsString = 'Y') then RFlg[j] := 'Y'
                                                else RFlg[j] := 'N';
      if (dmGdwtmp.cdsEditPFlag.AsString = 'Y') then PFlg[j] := 'Y'
                                                else PFlg[j] := 'N';
      if ((dmGdwtmp.cdsEditLatitude.AsString <> '') and
          (dmGdwtmp.cdsEditLongitude.AsString <> '')) then
      begin
        Latitude[j] := dmGdwtmp.cdsEditLatitude.AsFloat;
        Longitude[j] := dmGdwtmp.cdsEditLongitude.AsFloat;
      end else
      begin
        Latitude[j] := 0.0;
        Longitude[j] := 0.0;
      end;
      Next;
      j := j + 1;
    until dmGdwtmp.cdsEdit.EOF;
  end;
  NumberOfPoints := j - 1;
  dmGdwtmp.cdsEdit.First;
  dmGdwtmp.cdsEdit.Active := false;
  ModalResult := mrOK;
end;

procedure TfmEdit.bbCancelClick(Sender: TObject);
begin
  dmGdwtmp.cdsEdit.Active := false;
  ModalResult := mrCancel;
end;

procedure TfmEdit.bbEvapClick(Sender: TObject);
begin
  try
    fmZrEvap := TfmZrEvap.Create(Self);
    fmZrEvap.ShowModal;
    if (fmZrEvap.ModalResult = mrOK) then
    begin
    end;
  finally
    fmZrEvap.Free;
  end;
end;

procedure TfmEdit.bbFlagsClick(Sender: TObject);
var
  i : integer;
begin
  dmGdwtmp.cdsEdit.First;
  for i := 1 to NumberOfPoints do
  begin
    dmGdwtmp.cdsEdit.Edit;
    dmGdwtmp.cdsEditRFlag.AsString := 'Y';
    dmGdwtmp.cdsEditPFlag.AsString := 'Y';
    dmGdwtmp.cdsEdit.Post;
    dmGdwtmp.cdsEdit.Next;
  end;
end;

procedure TfmEdit.dbnEditClick(Sender: TObject; Button: TNavigateBtn);
begin
  case Button of
    nbFirst  : iRec := 1;
    nbPrior  : begin
      iRec := iRec - 1;
      if (iRec < 1) then iRec := 1;
    end;
    nbNext   : begin
      iRec := iRec + 1;
      if (iRec > dmGdwtmp.cdsEdit.RecordCount)
        then iRec := dmGdwtmp.cdsEdit.RecordCount;
    end;
    nbLast   : iRec := dmGdwtmp.cdsEdit.RecordCount;
    nbInsert : begin
      iRecCount := iRecCount + 1;
      iRec := iRec + 1;
    end;
    nbDelete : begin
      iRecCount := iRecCount - 1;
      iRec := iRec - 1;
      if (iRec < 1) then iRec := 1;
    end;
    nbCancel : begin
      iRecCount := dmGdwtmp.cdsEdit.RecordCount;
    end;
  end;
  eRec.Text := Int2Str(iRec);
  eRecCount.Text := Int2Str(iRecCount);
end;

procedure TfmEdit.bbRecalculateClick(Sender: TObject);
var
  temp : double;
begin
   IsoRatCalcRatios(iAnalTyp);
   if tempratio1<=0.000001 then tempratio1:=0.000001;
   temp := tempratio1;
   if ((Abs(dmGdwtmp.cdsEditXRatio.AsFloat-tempratio1))
     >(0.001*dmGdwtmp.cdsEditXRatio.AsFloat)) then
   begin
     if MessageDlg('Accept new value for '+XRatioStr[iAnalTyp]+' = '
          +Real2Str(temp,15,6),mtConfirmation,[mbYes,mbNo],0) = mrYes then
     begin
       dmGdwtmp.cdsEdit.Edit;
       dmGdwtmp.cdsEditXRatio.AsFloat := tempratio1;
       dmGdwtmp.cdsEdit.Post;
     end;
   end;
end;

procedure TfmEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  vk_PgUp       = 33;
  vk_PgDn       = 34;
  vk_End        = 35;
  vk_Home       = 36;
  vk_LeftArrow  = 37;
  vk_UpArrow    = 38;
  vk_RightArrow = 39;
  vk_DownArrow  = 40;
  vk_Insert     = 45;
  vk_Delete     = 46;
begin
  case Key of
    vk_Home : begin
      {
      dmGdwtmp.cdsEdit.First;
      iRec := 1;
      }
    end;
    vk_PgUp, vk_UpArrow : begin
      dmGdwtmp.cdsEdit.Prior;
      iRec := iRec - 1;
      if (iRec < 1) then iRec := 1;
    end;
    vk_PgDn, vk_DownArrow : begin
      dmGdwtmp.cdsEdit.Next;
      iRec := iRec + 1;
      if (iRec > dmGdwtmp.cdsEdit.RecordCount)
        then iRec := dmGdwtmp.cdsEdit.RecordCount;
    end;
    vk_End : begin
      {
      dmGdwtmp.cdsEdit.Last;
      iRec := dmGdwtmp.cdsEdit.RecordCount;
      }
    end;
  end;
  eRec.Text := Int2Str(iRec);
  eRecCount.Text := Int2Str(iRecCount);
end;

procedure TfmEdit.bbPrecisionsClick(Sender: TObject);
var
  frmPrec : TfmPrecision;
begin
  try
    frmPrec := TfmPrecision.Create(self);
    frmPrec.ShowModal;
    if (frmPrec.ModalResult = mrOK) then
    begin
      if (not frmPrec.PrecAll) then
      begin
        dmGdwtmp.cdsEdit.Edit;
        case frmPrec.PrecVar of
          'X' : begin
             if ((dmGdwtmp.cdsEditXPrec.AsFloat>0.0) and
                 (dmGdwtmp.cdsEditXRatio.AsFloat>0.0))then
             begin
               dmGdwtmp.cdsEdit.Edit;
               dmGdwtmp.cdsEditXWt.AsFloat :=
                  (100.0/frmPrec.PrecMult*dmGdwtmp.cdsEditXPrec.AsFloat)
                        /dmGdwtmp.cdsEditXRatio.AsFloat;
               dmGdwtmp.cdsEditXWtType.AsString := '%';
             end;
          end;
          'Y' : begin
             if ((dmGdwtmp.cdsEditYPrec.AsFloat>0.0) and
                 (dmGdwtmp.cdsEditYRatio.AsFloat>0.0))then
             begin
               dmGdwtmp.cdsEdit.Edit;
               dmGdwtmp.cdsEditYWt.AsFloat :=
                  (100.0/frmPrec.PrecMult*dmGdwtmp.cdsEditYPrec.AsFloat)
                        /dmGdwtmp.cdsEditYRatio.AsFloat;
               dmGdwtmp.cdsEditYWtType.AsString := '%';
             end;
          end;
          'Z' : begin
             if ((dmGdwtmp.cdsEditZPrec.AsFloat>0.0) and
                 (dmGdwtmp.cdsEditZRatio.AsFloat>0.0))then
             begin
               dmGdwtmp.cdsEdit.Edit;
               dmGdwtmp.cdsEditZWt.AsFloat :=
                  (100.0/frmPrec.PrecMult*dmGdwtmp.cdsEditZPrec.AsFloat)
                        /dmGdwtmp.cdsEditZRatio.AsFloat;
               dmGdwtmp.cdsEditZWtType.AsString := '%';
             end;
          end;
        end;
        dmGdwtmp.cdsEdit.Post;
      end;
      if (frmPrec.PrecAll) then
      begin
        dmGdwtmp.cdsEdit.First;
        repeat
          dmGdwtmp.cdsEdit.Edit;
          case frmPrec.PrecVar of
            'X' : begin
               if ((dmGdwtmp.cdsEditXPrec.AsFloat>0.0) and
                   (dmGdwtmp.cdsEditXRatio.AsFloat>0.0))then
               begin
                 dmGdwtmp.cdsEdit.Edit;
                 dmGdwtmp.cdsEditXWt.AsFloat :=
                    (100.0/frmPrec.PrecMult*dmGdwtmp.cdsEditXPrec.AsFloat)
                          /dmGdwtmp.cdsEditXRatio.AsFloat;
                 dmGdwtmp.cdsEditXWtType.AsString := '%';
               end;
            end;
            'Y' : begin
               if ((dmGdwtmp.cdsEditYPrec.AsFloat>0.0) and
                   (dmGdwtmp.cdsEditYRatio.AsFloat>0.0))then
               begin
                 dmGdwtmp.cdsEdit.Edit;
                 dmGdwtmp.cdsEditYWt.AsFloat :=
                    (100.0/frmPrec.PrecMult*dmGdwtmp.cdsEditYPrec.AsFloat)
                          /dmGdwtmp.cdsEditYRatio.AsFloat;
                 dmGdwtmp.cdsEditYWtType.AsString := '%';
               end;
            end;
            'Z' : begin
               if ((dmGdwtmp.cdsEditZPrec.AsFloat>0.0) and
                   (dmGdwtmp.cdsEditZRatio.AsFloat>0.0))then
               begin
                 dmGdwtmp.cdsEdit.Edit;
                 dmGdwtmp.cdsEditZWt.AsFloat :=
                    (100.0/frmPrec.PrecMult*dmGdwtmp.cdsEditZPrec.AsFloat)
                          /dmGdwtmp.cdsEditZRatio.AsFloat;
                 dmGdwtmp.cdsEditZWtType.AsString := '%';
               end;
            end;
          end;
          dmGdwtmp.cdsEdit.Post;
          dmGdwtmp.cdsEdit.Next;
        until dmGdwtmp.cdsEdit.EOF;
      end;
    end;
  finally
    frmPrec.Free;
  end;
end;

procedure TfmEdit.bbCorrelationClick(Sender: TObject);
var
  t1, temp : double;
begin
  if CharInSet(AnalType,['3','8']) then
  begin
    t1:=(2.0*dmGdwtmp.cdsEditXPrec.AsFloat/dmGdwtmp.cdsEditXRatio.AsFloat)
      *(dmGdwtmp.cdsEditYPrec.AsFloat/dmGdwtmp.cdsEditYRatio.AsFloat);
    if t1>0.0 then
    begin
      temp:=(dmGdwtmp.cdsEditXPrec.AsFloat/dmGdwtmp.cdsEditXRatio.AsFloat)
        *(dmGdwtmp.cdsEditXPrec.AsFloat/dmGdwtmp.cdsEditXRatio.AsFloat);
      temp:=temp+(dmGdwtmp.cdsEditYPrec.AsFloat/dmGdwtmp.cdsEditYRatio.AsFloat)
        *(dmGdwtmp.cdsEditYPrec.AsFloat/dmGdwtmp.cdsEditYRatio.AsFloat);
      temp:=temp-(dmGdwtmp.cdsEditZPrec.AsFloat/dmGdwtmp.cdsEditZRatio.AsFloat)
        *(dmGdwtmp.cdsEditZPrec.AsFloat/dmGdwtmp.cdsEditZRatio.AsFloat);
      tempratio1:=temp/t1;
      if (Abs(tempratio1) > 1.0) then
      begin
        dmGdwtmp.cdsEdit.Edit;
        if MessageDlg('New correlation coefficient is too large. Set to zero?',
             mtConfirmation,[mbYes,mbNo],0) = mrYes then
        begin
          dmGdwtmp.cdsEditR.AsFloat := 0.0;
        end;
        dmGdwtmp.cdsEdit.Post;
      end
      else begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditR.AsFloat := tempratio1;
        dmGdwtmp.cdsEdit.Post;
      end;
    end;
  end;
end;


procedure TfmEdit.bbErrorsPCClick(Sender: TObject);
var
  i : integer;
begin
  dmGdwtmp.cdsEdit.First;
  if (Sender = bbErrorsPC) then
  begin
    for i := 1 to NumberOfPoints do
    begin
      if (dmGdwtmp.cdsEditXWtType.AsString = '') then
      begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditXWtType.AsString := '%';
        dmGdwtmp.cdsEdit.Post;
      end;
      if (dmGdwtmp.cdsEditYWtType.AsString = '') then
      begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditYWtType.AsString := '%';
        dmGdwtmp.cdsEdit.Post;
      end;
      if (dmGdwtmp.cdsEditZWtType.AsString = '') then
      begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditZWtType.AsString := '%';
        dmGdwtmp.cdsEdit.Post;
      end;
      dmGdwtmp.cdsEdit.Next;
    end;
  end;
  if (Sender = bbErrorsA) then
  begin
    for i := 1 to NumberOfPoints do
    begin
      if (dmGdwtmp.cdsEditXWtType.AsString = '') then
      begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditXWtType.AsString := 'a';
        dmGdwtmp.cdsEdit.Post;
      end;
      if (dmGdwtmp.cdsEditYWtType.AsString = '') then
      begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditYWtType.AsString := 'a';
        dmGdwtmp.cdsEdit.Post;
      end;
      if (dmGdwtmp.cdsEditZWtType.AsString = '') then
      begin
        dmGdwtmp.cdsEdit.Edit;
        dmGdwtmp.cdsEditZWtType.AsString := 'a';
        dmGdwtmp.cdsEdit.Post;
      end;
      dmGdwtmp.cdsEdit.Next;
    end;
  end;
  dmGdwtmp.cdsEdit.First;
end;

end.


