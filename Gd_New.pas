unit Gd_New;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DBCtrls, Grids, DBGrids, Data.DB,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  ImageCollection_dm;

type
  TfmNewData = class(TForm)
    Panel1: TPanel;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    cbxIsoSys: TComboBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    eXWt: TEdit;
    eYWt: TEdit;
    cbxXWtType: TComboBox;
    cbxYWtType: TComboBox;
    Label3: TLabel;
    eR: TEdit;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    VirtualImageList1: TVirtualImageList;
    procedure bbCancelClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxIsoSysExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmNewData: TfmNewData;

implementation

{$R *.DFM}

uses
  Gdw_varb, dmGdtmpDB;

var
  iRec, iRecCount      : integer;

procedure TfmNewData.bbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TfmNewData.bbOKClick(Sender: TObject);
var
  j     : integer;
  ICode : integer;
begin
  with dmGdwtmp.cdsNew do
  begin
    First;
    j := 1;
    repeat
      SmpNo[j] := dmGdwtmp.cdsNewSample_No.AsString;
      Conc[j,1] := 0.0;
      Conc[j,2] := 0.0;
      Ratio[j,1] := 0.0;
      XPrec[j] := 0.0;
      Val(eXWt.Text,ErrorWt[j,1],Icode);
      Ratio[j,2] := 0.0;
      YPrec[j] := 0.0;
      Val(eYWt.Text,ErrorWt[j,2],Icode);
      Ratio[j,3] := 0.0;
      ZPrec[j] := 0.0;
      Val(eR.Text,R[j],Icode);
      if ((cbxXwtType.Text = '%') and
          (cbxYwtType.Text = '%')) then ErrTyp[j] := '1';
      if ((cbxXwtType.Text = '%') and
          (cbxYwtType.Text = 'a')) then ErrTyp[j] := '2';
      if ((cbxXwtType.Text = 'a') and
          (cbxYwtType.Text = '%')) then ErrTyp[j] := '3';
      if ((cbxXwtType.Text = 'a') and
          (cbxYwtType.Text = 'a')) then ErrTyp[j] := '4';
      RFlg[j] := 'Y';
      PFlg[j] := 'Y';
      Next;
      j := j + 1;
    until dmGdwtmp.cdsNew.EOF;
  end;
  NumberOfPoints := j - 1;
  dmGdwtmp.cdsNew.First;
  dmGdwtmp.cdsNew.Active := false;
  ModalResult := mrOk;
end;

procedure TfmNewData.FormShow(Sender: TObject);
var
  i : integer;
begin
  cbxIsoSys.Items.Clear;
  for i := 0 to 17 do
  begin
    cbxIsoSys.Items.Add(Process[i]);
  end;
  Title := '';
  N_Rep := 999;
  with dmGdwtmp.cdsNew do
  begin
    EmptyDataset;
    Active := true;
    First;
    iRec := 1;
    iRecCount := dmGdwtmp.cdsNew.RecordCount;
  end;
end;

procedure TfmNewData.cbxIsoSysExit(Sender: TObject);
begin
  IAnalTyp := cbxIsoSys.ItemIndex;
  case IAnalTyp of
    0 : AnalType := '0';
    1 : AnalType := '1';
    2 : AnalType := '2';
    3 : AnalType := '3';
    4 : AnalType := '4';
    5 : AnalType := '5';
    6 : AnalType := '6';
    7 : AnalType := '7';
    8 : AnalType := '8';
    9 : AnalType := '9';
    10 : AnalType := 'A';
    11 : AnalType := 'B';
    12 : AnalType := 'C';
    13 : AnalType := 'D';
    14 : AnalType := 'E';
    15 : AnalType := 'F';
    16 : AnalType := 'G';
    17 : AnalType := 'H';
  end;
end;

end.
