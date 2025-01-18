unit Gd_Ccda;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBCtrls, ExtCtrls, Grids, DBGrids, Printers, OleCtrls,
  Mask, AxCtrls, VclTee.TeeGDIPlus, VCLTee.Series, VCLTee.TeEngine,
  VCLTee.TeeComma, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.TeeErrorPoint,
  VCLTee.TeeSpline, VCLTee.TeeTools, VCL.Themes, GD_MSWD,
  VCL.FlexCel.Core, FlexCel.XlsAdapter, Vcl.ComCtrls, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, SVGIconVirtualImageList, VCLTee.TeeEdit;

type
  TfmConcordiaDate = class(TForm)
    pButtonsTop: TPanel;
    pResultLeft: TPanel;
    pResultsBottom: TPanel;
    eTitle: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lWtAvAugmentedSD: TLabel;
    eDateWO: TEdit;
    Label6: TLabel;
    eIcnt: TEdit;
    Label7: TLabel;
    eXCentroid: TEdit;
    lWtAvPlus: TLabel;
    eDatePlusMinusWO: TEdit;
    eDatePlusMinusW: TEdit;
    lWtAvMinus: TLabel;
    Label10: TLabel;
    edfEquivalence: TEdit;
    eYCentroid: TEdit;
    Label11: TLabel;
    eProbEquivalence: TEdit;
    Label12: TLabel;
    eMSWDEquivalence: TEdit;
    eDateW: TEdit;
    eProbWO: TEdit;
    lErrorsBased: TLabel;
    lResultTitle: TLabel;
    pGraphRight: TPanel;
    SaveDialogModels: TSaveDialog;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    eProbW: TEdit;
    eMSWDwo: TEdit;
    edfWO: TEdit;
    eMSWDw: TEdit;
    edfW: TEdit;
    Label13: TLabel;
    lNotApplicable: TLabel;
    ePb76: TEdit;
    Label14: TLabel;
    eSigma75: TEdit;
    eSigma68: TEdit;
    eSigma76: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    eFe: TEdit;
    eFwo: TEdit;
    eFw: TEdit;
    Label17: TLabel;
    DBEdit3: TDBEdit;
    DBCheckBox1: TDBCheckBox;
    DBNavigator1: TDBNavigator;
    lEllipseMagnif: TLabel;
    lResidual: TLabel;
    pGraphs: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Splitter1: TSplitter;
    ChartConcordia: TChart;
    TeeCommander1: TTeeCommander;
    Series1: TLineSeries;
    Series2: TPointSeries;
    Series3: TPointSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    Series6: TLineSeries;
    Series7: TPointSeries;
    Series8: TPointSeries;
    Series9: TLineSeries;
    Series10: TLineSeries;
    Series11: TLineSeries;
    Series12: TPointSeries;
    cbCurrentSample: TCheckBox;
    cbLegend: TCheckBox;
    cbTicLabels: TCheckBox;
    Series13: TPointSeries;
    Series14: TPointSeries;
    Label18: TLabel;
    eTicksEvery: TEdit;
    lModifyGraphSettings: TLabel;
    eNsamp: TEdit;
    Label19: TLabel;
    Splitter4: TSplitter;
    pTreeSmp: TPanel;
    TreeView1: TTreeView;
    bClose: TButton;
    bSprdSheet: TButton;
    bCumHist: TButton;
    bExport: TButton;
    bRecalculate: TButton;
    TeeFunction1: TSmoothingFunction;
    lMaxAgeConcordia: TLabel;
    eMaxAgeConcordia: TEdit;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    Series15: TLineSeries;
    Series16: TLineSeries;
    ChartEditor1: TChartEditor;
    Label20: TLabel;
    Edit1: TEdit;
    Label21: TLabel;
    eTicFormat: TEdit;
    procedure FormShow(Sender: TObject);
    procedure bbSprdSheetClick(Sender: TObject);
    procedure bbStoreMdlClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
    procedure bbRecalculateClick(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure cbLegendClick(Sender: TObject);
    procedure cbCurrentSampleClick(Sender: TObject);
    procedure cbTicLabelsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure ChartConcordiaClick(Sender: TObject);
    procedure ChartConcordiaDblClick(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
    procedure ChartConcordiaZoom(Sender: TObject);
  private
    //Private declarations
    //  0 = X-Y general
    //  1 = Rb-Sr
    //  2 = Sm-Nd
    //  3 = Pb-Pb
    //  4 = 238U-Pb
    //  5 = 235U-Pb
    //  6 = Th-Pb
    //  7 = Lu-Hf
    //  8 = Concordia
    //  9 = La-Ce
    //  A 10 = Tera-Wasserburg
    //  B 11 = K-Ar
    //  C 12 = Ar-Ar
    //  D 13 = Ar inverse
    //  E 14 = K-Ca
    //  F 15 = Re-Os
    //  G 16 = La-Ba
    //  H 17 = Evaporation Pb
    //  I 18 = Ar plateau
    //  J 19 = T(2DM) from Age-Epsilon values
    CMaxX, CMinX : double;
    ChartRow : integer;
    RowNumber : integer;
    NumberOfGraphRows : integer;
    XMax,XMin,YMax,YMin : double;
    SprdSheet : TXlsFile;
    procedure UpdateRFlg(Sender: TObject);
    procedure FillRegTable(Sender: TObject);
  public
    { Public declarations }
    HistOK  : boolean;
    VarbNoX : integer;
    AllSame : boolean;
    FileVarStr : string;
    procedure Choose_Wt_Field;
    procedure SprdSheetEllipse ( i : integer);
    procedure DrawEllipse ( i : integer);
    procedure SetAllChartData;
  end;

var
  fmConcordiaDate: TfmConcordiaDate;
  MSWDForm     : TfmMSWD;

implementation

{$R *.DFM}

uses
  GDW_varb, Allsorts, gd_drv, dmGdtmpDB,
  GDW_regp, gd_HstVl, mathproc,
  TeePNG, TeeSVGCanvas, VCLTee.TeeThemes,
  TeeJPEG, TeExport;

//const
//  VtChAxisIdX = 0;
//  VtChAxisIdY = 1;

var
  SD1, SD2   : double;
  iRec, iRecCount         : integer;

procedure TfmConcordiaDate.UpdateRFlg(Sender: TObject);
var
  i      : integer;
  tmpStr : string; //string[1]
begin
  with dmGdwtmp do
  begin
     cdsReg.First;
     for i := 1 to NumberOfPoints do
     begin
       tmpStr := cdsRegRFlag.AsString;
       if ((tmpStr = 'Y') or (tmpStr = 'y')) then RFlg[i] := 'Y'
                                             else RFlg[i] := 'N';
       cdsReg.Next;
     end;
     cdsReg.First;
  end;
end;

procedure TfmConcordiaDate.FillRegTable(Sender: TObject);
var
  i      : integer;
  tmpStr : string; //string[1]
begin
  with dmGdwtmp.cdsReg do
  begin
    Active := true;
    EmptyDataset;
  end;
  with dmGdwtmp do
  begin
     for i := 1 to NumberOfPoints do
     begin
         cdsReg.Append;
         cdsRegSample_No.AsString := SmpNo[i];
         if (RFlg[i] = 'Y') then tmpStr := 'Y'
                            else tmpStr := 'N';
         cdsRegRFlag.AsString := tmpStr;
         cdsRegXWt.AsFloat := ErrorWt[i,1];
         if (ErrTyp[i] in ['1','2']) then cdsRegXWtType.AsString := '%'
                                     else cdsRegXWtType.AsString := 'a';
         cdsRegYWt.AsFloat := ErrorWt[i,2];
         if (ErrTyp[i] in ['1','3']) then cdsRegYWtType.AsString := '%'
                                     else cdsRegYWtType.AsString := 'a';
         cdsRegXDev.AsFloat := Residual[i,1];
         if (PFlg[i] = 'Y') then cdsRegPFlag.AsString := 'Y'
                            else cdsRegPFlag.AsString := 'N';
         cdsRegProject.AsString := ProjectName;
         cdsRegXRatio.AsFloat := Xtra[i];
         cdsRegYRatio.AsFloat := Xtra3[i];
         cdsRegi.AsInteger := i;
         cdsReg.Next;
     end;
     cdsReg.First;
     iRec := 1;
  end;
  {
  if ((Residual[iRec,1] > (2.5 * ErrorWt[iRec,1]))) then
  begin
    lResidual.Visible := true;
  end else
  begin
    lResidual.Visible := false;
  end;
  }
end;

procedure TfmConcordiaDate.FormShow(Sender: TObject);
var
  iCode : integer;
  i, j      : integer;
  tmpStr    : string;
  temp      : double;
  Idf       : integer;
  tmpSingle, tmpAge : double;
  MaxAge    : double;
  t1, t2, t3, t4 : double;
  SD3       : double;
  tx, ty, tEx, tEy, tr : double;
  ProbabilityOfFitE : double;
  tmpX, tmpY, tmpZ : double;
  Range : double;
  tMsum : double;
  Node   : TTreeNode;
  MyArray : array of TPoint;
begin
  //TSystemTheme.ApplyStyle(ChartConcordia);
  UprIntercept := 4500.0;
  MaxAge := 4500.0;
  MaxAgeConcordia := 4500.0;
  eTicksEvery.Text := FormatFloat('###0.0',TicksEvery);
  Val(eTicksEvery.Text,TicksEvery,iCode);
  if (iCode <> 0) then TicksEvery := 10.0;
  eTicksEvery.Text := FormatFloat('###0.0',TicksEvery);
  Val(eMaxAgeConcordia.Text,MaxAgeConcordia,iCode);
  if (iCode <>0) then MaxAgeConcordia := 4500.0;
  eMaxAgeConcordia.Text := FormatFloat('###0.0',MaxAgeConcordia);
  try
    MSWDForm := TfmMSWD.Create(Self);
    MSWDForm.ExitBtnClick(Sender);
  finally
    MSWDForm.Free;
  end;
  with dmGdwtmp.cdsReg do
  begin
    Active := false;
  end;
  if (EllipseMagnif = 1.0) then lEllipseMagnif.Caption := 'Ellipses are 1 sigma';
  if (EllipseMagnif > 1.0) then lEllipseMagnif.Caption := 'Ellipses are 95% confidence';
  lNotApplicable.Visible := false;
  bExport.Enabled := false;
  eTitle.Text := Title;
  ChartConcordia.Title.Caption := Title;
  CMaxX := 0.0;
  CMinX := 0.0;
  T_Mult:=TMultiplier(1.0*N_Rep);
  FillRegTable(Sender);
  SetAllChartData;
  // example Chart1.Title.TextFormat:=ttfHtml;
  // example Chart1.Title.Text.Text:='Theta: ' + WideChar($0398) + '<sub>subscript</sub>' + '<sup>superscript</sup>';
  ChartConcordia.BottomAxis.Title.TextFormat:=ttfHtml;
  ChartConcordia.BottomAxis.Title.Text := '<sup>207</sup>Pb<sup>*</sup>/<sup>235</sup>U';
  ChartConcordia.LeftAxis.Title.TextFormat:=ttfHtml;
  ChartConcordia.LeftAxis.Title.Text := '<sup>206</sup>Pb<sup>*</sup>/<sup>238</sup>U';
  if (AnalType = '8') then
  begin
    ChartConcordia.BottomAxis.Title.Text := '<sup>207</sup>'+'Pb'+'<sup>*</sup>'+'/'+'<sup>235</sup>'+'U';
    ChartConcordia.LeftAxis.Title.Text := '<sup>206</sup>'+'Pb'+'<sup>*</sup>'+'/'+'<sup>238</sup>'+'U';
  end;
  if ((AnalType = 'A') or (temporaryAnalType = 'A')) then
  begin
    ChartConcordia.BottomAxis.Title.Text := '<sup>238</sup>U/<sup>206</sup>Pb<sup>*</sup>';
    ChartConcordia.LeftAxis.Title.Text := '<sup>207</sup>Pb<sup>*</sup>/<sup>206</sup>Pb<sup>*</sup>';
  end;
  ChartConcordia.Foot.Caption := GeodateVersionStr;
  if (EllipseMagnif = 1.0) then ChartConcordia.SubFoot.Caption := '1 sigma uncertainties';
  if (EllipseMagnif > 1.0) then ChartConcordia.SubFoot.Caption := '95% conf. uncertainties';
  TreeView1.Items.Clear;
  dmGdwtmp.cdsReg.First;
  //Node := TreeView1.Items.Add(TreeView1.Selected,'Data');
  TreeView1.SetFocus;
  for i := 1 to NumberOfPoints do
  begin
    Node := TreeView1.Items.Add(TreeView1.Selected,dmGdwtmp.cdsRegSample_No.AsString);
    //Node := TreeView1.Items.AddChild(TreeView1.Selected,dmGdwtmp.cdsRegRFlag.AsString);
    //Node := TreeView1.Items.AddChild(TreeView1.Selected,dmGdwtmp.cdsRegPFlag.AsString);
    dmGdwtmp.cdsReg.Next;
  end;
  dmGdwtmp.cdsReg.First;
end;

procedure TfmConcordiaDate.SetAllChartData;
var
  iCode : integer;
  i, j      : integer;
  tmpStr    : string;
  temp      : double;
  Idf       : integer;
  tmpSingle, tmpAge : double;
  MaxAge    : double;
  t1, t2, t3, t4 : double;
  SD3       : double;
  tx, ty, tEx, tEy, tr : double;
  ProbabilityOfFitE : double;
  tmpX, tmpY, tmpZ : double;
  tmpXp, tmpYp, tmpXm, tmpYm : double;
  Range : double;
  tMsum : double;
  Node   : TTreeNode;
  MyArray : array of TPoint;
  TMult : double;
begin
  CMaxX := 0.0;
  CMinX := 0.0;
  T_Mult:=TMultiplier(1.0*N_Rep);
  Choose_Wt_Field;
  if ((VarbNoX in [9]) and (HistOK)) then
  begin
    case VarbNoX of
      9 : begin
        if CharInSet(AnalType, ['8','A']) then
        begin
          lResultTitle.Caption := 'Mean T (concordia)';
          FileVarStr := 'Mean T concordia';
          bExport.Enabled := true;
        end;
        {
        if (AllSame) then VtChWtAv.Visible := false
                     else VtChWtAv.Visible := true;
        }
      end;
    end;
    if (VarbNoX in [9]) then
    begin
      Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
      lErrorsBased.Caption := 'Date errors based on expected s.d. of mean and t = '
        +tmpStr;
      WtAverConcordia(NumberOfPoints,SD1,SD2,SD3,Icnt,MSWDe,Idf,ProbE,
              DateWO,VarDateWO,MSWDwo,Probwo,
              DateW,VarDateW,MSWDw,Probw);
      //ShowMessage('Idf = '+Int2Str(Idf));
      NumberOfPointsRegressed := Icnt;
      eXCentroid.Text := FormatFloat(' ##0.000000',XCentroid);
      eYCentroid.Text := FormatFloat('  #0.000000',YCentroid);
      ePb76.Text := FormatFloat('  0.000000',(XCentroid/YCentroid)/U238U235);
      eSigma75.Text := FormatFloat(' ##0.000000',Sqrt(SD1));
      eSigma68.Text := FormatFloat('  #0.000000',Sqrt(SD2));
      Xtra[0] := XCentroid;
      Xtra3[0] := YCentroid;
      Xtra1[0] := Sqrt(SD1);
      Xtra2[0] := Sqrt(SD2);
      RFlg[0] := 'Y';
      PFlg[0] := 'Y';
      for i := 1 to NumberOfPoints do
      begin
        RR[i] := R[i];
      end;
      RR[0] := SD3/(Sqrt(SD1)*Sqrt(SD2));
      t1 := Sqrt(SD1)/XCentroid;
      t2 := Sqrt(SD2)/YCentroid;
      t4 := 2.0*t1*t2;
      t3 := t1*t1 + t2*t2 - RR[0]*t4;
      t3 := Sqrt(t3);
      t4 := t3*(XCentroid/YCentroid)/U238U235;
      eSigma76.Text := FormatFloat('  0.000000',t4);
      eIcnt.Text := IntToStr(Icnt);
      eNsamp.Text := IntToStr(NumberOfPoints);
      eMSWDEquivalence.Text := FormatFloat('###0.000',MSWDe);
      edfEquivalence.Text := IntToStr(Idf);
      ProbabilityOfFitE := ProbabilityOfF(Idf,N_Rep,MSWDe,1);
      eProbEquivalence.Text := FormatFloat(' 0.000',ProbabilityOfFitE);
      if (ProbabilityOfFitE < FAlpha) then eProbEquivalence.Font.Color := clRed
                                      else eProbEquivalence.Font.Color := clBlue;
      if (Idf <= 0) then
      begin
         MsumCutOff:=1;
      end else
      begin
         if (Idf >= NumStatisticsValues) then
         begin
           MsumCutoff:=MSUM_Val[NumStatisticsValues];
         end else
         begin
           MsumCutoff:=MSUM_Val[Idf];
         end;
      end;
      if (MsumCutoff = 0.0) then MsumCutOff := 1.00;
      eFe.Text := FormatFloat(' ##0.000',MsumCutOff);
      //MsumCutoff:=MSUM_Val[Idf];
//      ShowMessage('Idf = '+Int2Str(Idf));
//      ShowMessage('MsumCutoff = '+FormatFloat('###0.00000',MsumCutoff));
//      ShowMessage('MSWDe = '+FormatFloat('###0.00000',MSWDe));
//      ShowMessage('Idf-2 = '+Int2Str(Idf-2));
//      ShowMessage('Msum_Val = '+FormatFloat('###0.00000',Msum_Val[Idf-2]));
//      ShowMessage('Idf-1 = '+Int2Str(Idf-1));
//      ShowMessage('Msum_Val = '+FormatFloat('###0.00000',Msum_Val[Idf-1]));
      {
      for i := 1 to 20 do
      begin
        ShowMessage('i = '+Int2Str(i));
        ShowMessage('Msum_Val = '+FormatFloat('###0.00000',Msum_Val[i]));
      end;
      }
      if (MSWDe > MsumCutoff) then
      begin
        lNotApplicable.Font.Color := clRed;
        lNotApplicable.Visible := true;
      end;
      eDateWO.Text := FormatFloat(' ###0.0',DateWO/1.0e6);
      eDatePlusMinusWO.Text := FormatFloat(' ###0.0',T_Mult*Sqrt(VarDateWO)/1.0e6);
      eMSWDwo.Text := FormatFloat('###0.000',MSWDwo);
      edfWO.Text := IntToStr(Idf+1);
      if (Idf+1 <= 0) then
      begin
         tMsum:=1;
      end else
      begin
         if (Idf+1 >= NumStatisticsValues) then
         begin
           tMsum:=MSUM_Val[NumStatisticsValues];
         end else
         begin
           tMsum:=MSUM_Val[Idf+1];
         end;
      end;
      if (tMsum = 0.0) then tMsum := 1.00;
      eFwo.Text := FormatFloat(' ##0.000',tMsum);
      //eFwo.Text := FormatFloat(' ##0.000',MSUM_Val[Idf+1]);
      eProbWO.Text := '';
      ProbabilityOfFitWO := ProbabilityOfF(Idf+1,N_Rep,MSWDwo,1);
      eProbWO.Text := FormatFloat(' 0.000',ProbabilityOfFitWO);
      if (ProbabilityOfFitWO < FAlpha) then eProbWO.Font.Color := clRed
                                     else eProbWO.Font.Color := clBlue;
      eDateW.Text := FormatFloat(' ###0.0',DateW/1.0e6);
      eDatePlusMinusW.Text := FormatFloat(' ###0.0',T_Mult*Sqrt(VarDateW)/1.0e6);
      eMSWDw.Text := FormatFloat('###0.000',MSWDw);
      edfW.Text := IntToStr(Idf+1);
      if (Idf+1 <= 0) then
      begin
         tMsum:=1;
      end else
      begin
         if (Idf+1 >= NumStatisticsValues) then
         begin
           tMsum:=MSUM_Val[NumStatisticsValues];
         end else
         begin
           tMsum:=MSUM_Val[Idf+1];
         end;
      end;
      if (tMsum = 0.0) then tMsum := 1.00;
      eFw.Text := FormatFloat(' ##0.000',tMsum);
      //eFw.Text := FormatFloat(' ##0.000',MSUM_Val[Idf+1]);
      eProbW.Text := '';
      ProbabilityOfFitW := ProbabilityOfF(Idf+1,N_Rep,MSWDw,1);
      eProbW.Text := FormatFloat(' 0.000',ProbabilityOfFitW);
      if (ProbabilityOfFitW < FAlpha) then eProbW.Font.Color := clRed
                                    else eProbW.Font.Color := clBlue;
    end;
    if (temporaryAnalType = 'A') then
    begin
      tx := 1.0/Xtra3[0];
      ty := Xtra[0]/(U238U235*Xtra3[0]);
      Ex := t1;
      Ey := t2;
      tEx := Ey;
      tEy := Sqrt(Ex*Ex+Ey*Ey-2.0*RR[0]*Ex*Ey);
      tr := (Ey-tEx*RR[0])/tEy;
      Xtra[0] := tx;
      Xtra3[0] := ty;
      Xtra1[0] := tEx*Xtra[0];
      Xtra2[0] := t4;
      RR[0] := tr;
      ConvertConcordia2TeraWasserburg;
      Choose_Wt_Field;
    end;
    ChartConcordia.Series[iEllipsesExcluded].Clear;
    ChartConcordia.Series[iEllipsesIncluded].Clear;
    ChartConcordia.Series[iEnvelopeLower].Clear;
    ChartConcordia.Series[iEnvelopeUpper].Clear;
    ChartConcordia.Series[iCurveTic].Clear;
    ChartConcordia.Series[iCurveLine].Clear;
    ChartConcordia.Series[iRegressionLine].Clear;
    ChartConcordia.Series[iDataExcluded].Clear;
    ChartConcordia.Series[iDataIncluded].Clear;
    ChartConcordia.Series[iErrorExcluded].Clear;
    ChartConcordia.Series[iErrorIncluded].Clear;
    ChartConcordia.Series[iEllipseConcordia].Clear;
    ChartConcordia.Series[iDataConcordia].Clear;
    ChartConcordia.Series[iCurrent].Clear;
    ChartConcordia.Series[iCurveLinePlus].Clear;
    ChartConcordia.Series[iCurveLineMinus].Clear;
    ChartConcordia.Series[iRegressionLine].XValues.Order := loNone;
    ChartConcordia.Series[iRegressionLine].YValues.Order := loNone;
    ChartConcordia.Series[iDataIncluded].XValues.Order := loNone;
    ChartConcordia.Series[iDataIncluded].YValues.Order := loNone;
    ChartConcordia.Series[iDataExcluded].XValues.Order := loNone;
    ChartConcordia.Series[iDataExcluded].YValues.Order := loNone;
    ChartConcordia.Series[iCurveLine].XValues.Order := loNone;
    ChartConcordia.Series[iCurveLine].YValues.Order := loNone;
    ChartConcordia.Series[iCurveTic].XValues.Order := loNone;
    ChartConcordia.Series[iCurveTic].YValues.Order := loNone;
    ChartConcordia.Series[iEnvelopeUpper].XValues.Order := loNone;
    ChartConcordia.Series[iEnvelopeUpper].YValues.Order := loNone;
    ChartConcordia.Series[iEnvelopeLower].XValues.Order := loNone;;
    ChartConcordia.Series[iEnvelopeLower].YValues.Order := loNone;;
    ChartConcordia.Series[iEllipsesIncluded].XValues.Order := loNone;
    ChartConcordia.Series[iEllipsesIncluded].YValues.Order := loNone;
    ChartConcordia.Series[iEllipsesExcluded].XValues.Order := loNone;
    ChartConcordia.Series[iEllipsesExcluded].YValues.Order := loNone;
    ChartConcordia.Series[iCurveLinePlus].XValues.Order := loNone;
    ChartConcordia.Series[iCurveLinePlus].YValues.Order := loNone;
    ChartConcordia.Series[iCurveLineMinus].XValues.Order := loNone;
    ChartConcordia.Series[iCurveLineMinus].YValues.Order := loNone;
    XMax := -1e9;
    XMin :=  9e9;
    YMax := -1e9;
    YMin :=  9e9;
    MaxAge := 0.0;
    tmpX := Xtra[0];
    tmpY := Xtra3[0];
    ChartConcordia.Series[iDataConcordia].AddXY(tmpX,tmpY);
    tmpX := Xtra[1];
    tmpY := Xtra3[1];
    //ChartConcordia.Series[iPointIncluded].AddXY(tmpX,tmpY);
    for i := 1 to NumberOfPoints do
    begin
      if (i = 1) then
      begin
        ChartConcordia.Series[iCurrent].Clear;
        ChartConcordia.Series[iCurrent].AddXY(Xtra[i],Xtra3[i]);
      end;
      if (PFlg[i] = 'Y') then
      begin
        if (RFlg[i] = 'Y') then
        begin
          tmpX := Xtra[i];
          tmpY := Xtra3[i];
          ChartConcordia.Series[iDataIncluded].AddXY(tmpX,tmpY);
        end else
        begin
          tmpX := Xtra[i];
          tmpY := Xtra3[i];
          ChartConcordia.Series[iDataExcluded].AddXY(tmpX,tmpY);
        end;
      end;
      if (PFlg[i] = 'Y') then
      begin
        SprdSheetEllipse(i);
      end;
      if (PFlg[i] = 'Y') then
      begin
        if (RFlg[i] = 'Y') then
        begin
          //VtChWtAv.Plot.SeriesCollection.Item[5].Pen.VtColor.Red := GraphColour[3,1];
          //VtChWtAv.Plot.SeriesCollection.Item[5].Pen.VtColor.Blue := GraphColour[3,2];
          //VtChWtAv.Plot.SeriesCollection.Item[5].Pen.VtColor.Green := GraphColour[3,3];
        end else
        begin
          //VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Red := GraphColour[4,1];
          //VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Blue := GraphColour[4,2];
          //VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Green := GraphColour[4,3];
        end;
        DrawEllipse(i);
      end;
    end;
    {
    //attempt to colour individual ellipses based on suggestions from Steema staff
    ChartConcordia.Canvas.Brush.Gradient.StartColor := clRed;  //can use (a)rgb colours
    ChartConcordia.Canvas.Brush.Gradient.EndColor := clYellow;
    ChartConcordia.Canvas.Brush.Gradient.Visible := True;
    SetLength(MyArray, ChartConcordia.Series[iEllipsesIncluded].Count);
    for j:= 0 to ChartConcordia.Series[iEllipsesIncluded].Count-1 do
    begin
      if ((ChartConcordia.Series[iEllipsesIncluded].IsNull(j)) or (ChartConcordia.Series[iEllipsesIncluded].XValue[j] = 0.0)) then
      begin
        // do nothing
      end else
      begin
        MyArray[j] := TPoint.Create(ChartConcordia.Series[iEllipsesIncluded].CalcXPos(j),ChartConcordia.Series[iEllipsesIncluded].CalcYPos(j));
      end;
    end;
    ChartConcordia.Canvas.Polygon(MyArray);
    }


    SprdSheetEllipse(0);
    DrawEllipse(0);
    if (XMax <= XMin) then XMax := XMin + 0.1*XMin;
    if (YMax <= YMin) then YMax := YMin + 0.1*YMin;
    XMax := XMax + 0.1*(XMax-XMin);
    XMin := XMin - 0.1*(XMax-XMin);
    YMax := YMax + 0.1*(YMax-YMin);
    YMin := YMin - 0.1*(YMax-YMin);

    TMult := TMultiplier(N_Rep);
    //Wether  TMult := TMultiplier(N_Rep);
    MaxAge := 4500.0;
    if CharInSet(AnalType, ['8']) then
    begin
      TMult := TMultiplier(N_Rep);
      //tmpSingle := exp(DecayConst[ord(at235UPb)]*MaxAge*1.0e6)-1.0;
      //tmpSingle := exp(DecayConst[ord(at238UPb)]*MaxAge*1.0e6)-1.0;
      tmpAge := 0.0;
      tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
      tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
      ChartConcordia.Series[iCurveLine].AddXY(tmpX,tmpY);
      //Plus concordia uncertainty curve above normal curve
      tmpXp := exp((DecayConst[ord(at235UPb)]-(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      tmpYp := exp((DecayConst[ord(at238UPb)]+(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      ChartConcordia.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
      //Minus concordia uncertainty curve above normal curve
      tmpXm := exp((DecayConst[ord(at235UPb)]+(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      tmpYm := exp((DecayConst[ord(at238UPb)]-(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      ChartConcordia.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
      j := 1;
      repeat
        //normal concordia curve
        tmpX := Exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
        tmpY := Exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
        ChartConcordia.Series[iCurveLine].AddXY(tmpX,tmpY);
        //Plus concordia uncertainty curve above normal curve
        tmpXp := exp((DecayConst[ord(at235UPb)]-(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
        tmpYp := exp((DecayConst[ord(at238UPb)]+(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
        ChartConcordia.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
        //Minus concordia uncertainty curve above normal curve
        tmpXm := exp((DecayConst[ord(at235UPb)]+(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
        tmpYm := exp((DecayConst[ord(at238UPb)]-(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
        ChartConcordia.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
        tmpAge := tmpAge + TicksEvery/5.0;
        j := j+1;
      until (tmpAge > (MaxAgeConcordia+50.0));
    end;

    //Wetherill Concordia curve tics
    if CharInSet(AnalType, ['8']) then
    begin
      ChartConcordia.Series[iCurveTic].Clear;
      if (MaxAge <= 1.0) then MaxAge := 4500.0;
      tmpAge := 0.0;
      tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
      tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
      //ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
      j := 1;
      repeat
        tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
        tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
        ChartConcordia.Series[iCurveTic].AddXY(tmpX,tmpY,FormatFloat(eTicFormat.Text,tmpAge));
        tmpAge := tmpAge + TicksEvery;
        j := j+1;
      until (tmpAge > (MaxAgeConcordia+50.0));
    end;

    //Tera-Wasserburg curves
    if ((temporaryAnalType = 'A') or (CharInSet(AnalType, ['A']))) then
    begin
      MaxAge := 4500.0;
      tmpAge := 0.0000001;
      //ShowMessage('TMult '+FormatFloat('##0.000',TMult));
      j := 1;
      repeat
        //standard concordia curve
        tmpSingle := 1.0/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
        tmpX := tmpSingle;
        tmpSingle := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0);
        tmpSingle := tmpSingle/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
        tmpY := tmpSingle;
        ChartConcordia.Series[iCurveLine].AddXY(tmpX,tmpY);
        //Minus uncertainty curve to right of normal one
        tmpSingle := 1.0/(exp((DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
        tmpXm := tmpSingle;
        tmpSingle := (1.0/U238U235)*(exp((DecayConst[ord(at235UPb)]-DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
        tmpSingle := tmpSingle/(exp((DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
        tmpYm := tmpSingle;
        ChartConcordia.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
        //Plus uncertainty curve to right of normal one
        tmpSingle := 1.0/(exp((DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
        tmpXp := tmpSingle;
        tmpSingle := (1.0/U238U235)*(exp((DecayConst[ord(at235UPb)]+DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
        tmpSingle := tmpSingle/(exp((DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
        tmpYp := tmpSingle;
        ChartConcordia.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
        tmpAge := tmpAge + TicksEvery/5.0;
        j := j+1;
      until (tmpAge > (MaxAge+0.1*MaxAge));
    end;

    //Tera-Wasserburg curve tics
    if CharInSet(AnalType, ['A']) then
    begin
      ChartConcordia.Series[iCurveTic].Clear;
      if (LwrIntercept < 1.0) then
        tmpX := 12.0
      else
        tmpY := 1.0/(exp(DecayConst[ord(at238UPb)]*LwrIntercept*1.0e6)-1.0);
      if (UprIntercept <=1.0) then
      begin
        tmpX := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*4500.0*1.0e6)-1.0);
        tmpY := tmpX/(exp(DecayConst[ord(at238UPb)]*4500.00*1.0e6)-1.0);
        MaxAge := 4500.0;
      end else
      begin
        tmpX := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*UprIntercept*1.0e6)-1.0);
        tmpY := tmpX/(exp(DecayConst[ord(at238UPb)]*UprIntercept*1.0e6)-1.0);
        MaxAge := UprIntercept;
      end;
      tmpAge := 0.00000001;
      j := 1;
      repeat
        tmpX := 1.0/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
        tmpY := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0);
        tmpY := tmpY/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
        ChartConcordia.Series[iCurveTic].AddXY(tmpX,tmpY,FormatFloat(eTicFormat.Text,tmpAge));
        tmpAge := tmpAge + TicksEvery;
        j := j+1;
      until (tmpAge > (MaxAge+0.1*MaxAge));
    end;
    ChartConcordia.Visible := true;
    ChartConcordia.BottomAxis.SetMinMax(XMin,XMax);
    ChartConcordia.LeftAxis.SetMinMax(YMin,YMax);
    Range := YMax-YMin;
    ChartConcordia.Axes.Left.AxisValuesFormat := '####0.0##';
    if (Range <= 1.0) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.00';
    if (Range <= 0.10) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.000';
    if (Range <= 0.010) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.0000';
    if (Range <= 0.001) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.00000';
    if (Range <= 0.0001) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.000000';
    Range := XMax-XMin;
    ChartConcordia.Axes.Bottom.AxisValuesFormat := '####0.###';
    if (Range <= 1.0) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.00';
    if (Range <= 0.2) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.000';
    if (Range <= 0.02) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.0000';
    if (Range <= 0.002) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.00000';
    if (Range <= 0.0002) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.000000';
  end;
end;

procedure TfmConcordiaDate.ChartConcordiaClick(Sender: TObject);
var
  i, itmp : integer;
  x, y : double;
  tx, ty : double;
  tmpStr : string;
  Node : TTreeNode;
begin
  //Series1.GetCursorValues(x,y);
  for i := 0 to ChartConcordia.SeriesCount-1 do
  begin
    if (i in [7,8,9]) then
    begin
      itmp := ChartConcordia.Series[i].GetCursorValueIndex;
      if (itmp <> -1) then
      begin
        //ShowMessage('Clicked series: '+ChartConcordia.Series[i].Name+' at point: '+IntToStr(itmp));
        dmGDWtmp.cdsReg.Locate('i',itmp+1,[]);
        tmpStr := dmGDWtmp.cdsRegSample_no.AsString;
        tx := dmGdwtmp.cdsRegXRatio.AsFloat;
        ty := dmGdwtmp.cdsRegYRatio.AsFloat;
        ChartConcordia.Series[iCurrent].Clear;
        ChartConcordia.Series[iCurrent].AddXY(tx,ty);
        ChartConcordia.Enabled := true;
        Node := TreeView1.Items[itmp];
        TreeView1.Select(Node);
        TreeView1.HideSelection := false;
        TreeView1.SetFocus;
        Node.MakeVisible;
        Node.SelectedIndex := itmp+1;
      end;
    end;
  end;
end;

procedure TfmConcordiaDate.ChartConcordiaDblClick(Sender: TObject);
begin
  ChartConcordiaClick(Sender);
  dmGDWtmp.cdsReg.Edit;
  if (dmGDWtmp.cdsRegRFlag.AsString = 'Y') then dmGDWtmp.cdsRegRFlag.AsString := 'N'
  else dmGDWtmp.cdsRegRFlag.AsString := 'Y';
  dmGDWtmp.cdsReg.Post;
end;

procedure TfmConcordiaDate.ChartConcordiaZoom(Sender: TObject);
var
  XMax, XMin,
  YMax, YMin,
  Range : double;
begin
  XMin := ChartConcordia.BottomAxis.Minimum;
  XMax := ChartConcordia.BottomAxis.Maximum;
  YMin := ChartConcordia.LeftAxis.Minimum;
  YMax := ChartConcordia.LeftAxis.Maximum;
  Range := YMax-YMin;
  ChartConcordia.Axes.Left.AxisValuesFormat := '####0.0##';
  if (Range <= 1.0) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.00';
  if (Range <= 0.10) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.000';
  if (Range <= 0.010) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.0000';
  if (Range <= 0.001) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.00000';
  if (Range <= 0.0001) then ChartConcordia.Axes.Left.AxisValuesFormat := '###0.000000';
  Range := XMax-XMin;
  ChartConcordia.Axes.Bottom.AxisValuesFormat := '####0.###';
  if (Range <= 1.0) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.00';
  if (Range <= 0.2) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.000';
  if (Range <= 0.02) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.0000';
  if (Range <= 0.002) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.00000';
  if (Range <= 0.0002) then ChartConcordia.Axes.Bottom.AxisValuesFormat := '###0.000000';
end;

procedure TfmConcordiaDate.Choose_Wt_Field;
var
  LWt          : array[1..2] of double;
  i            : integer;
begin
  HistOK:=false;
  for i:=1 to NumberOfPoints do
  begin
      case VarbNoX of
        9 : begin {T(concordia)}
          HistOK:=true;
          if (iAnalTyp in [8,10]) then
          begin
            HistOK:=true;
            AnalType8 := 'N';
            if ((Ratio[i,1] > 0.000001)) then
            begin
              Xtra[i] := Ratio[i,1];
            end else
            begin
                Xtra1[i] := 0.0;
                Xtra2[i] := 0.0;
                Xtra3[i] := 0.0;
                RFlg[i] := 'N';
            end;
            if ((Ratio[i,2] > 0.000001)) then
            begin
              Xtra3[i] := Ratio[i,2];
              case ErrTyp[i] of
                '1' : begin
                   LWt[1]:=ErrorWt[i,1]*Ratio[i,1]/100.0;
                   LWt[2]:=ErrorWt[i,2]*Ratio[i,2]/100.0;
                 end;
                '2' : begin
                   LWt[1]:=ErrorWt[i,1]*Ratio[i,1]/100.0;
                   LWt[2]:=ErrorWt[i,2];
                 end;
                '3' : begin
                   LWt[1]:=ErrorWt[i,1];
                   LWt[2]:=ErrorWt[i,2]*Ratio[i,1]/100.0;
                 end;
                '4' : begin
                   LWt[1]:=ErrorWt[i,1];
                   LWt[2]:=ErrorWt[i,2];
                 end;
              end;
              Xtra1[i] := LWt[1];
              Xtra2[i] := LWt[2];
              if (Ratio[i,3] > 0.000001) then
              begin
                //Xtra[i]:=Ratio[i,3];
                //ShowMessage('Ratio 3 '+FormatFloat('##0.000000',Xtra[i]));
              end else
              begin
                if ((Ratio[i,2] > 0.000001)) then
                begin
                  Ratio[i,3] := Ratio[i,1]/Ratio[i,2]/U238U235;
                  //ShowMessage('Ratio 1/2 '+FormatFloat('##0.000000',Xtra[i]));
                end else
                begin
                  //Xtra[i]:=0.0;
                  RFlg[i]:='N';
                end;
              end;
              if (ZPrec[i] > 0.00001) then
              begin
                if ((Ratio[i,3]/100.0*BlanketZErrVal) <= ZPrec[i]) then
                  //Xtra1[i]:=ZPrec[i]
                else begin
                  ZPrec[i]:=Ratio[i,3]/100.0*BlanketZErrVal;
                end;
              end
              else begin
                 if ((Ratio[i,2] > 0.000001)) then
                 begin
                     case ErrTyp[i] of
                       '1' : begin
                               LWt[1]:=ErrorWt[i,1];
                               LWt[2]:=ErrorWt[i,2];
                             end;
                       '2' : begin
                               LWt[1]:=ErrorWt[i,1];
                               LWt[2]:=ErrorWt[i,2]/100.0*Ratio[i,2];
                             end;
                       '3' : begin
                               LWt[1]:=ErrorWt[i,1]/100.0*Ratio[i,1];
                               LWt[2]:=ErrorWt[i,2];
                             end;
                       '4' : begin
                               LWt[1]:=ErrorWt[i,1]/100.0*Ratio[i,1];
                               LWt[2]:=ErrorWt[i,2]/100.0*Ratio[i,2];
                             end;
                     end;
                     Zprec[i] := Sqrt(LWt[1]*LWt[1]+LWt[2]*LWt[2]-2.0*R[i]*LWt[1]*LWt[2]);
                     //ShowMessage('Xtra1 1/2 a '+FormatFloat('##0.000000',Xtra1[i]));
                     //Xtra1[i] := Xtra1[i]/Xtra[i]*100.0;
                     //ShowMessage('Xtra1 1/2 % '+FormatFloat('##0.000000',Xtra1[i]));
                 end else
                 begin
                     //Xtra1[i]:=0.0;
                     RFlg[i]:='N';
                 end;
                 Xtra2[i] := Xtra1[i];
              end;
            end else
            begin
                Xtra1[i] := 0.0;
                Xtra2[i] := 0.0;
                Xtra3[i] := 0.0;
                RFlg[i] := 'N';
            end;
          end;
        end;
      end;
  end;
end;


procedure TfmConcordiaDate.bbSprdSheetClick(Sender: TObject);
var
  i, iRow, iCol    : integer;
  StepSize, t1, t2, temp : double;
  tmpStr   : string;
  StepIncrement : integer;
  MinX,MaxX,MinY,MaxY : double;
  zero : double;
  Xtra4 : array [1..MaxSamp,1..2] of double;
  Xtra5 : array [1..MaxSamp,1..2] of double;
begin
  zero := 0.0;
  //for i := 1 to NumberOfPoints do
  for i := 1 to 10 do
  begin
    ConcordiaAgeSingle (i,Xtra4[i,1],Xtra4[i,2],
                          Xtra5[i,1],Xtra5[i,2]);
    //ShowMessage('i = '+Int2Str(i)+'  Xtra4 X = '+FormatFloat('####0.00000',Xtra4[i,1])+'  Xtra4 Y = '+FormatFloat('####0.00000',Xtra4[i,2]));
  end;
  try
      SaveDialogModels.InitialDir := TTPath;
      SaveDialogModels.FileName := ProjectName+'_ConcordiaAge';
      if SaveDialogModels.Execute then
      begin
        Drive3 := ExtractFileDir(SaveDialogModels.FileName);
        TTPath := ExtractFilePath(SaveDialogModels.FileName);
        MinX := ChartConcordia.BottomAxis.Minimum;
        MaxX := ChartConcordia.BottomAxis.Maximum;
        MinY := ChartConcordia.LeftAxis.Minimum;
        MaxY := ChartConcordia.LeftAxis.Maximum;
        try
          SprdSheet := TXlsFile.Create(true);
          SprdSheet.NewFile(1);
          iRow := 1;
          iCol := 1;
          tmpStr := 'Sample';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 2;
          tmpStr := Element[iAnalTyp,1];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 3;
          tmpStr := Element[iAnalTyp,2];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 4;
          tmpStr := XRatioStr[iAnalTyp];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 5;
          tmpStr := 'Precision';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 6;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 8;
          tmpStr := YRatioStr[iAnalTyp];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 9;
          tmpStr := 'Precision';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 10;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 12;
          tmpStr := 'R';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 13;
          tmpStr := ZRatioStr[iAnalTyp];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 14;
          tmpStr := 'Precision or Sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 15;
          tmpStr := 'RFlg';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 16;
          tmpStr := 'PFlg';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 17;
          tmpStr := 'X (included)';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 18;
          tmpStr := 'Y (included)';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 19;
          tmpStr := 'X (excluded)';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 20;
          tmpStr := 'Y (excluded)';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 21;
          tmpStr := 'Concordia Age (incl.)';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 22;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 23;
          tmpStr := 'Concordia Age (excl.)';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 24;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 25;
          tmpStr := 'X calc';
          //SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 26;
          tmpStr := 'X +95%';
          //SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 27;
          tmpStr := 'X -95%';
          //SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 28;
          tmpStr := 'Y calc';
          //SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 29;
          tmpStr := 'Y +95%';
          //SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 30;
          tmpStr := 'Y -95%';
          //SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          for i := 1 to NumberOfPoints do
          begin
            iRow := i+1;
            iCol := 1;
            tmpStr := SmpNo[i];
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            iCol := 16;
            tmpStr := PFlg[i];
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            if (PFlg[i] = 'Y') then
            begin
              iCol := 2;
              //tmpStr := FloatToStr(Conc[i,1]);
              SprdSheet.SetCellValue(iRow,iCol,Conc[i,1]);
              iCol := 3;
              //tmpStr := FloatToStr(Conc[i,2]);
              SprdSheet.SetCellValue(iRow,iCol,Conc[i,2]);
              iCol := 4;
              //tmpStr := FloatToStr(Ratio[i,1]);
              SprdSheet.SetCellValue(iRow,iCol,Ratio[i,1]);
              iCol := 5;
              //tmpStr := FloatToStr(XPrec[i]);
              SprdSheet.SetCellValue(iRow,iCol,XPrec[i]);
              iCol := 6;
              //tmpStr := FloatToStr(ErrorWt[i,1]);
              SprdSheet.SetCellValue(iRow,iCol,ErrorWt[i,1]);
              iCol := 7;
              if (ErrTyp[i] in ['1','2']) then
                tmpStr := '%'
              else
                tmpStr := 'a';
              SprdSheet.SetCellValue(iRow,iCol,tmpStr);
              iCol := 8;
              //tmpStr := FloatToStr(Ratio[i,2]);
              SprdSheet.SetCellValue(iRow,iCol,Ratio[i,2]);
              iCol := 9;
              //tmpStr := FloatToStr(YPrec[i]);
              SprdSheet.SetCellValue(iRow,iCol,YPrec[i]);
              iCol := 10;
              //tmpStr := FloatToStr(ErrorWt[i,2]);
              SprdSheet.SetCellValue(iRow,iCol,ErrorWt[i,2]);
              iCol := 11;
              if CharInSet(ErrTyp[i], ['1','3']) then
                tmpStr := '%'
              else
                tmpStr := 'a';
              SprdSheet.SetCellValue(iRow,iCol,tmpStr);
              iCol := 12;
              //tmpStr := FloatToStr(R[i]);
              SprdSheet.SetCellValue(iRow,iCol,R[i]);
              iCol := 13;
              //tmpStr := FloatToStr(Ratio[i,3]);
              SprdSheet.SetCellValue(iRow,iCol,Ratio[i,3]);
              iCol := 14;
              //tmpStr := FloatToStr(ZPrec[i]);
              SprdSheet.SetCellValue(iRow,iCol,ZPrec[i]);
              iCol := 15;
              tmpStr := RFlg[i];
              SprdSheet.SetCellValue(iRow,iCol,tmpStr);
              iCol := 16;
              tmpStr := PFlg[i];
              SprdSheet.SetCellValue(iRow,iCol,tmpStr);
              iCol := 17;
              //tmpStr := FloatToStr(Residual[i,1]);
              //SprdSheet.SetCellValue(iRow,iCol,Residual[i,1]);
              iCol := 18;
              //tmpStr := FloatToStr(Residual[i,2]);
              //SprdSheet.SetCellValue(iRow,iCol,Residual[i,2]);
              iCol := 25;
              //tmpStr := FloatToStr(1.0*i);
              SprdSheet.SetCellValue(iRow,iCol,1.0*i);
              iCol := 26;
              //tmpStr := FloatToStr(0.0);
              //SprdSheet.SetCellValue(iRow,iCol,zero);
              iCol := 27;
              //tmpStr := FloatToStr(0.0);
              //SprdSheet.SetCellValue(iRow,iCol,zero);
              iCol := 28;
              //tmpStr := FloatToStr(Xtra[i]);
              //SprdSheet.SetCellValue(iRow,iCol,Xtra[i]);
              iCol := 29;
              //tmpStr := FloatToStr(Xtra1[i]);
              //SprdSheet.SetCellValue(iRow,iCol,Xtra1[i]);
              iCol := 30;
              //tmpStr := FloatToStr(Xtra2[i]);
              //SprdSheet.SetCellValue(iRow,iCol,Xtra2[i]);
            end;
          end;
          //Data
          iRow := 1;
          iCol := 1;
          for i := 1 to NumberOfPoints do
          begin
            iCol := 25;
            iRow := i+1;
            //temp := ChartConcordia.Series[iErrorIncluded].XValue[i];
            temp := 1.0*i;
            //tmpStr := FloatToStr(temp);
            //SprdSheet.SetCellValue(iRow,iCol,temp);
            if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
            begin
              iCol := 17;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra[i];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
              iCol := 18;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra3[i];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
            end;
            if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
            begin
              iCol := 19;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra[i];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
              iCol := 20;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra3[i];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
            end;
            if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
            begin
              iCol := 21;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra5[i,1];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
              iCol := 22;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra5[i,2];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
            end;
            if ((RFlg[i] = 'N') and (PFlg[i] = 'Y')) then
            begin
              iCol := 23;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra5[i,1];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
              iCol := 24;
              //temp := ChartConcordia.Series[iErrorIncluded].YValue[i];
              temp := Xtra5[i,2];
              //tmpStr := FloatToStr(temp);
              SprdSheet.SetCellValue(iRow,iCol,temp);
            end;
            j := j+1;
          end;
          SprdSheet.Save(SaveDialogModels.FileName);
        finally
          FreeAndNil(SprdSheet);
        end;
      end;
  finally
  end;
end;



procedure TfmConcordiaDate.bbStoreMdlClick(Sender: TObject);
var
  iCode : integer;
begin
  {
  if ((VarbNox = 9) and (AnalType in ['8','A'])) then
  begin
    UpDateRFlg(Sender);
    AnalType8 := 'N';
    Model := 1;
    try
      ResultsForm := TfmResults.Create(Self);
      ResultsForm.GDWSender := 'Ccda';
      try
        Val(eMSWDEquivalence.Text,Msum,iCode);
      except
        Msum := 0.0;
      end;
      try
        Val(eIcnt.Text,NumberOfPointsRegressed,iCode);
      except
        NumberOfPointsRegressed := 0;
      end;
      if (AnalType in ['8','A']) then
      begin
        try
          Val(eDateWO.Text,UprIntercept,iCode);
        except
          UprIntercept := 0.0;
        end;
        try
          Val(eDatePlusMinusWO.Text,UprUprAgeError,iCode);
        except
          UprUprAgeError := 0.0;
        end;
        try
          Val(eDatePlusMinusWO.Text,UprLwrAgeError,iCode);
        except
          UprLwrAgeError := 0.0;
        end;
      end;
      ResultsForm.ShowModal;
    finally
      ResultsForm.Free;
    end;
  end;
  }
end;

procedure TfmConcordiaDate.bPrintClick(Sender: TObject);
begin
 //
end;

procedure TfmConcordiaDate.cbCurrentSampleClick(Sender: TObject);
begin
  if (cbCurrentSample.Checked) then
  begin
    ChartConcordia.Series[iCurrent].Visible := true;
  end
  else begin
    ChartConcordia.Series[iCurrent].Visible := false;
  end;
end;

procedure TfmConcordiaDate.cbLegendClick(Sender: TObject);
begin
  if (cbLegend.Checked) then
  begin
    ChartConcordia.Legend.Visible := true;
  end
  else begin
    ChartConcordia.Legend.Visible := false;
  end;
end;

procedure TfmConcordiaDate.cbTicLabelsClick(Sender: TObject);
begin
  if (cbTicLabels.Checked) then
  begin
    ChartConcordia.Series[iCurveTic].Marks.Visible := true;
  end
  else begin
    ChartConcordia.Series[iCurveTic].Marks.Visible := false;
  end;
end;

procedure TfmConcordiaDate.SprdSheetEllipse ( i : integer);
const
  St           : double = 0.2;
var
  T_Mult_ell            : double;
  Angle, C1, C2,
  A, B, Vx, Vy,
  SinAngle, CosAngle,
  K, X, Y, Z,
  XE, YE, R,
  XP, YP, XPP, YPP      : double;
  J			: integer;
  LWt                   : array[1..2] of double;
  tmpiStr, tmpStr   : string;

begin
  {
    if (EllipseMagnif > 1.0)
      then T_Mult_ell:=TMultiplier(1.0*N_Rep)
      else T_Mult_ell := 1.0;
    LWt[1] := Xtra1[i];
    LWt[2] := Xtra2[i];
    XE:=LWt[1]*T_Mult_ell;
    YE:=LWt[2]*T_Mult_ell;
  if (Abs(RR[i])<1.0) then R:=RR[i]
		  else R:=0.0;
  if (XE=YE) then XE:=XE*1.00001;
  Angle:=0.5*ArcTan((2.0*R*XE*YE)/(XE*XE-YE*YE));
  C1:=2.0*(1-R*R)*1.224*1.224;
  C2:=1.0/(Cos(2.0*Angle));
  Vx:=XE*XE;
  Vy:=YE*YE;
  if (Vx = 0.0) then Vx := 0.001;
  if (Vy = 0.0) then Vy := 0.001;
  A:=(C1/((1.0+C2)/Vx + (1.0-C2)/Vy));
  B:=(C1/((1.0-C2)/Vx + (1.0+C2)/Vy));
  if (A > 0.0) then A:=Sqrt(A)
               else A := 1.0;  //length of major axis
  if (B > 0.0) then B:=Sqrt(B)
               else B := 1.0;  //length of minor axis
  SinAngle:=Sin(Angle);
  CosAngle:=Cos(Angle);
  K:=1.0;
  J:=0;
  X:=K*A;
  St := X/15.0;
  if (St <= 0.0) then St := 1.0;
  repeat
    begin
      J:=J+1;
      Z:=1.0-(X/A)*(X/A);
      if (Z>=0.0) then Y:=K*B*Sqrt(Z)
                  else Y:=0.0;
      XP:=Xtra[i]+X*CosAngle-Y*SinAngle;
      YP:=Xtra3[i]+X*SinAngle+Y*CosAngle;
      if (J=1) then
      begin
        MdlSheet.Row := RowNumber;
        if (i = 0) then
        begin
          if (RFlg[i] = 'Y') then MdlSheet.Col := 11
        end;
        if (i > 0) then
        begin
          MdlSheet.Col := 8;
          MdlSheet.Text := SmpNo[i];
          if (RFlg[i] = 'Y') then MdlSheet.Col := 6
                             else MdlSheet.Col := 9;
        end;
        MdlSheet.Number := XP;
        if (i = 0) then
        begin
          if (RFlg[i] = 'Y') then MdlSheet.Col := 12;
        end;
        if (i > 0) then
        begin
          if (RFlg[i] = 'Y') then MdlSheet.Col := 7
                             else MdlSheet.Col := 10;
        end;
        MdlSheet.Number := YP;
        RowNumber := RowNumber + 1;
      end;
      if (J=1) then
      begin
	XPP:=XP;
	YPP:=YP;
      end;
        MdlSheet.Row := RowNumber;
        if (i = 0) then
        begin
          if (RFlg[i] = 'Y') then MdlSheet.Col := 11;
        end;
        if (i > 0) then
        begin
          MdlSheet.Col := 8;
          MdlSheet.Text := SmpNo[i];
          if (RFlg[i] = 'Y') then MdlSheet.Col := 6
                             else MdlSheet.Col := 9;
        end;
        MdlSheet.Number := XP;
        if (i = 0) then
        begin
          if (RFlg[i] = 'Y') then MdlSheet.Col := 12;
        end;
        if (i > 0) then
        begin
          if (RFlg[i] = 'Y') then MdlSheet.Col := 7
                             else MdlSheet.Col := 10;
        end;
        MdlSheet.Number := YP;
        RowNumber := RowNumber + 1;
    end;
    if (J < 5) then X:=X-K*St
               else X:=X-K*St*2.0;
  until (X < (-(K*A)));
  K:=-1.0;
  J:=0;
  X:=K*A;
  repeat
    begin
      J:=J+1;
      Z:=1.0-(X/A)*(X/A);
      if (Z>=0.0) then Y:=K*B*Sqrt(Z)
                  else Y:=0.0;
      XP:=Xtra[i]+X*CosAngle-Y*SinAngle;
      YP:=Xtra3[i]+X*SinAngle+Y*CosAngle;
      MdlSheet.Row := RowNumber;
      if (i = 0) then
      begin
        if (RFlg[i] = 'Y') then MdlSheet.Col := 11;
      end;
      if (i > 0) then
      begin
        MdlSheet.Col := 8;
        MdlSheet.Text := SmpNo[i];
        if (RFlg[i] = 'Y') then MdlSheet.Col := 6
                           else MdlSheet.Col := 9;
      end;
      MdlSheet.Number := XP;
      if (i = 0) then
      begin
        if (RFlg[i] = 'Y') then MdlSheet.Col := 12;
      end;
      if (i > 0) then
      begin
        if (RFlg[i] = 'Y') then MdlSheet.Col := 7
                           else MdlSheet.Col := 10;
      end;
      MdlSheet.Number := YP;
      RowNumber := RowNumber + 1;
    end;
    if (J < 5) then X:=X-K*St
               else X:=X-K*St*2.0;
  until (X > (-(K*A)));

  J:=J+1;
  X := -K*A;
  Z:=1.0-(X/A)*(X/A);
  if (Z>=0.0) then Y:=K*B*Sqrt(Z)
              else Y:=0.0;
  XP:=Xtra[i]+X*CosAngle-Y*SinAngle;
  YP:=Xtra3[i]+X*SinAngle+Y*CosAngle;
  MdlSheet.Row := RowNumber;
  if (i = 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 11;
  end;
  if (i > 0) then
  begin
    MdlSheet.Col := 8;
    MdlSheet.Text := SmpNo[i];
    if (RFlg[i] = 'Y') then MdlSheet.Col := 6
                       else MdlSheet.Col := 9;
  end;
  MdlSheet.Number := XP;
  if (i = 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 12;
  end;
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 7
                       else MdlSheet.Col := 10;
  end;
  MdlSheet.Number := YP;
  RowNumber := RowNumber + 1;

  MdlSheet.Row := RowNumber;
  if (i = 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 11;
  end;
  if (i > 0) then
  begin
    MdlSheet.Col := 8;
    MdlSheet.Text := SmpNo[i];
    if (RFlg[i] = 'Y') then MdlSheet.Col := 6
                       else MdlSheet.Col := 9;
  end;
  MdlSheet.Number := XP;
  if (i = 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 12;
  end;
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 7
                       else MdlSheet.Col := 10;
  end;
  MdlSheet.Number := YP;
  RowNumber := RowNumber + 1;
  MdlSheet.Row := RowNumber;
  if (i = 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 11;
  end;
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 6
                       else MdlSheet.Col := 9;
  end;
  MdlSheet.Number := -100.0;
  if (i = 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 12;
  end;
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then MdlSheet.Col := 7
                       else MdlSheet.Col := 10;
  end;
  MdlSheet.Number := -100.0;
  RowNumber := RowNumber + 1;
  }
end;

procedure TfmConcordiaDate.TreeView1Click(Sender: TObject);
var
  tmpStr : string;
  i : integer;
  tx, ty : double;
begin
  i := TreeView1.Selected.AbsoluteIndex;
  tmpStr := TreeView1.Items.Item[i].Text;
  dmGdwtmp.cdsReg.Locate('Sample_No',tmpStr,[]);
  tx := dmGdwtmp.cdsRegXRatio.AsFloat;
  ty := dmGdwtmp.cdsRegYRatio.AsFloat;
  ChartConcordia.Series[iCurrent].Clear;
  ChartConcordia.Series[iCurrent].AddXY(tx,ty);
  ChartConcordia.Enabled := true;
end;

procedure TfmConcordiaDate.DrawEllipse ( i : integer);
const
  St           : double = 0.1;
var
  T_Mult_ell            : double;
  Angle, C1, C2,
  A, B, Vx, Vy,
  SinAngle, CosAngle,
  K, X, Y, Z,
  XE, YE, R,
  XP, YP, XPP, YPP      : double;
  J			: integer;
  LWt                   : array[1..2] of double;
  tmpiStr, tmpStr   : string;
  ii : integer;
  EllipseArea : double;
  //MyArray : array of TPoint;
begin
  ChooseEllipse := 'O';
  if (EllipseMagnif > 1.0)
    then T_Mult_ell:=TMultiplier(1.0*N_Rep)
    else T_Mult_ell := 1.0;
  LWt[1] := Xtra1[i];
  LWt[2] := Xtra2[i];
  XE:=LWt[1]*T_Mult_ell;
  YE:=LWt[2]*T_Mult_ell;
  if (Abs(RR[i])<1.0) then R:=RR[i]
		  else R:=0.0;
  if (XE=YE) then XE:=XE*1.00001;
  Angle:=0.5*ArcTan((2.0*R*XE*YE)/(XE*XE-YE*YE));
  C1:=2.0*(1-R*R)*1.224*1.224;
  C2:=1.0/(Cos(2.0*Angle));
  Vx:=XE*XE;
  Vy:=YE*YE;
  if (Vx = 0.0) then Vx := 0.001;
  if (Vy = 0.0) then Vy := 0.001;
  A:=(C1/((1.0+C2)/Vx + (1.0-C2)/Vy));
  B:=(C1/((1.0-C2)/Vx + (1.0+C2)/Vy));
  if (A > 0.0) then A:=Sqrt(A)
               else A := 1.0;  //length of major axis
  if (B > 0.0) then B:=Sqrt(B)
               else B := 1.0;  //length of minor axis
  EllipseArea := pi() * A * B * 1.0e6;    // ellipse area can be used to place smaller ellipses on top of larger ones. Scaled by 1 million to make number easier to read
  //ShowMessage(IntToStr(i)+' : '+FormatFloat('#####0.0000',EllipseArea));
  SinAngle:=Sin(Angle);
  CosAngle:=Cos(Angle);
  K:=1.0;
  J:=0;
  X:=K*A;
  St := X/100.0;
  if (St <= 0.0) then St := 1.0;
  repeat
    begin
      J:=J+1;
      Z:=1.0-(X/A)*(X/A);
      if (Z>=0.0) then Y:=K*B*Sqrt(Z)
                  else Y:=0.0;
      XP:=Xtra[i]+X*CosAngle-Y*SinAngle;
      YP:=Xtra3[i]+X*SinAngle+Y*CosAngle;
      if (XP > XMax) then XMax := XP;
      if (XP < XMin) then XMin := XP;
      if (YP > YMax) then YMax := YP;
      if (YP < YMin) then YMin := YP;
      if (J=1) then
      begin
        if (i = 0) then
        begin
          ChartConcordia.Series[iEllipseConcordia].AddXY(XP,YP);
        end;
        if (i > 0) then
        begin
          if (RFlg[i] = 'Y') then ChartConcordia.Series[iEllipsesIncluded].AddXY(XP,YP)
                             else ChartConcordia.Series[iEllipsesExcluded].AddXY(XP,YP);
        end;
      end;
      if (J=1) then
      begin
      	XPP:=XP;
	      YPP:=YP;
      end;
      if (i = 0) then
      begin
        ChartConcordia.Series[iEllipseConcordia].AddXY(XP,YP);
      end;
      if (i > 0) then
      begin
        if (RFlg[i] = 'Y') then ChartConcordia.Series[iEllipsesIncluded].AddXY(XP,YP)
                           else ChartConcordia.Series[iEllipsesExcluded].AddXY(XP,YP);
      end;
    if (J < 5) then X:=X-K*St
               else X:=X-K*St*2.0;
    end;
  until (X < (-(K*A)));
  K:=-1.0;
  J:=0;
  X:=K*A;
  repeat
    begin
      J:=J+1;
      Z:=1.0-(X/A)*(X/A);
      if (Z>=0.0) then Y:=K*B*Sqrt(Z)
                  else Y:=0.0;
      XP:=Xtra[i]+X*CosAngle-Y*SinAngle;
      YP:=Xtra3[i]+X*SinAngle+Y*CosAngle;
      if (XP > XMax) then XMax := XP;
      if (XP < XMin) then XMin := XP;
      if (YP > YMax) then YMax := YP;
      if (YP < YMin) then YMin := YP;
      if (i = 0) then
      begin
        ChartConcordia.Series[iEllipseConcordia].AddXY(XP,YP);
      end;
      if (i > 0) then
      begin
        if (RFlg[i] = 'Y') then ChartConcordia.Series[iEllipsesIncluded].AddXY(XP,YP)
                           else ChartConcordia.Series[iEllipsesExcluded].AddXY(XP,YP);
      end;
    end;
    if (J < 5) then X:=X-K*St
               else X:=X-K*St*2.0;
  until (X > (-(K*A)));

  J:=J+1;
  X := -K*A;
  Z:=1.0-(X/A)*(X/A);
  if (Z>=0.0) then Y:=K*B*Sqrt(Z)
              else Y:=0.0;
  XP:=Xtra[i]+X*CosAngle-Y*SinAngle;
  YP:=Xtra3[i]+X*SinAngle+Y*CosAngle;
  if (XP > XMax) then XMax := XP;
  if (XP < XMin) then XMin := XP;
  if (YP > YMax) then YMax := YP;
  if (YP < YMin) then YMin := YP;
  if (i = 0) then
  begin
    ChartConcordia.Series[iEllipseConcordia].AddXY(XP,YP);
  end;
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then ChartConcordia.Series[iEllipsesIncluded].AddXY(XP,YP)
                       else ChartConcordia.Series[iEllipsesExcluded].AddXY(XP,YP);
  end;
  {
  if (i = 0) then
  begin
    ChartConcordia.Series[iEllipseConcordia].AddXY(XP,YP);
  end;
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then ChartConcordia.Series[iEllipsesIncluded].AddXY(XP,YP)
                       else ChartConcordia.Series[iEllipsesExcluded].AddXY(XP,YP);
  end;
  }
  if (i > 0) then
  begin
    if (RFlg[i] = 'Y') then ChartConcordia.Series[iEllipsesIncluded].AddNullXY(0.0,0.0)
                       else ChartConcordia.Series[iEllipsesExcluded].AddNullXY(0.0,0.0);
  end;
  {
  You could take the point values that you use to plot the data, converting them to pixel locations using:

    Series1.CalcXPosValue(value) and Series1.CalcYPosValue(value)

  ..or loop through the indexes of the points using:

    Series1.CalcXPos(idx) and Series1.CalcYPos(idx)

  ..and adding them to an array of TPoints. You can then plot the array in the OnAfterDraw event using:

  Code: Select all

  Chart1.Canvas.Polygon(yourArray);

  Before you plot the polygon, set the Brush to fill as you require.

  eg.

  Code: Select all

  Chart1.Canvas.Brush.Gradient.StartColor := clRed;  //can use (a)rgb colours
  Chart1.Canvas.Brush.Gradient.EndColor := clYellow;
  Chart1.Canvas.Brush.Gradient.Visible := True;

  Chart1.Canvas.Polygon(yourArray);


  procedure TForm1.Chart1AfterDraw(Sender: TObject);
  var i : Integer;
      myPoints : Array of TPoint;
  begin
    Chart1.Canvas.Brush.Gradient.StartColor := clRed;  //can use (a)rgb colours
    Chart1.Canvas.Brush.Gradient.EndColor := clYellow;
    Chart1.Canvas.Brush.Gradient.Visible := True;

    SetLength(myPoints, Series1.Count);
    for i:= 0 to Series1.Count-1 do
    Begin
      myPoints[i] := TPoint.Create(Series1.CalcXPos(i),Series1.CalcYPos(i));
    End;

    Chart1.Canvas.Polygon(myPoints);
  end;

  }
  {
  ii := 1;
  repeat
    //MyArrayX[ii] := ChartConcordia.Series[iEllipsesIncluded].CalcXPos(ii);
    //MyArrayY[ii] := ChartConcordia.Series[iEllipsesIncluded].CalcYPos(ii);
    if (RFlg[i] = 'Y') then MyArray[ii].Create(ChartConcordia.Series[iEllipsesIncluded].CalcXPos(ii),ChartConcordia.Series[iEllipsesIncluded].CalcYPos(ii));
    ii := ii + 1;
  until (ii >= ChartConcordia.Series[iEllipsesIncluded].Count);
  // Series1.CalcXPos(idx) and Series1.CalcYPos(idx)
  ChartConcordia.Canvas.Brush.Gradient.StartColor := clRed;  //can use (a)rgb colours
  ChartConcordia.Canvas.Brush.Gradient.EndColor := clYellow;
  ChartConcordia.Canvas.Brush.Gradient.Visible := True;
  ChartConcordia.Canvas.Polygon(MyArray) ;
  }
  {
    ChartConcordia.Canvas.Brush.Gradient.StartColor := clRed;  //can use (a)rgb colours
    ChartConcordia.Canvas.Brush.Gradient.EndColor := clYellow;
    ChartConcordia.Canvas.Brush.Gradient.Visible := True;
    SetLength(MyArray, ChartConcordia.Series[iEllipsesIncluded].Count);
    for j:= 0 to ChartConcordia.Series[iEllipsesIncluded].Count-1 do
    begin
      if ((ChartConcordia.Series[iEllipsesIncluded].IsNull(j)) or (ChartConcordia.Series[iEllipsesIncluded].XValue[j] = 0.0)) then
      begin
        // do nothing
      end else
      begin
        MyArray[j] := TPoint.Create(ChartConcordia.Series[iEllipsesIncluded].CalcXPos(j),ChartConcordia.Series[iEllipsesIncluded].CalcYPos(j));
      end;
    end;
    ChartConcordia.Canvas.Polygon(MyArray);
    }
end;

procedure TfmConcordiaDate.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  UpDateRFlg(Sender);
  Title := eTitle.Text;
  {
  GraphColour[1,1] := VtChWtAv.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Red;
  GraphColour[1,2] := VtChWtAv.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Blue;
  GraphColour[1,3] := VtChWtAv.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Green;
  GraphColour[2,1] := VtChWtAv.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Red;
  GraphColour[2,2] := VtChWtAv.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Blue;
  GraphColour[2,3] := VtChWtAv.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Green;
  GraphColour[3,1] := VtChWtAv.Plot.SeriesCollection.Item[5].Pen.VtColor.Red;
  GraphColour[3,2] := VtChWtAv.Plot.SeriesCollection.Item[5].Pen.VtColor.Blue;
  GraphColour[3,3] := VtChWtAv.Plot.SeriesCollection.Item[5].Pen.VtColor.Green;
  GraphColour[4,1] := VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Red;
  GraphColour[4,2] := VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Blue;
  GraphColour[4,3] := VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Green;
  GraphColour[7,1] := VtChWtAv.Plot.SeriesCollection.Item[9].Pen.VtColor.Red;
  GraphColour[7,2] := VtChWtAv.Plot.SeriesCollection.Item[9].Pen.VtColor.Blue;
  GraphColour[7,3] := VtChWtAv.Plot.SeriesCollection.Item[9].Pen.VtColor.Green;
  GraphColour[8,1] := VtChWtAv.Plot.SeriesCollection.Item[11].Pen.VtColor.Red;
  GraphColour[8,2] := VtChWtAv.Plot.SeriesCollection.Item[11].Pen.VtColor.Blue;
  GraphColour[8,3] := VtChWtAv.Plot.SeriesCollection.Item[11].Pen.VtColor.Green;
  }
end;

procedure TfmConcordiaDate.FormCreate(Sender: TObject);
begin
  TicksEvery := 10.0;
end;

procedure TfmConcordiaDate.DBNavigator1Click(Sender: TObject;
  Button: TNavigateBtn);
begin
  case Button of
    nbFirst  : begin
      iRec := 1;
    end;
    nbPrior  : begin
      iRec := iRec - 1;
      if (iRec < 1) then iRec := 1;
    end;
    nbNext   : begin
      iRec := iRec + 1;
      if (iRec > dmGdwtmp.cdsReg.RecordCount)
        then iRec := dmGdwtmp.cdsReg.RecordCount;
    end;
    nbLast   : begin
      iRec := dmGdwtmp.cdsReg.RecordCount;
    end;
  end;
  {
  VtChWtAv.Row := 1;
  VtChWtAv.Column := 13;
  VtChWtAv.Data := dmGdwtmp.cdsRegXRatio.AsString;
  VtChWtAv.Column := 14;
  VtChWtAv.Data := dmGdwtmp.cdsRegYRatio.AsString;
  VtChWtAv.Row := 2;
  VtChWtAv.Column := 13;
  VtChWtAv.Data := dmGdwtmp.cdsRegXRatio.AsString;
  VtChWtAv.Column := 14;
  VtChWtAv.Data := dmGdwtmp.cdsRegYRatio.AsString;
  VtChWtAv.Enabled := true;
  }
  ChartConcordia.Series[iCurrent].Clear;
  ChartConcordia.Series[iCurrent].AddXY(dmGdwtmp.cdsRegXRatio.AsFloat,dmGdwtmp.cdsRegYRatio.AsFloat);
end;

procedure TfmConcordiaDate.bbRecalculateClick(Sender: TObject);
var
  iCode : integer;
  MaxT, MinT : double;
  MaxX, MinX : double;
  RangeT : double;
begin
  MaxX := ChartConcordia.BottomAxis.Maximum;
  MinX := ChartConcordia.BottomAxis.Minimum;
  MaxT := Age238(MaxX,false,'neither');
  MinT := Age238(MinX,false,'neither');
  RangeT := MaxT - MinT;
  Val(eTicksEvery.Text,TicksEvery,iCode);
  if (iCode <> 0) then TicksEvery := 10.0;
  //if (RangeT > 5.0) then
  //begin
  //  eTicksEvery.Text := FormatFloat('###0',TicksEvery);
  //end else
  //begin
    eTicksEvery.Text := FormatFloat('###0.0',TicksEvery);
  //end;
  if (temporaryAnalType = 'A') then ConvertTeraWasserburg2Concordia;
  UpDateRFlg(Sender);
  FormShow(Sender);
end;

procedure TfmConcordiaDate.bbOKClick(Sender: TObject);
begin
  Close;
end;

end.



