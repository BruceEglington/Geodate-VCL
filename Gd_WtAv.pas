unit Gd_WtAv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBCtrls, ExtCtrls, Grids, DBGrids, Printers, OleCtrls,
  Mask, AxCtrls, TeeEdit, Series, TeEngine, TeeComma, TeeProcs, Chart,
  VCL.FlexCel.Core, FlexCel.XlsAdapter, FlexCel.Render, FlexCel.Preview,
  VclTee.TeeGDIPlus, VCLTee.TeeErrorPoint, VCLTee.TeeSpline, TeeTree,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TfmWtAv = class(TForm)
    pButtons: TPanel;
    pTopLeft: TPanel;
    pWtAverages: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lWtAvAugmentedSD: TLabel;
    eWtAv: TEdit;
    Label6: TLabel;
    eIcnt: TEdit;
    Label7: TLabel;
    eWtAvExpectedSD: TEdit;
    lWtAvPlus: TLabel;
    eWtAvPlus95: TEdit;
    eWtAvMinus95: TEdit;
    lWtAvMinus: TLabel;
    Label10: TLabel;
    eWtAvExpected95: TEdit;
    eWtAvObservedSD: TEdit;
    Label11: TLabel;
    eWtAvObserved95: TEdit;
    Label12: TLabel;
    eMSWD: TEdit;
    eWtAvAugmentedSD: TEdit;
    lWtAvAugmented95: TLabel;
    eWtAvAugmented95: TEdit;
    lErrorsBased: TLabel;
    lResultTitle: TLabel;
    pGraph: TPanel;
    pBottomRight: TPanel;
    SaveDialogSprdSheet: TSaveDialog;
    Label5: TLabel;
    eProbabilityOfF: TEdit;
    pResiduals: TPanel;
    DBeSample: TDBEdit;
    DBeUncertainty: TDBEdit;
    DBeMisfit: TDBEdit;
    DBePercentActual: TDBEdit;
    dbcbInclude: TDBCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    Label13: TLabel;
    dbnSamples: TDBNavigator;
    lResidual: TLabel;
    pTop: TPanel;
    Splitter1: TSplitter;
    pResultTitle: TPanel;
    Splitter2: TSplitter;
    pBottomLeft: TPanel;
    Splitter3: TSplitter;
    pBottom: TPanel;
    Splitter4: TSplitter;
    ChWtAv: TChart;
    Series7: TPointSeries;
    Series1: TLineSeries;
    Series2: TPointSeries;
    Series6: TPointSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series9: TLineSeries;
    Series10: TPointSeries;
    Series5: TLineSeries;
    Series8: TLineSeries;
    ChartEditor1: TChartEditor;
    TeeCommander1: TTeeCommander;
    Series11: TErrorPointSeries;
    Series12: TErrorPointSeries;
    Series13: TLineSeries;
    Series14: TPointSeries;
    cbLegend: TCheckBox;
    Panel1: TPanel;
    lEvap: TLabel;
    Panel2: TPanel;
    ChCum: TChart;
    LineSeries1: TLineSeries;
    LineSeries2: TLineSeries;
    LineSeries3: TLineSeries;
    LineSeries4: TLineSeries;
    PointSeries1: TPointSeries;
    LineSeries5: TLineSeries;
    LineSeries6: TLineSeries;
    PointSeries2: TPointSeries;
    PointSeries3: TPointSeries;
    ErrorPointSeries1: TErrorPointSeries;
    ErrorPointSeries2: TErrorPointSeries;
    LineSeries7: TLineSeries;
    PointSeries4: TPointSeries;
    PointSeries5: TPointSeries;
    TeeCommander2: TTeeCommander;
    lEllipseMagnif: TLabel;
    lModifyGraphSettings: TLabel;
    ChartEditor2: TChartEditor;
    Label14: TLabel;
    eNSamp: TEdit;
    lCaption1: TLabel;
    lCaption2: TLabel;
    Splitter5: TSplitter;
    pTreeSmp: TPanel;
    TreeView1: TTreeView;
    bClose: TButton;
    VirtualImageListWtAv: TVirtualImageList;
    bSprdSheet: TButton;
    bCumHist: TButton;
    bExport: TButton;
    bRecalculate: TButton;
    lWtAvIncl: TLabel;
    lWtAvPlusIncl: TLabel;
    eWtAvPlus95Incl: TEdit;
    lWtAvMinusIncl: TLabel;
    eWtAvMinus95Incl: TEdit;
    Series15: TLineSeries;
    procedure FormShow(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure bbSaveSheetClick(Sender: TObject);
    procedure bbCumHistClick(Sender: TObject);
    procedure bbExportmdlToXMLClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure dbnSamplesClick(Sender: TObject; Button: TNavigateBtn);
    procedure cbLegendClick(Sender: TObject);
    procedure dbcbIncludeClick(Sender: TObject);
    procedure bbSpreadSheetClick(Sender: TObject);
    procedure bbRecalculateClick(Sender: TObject);
    procedure ChWtAvClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure ChWtAvDblClick(Sender: TObject);
    procedure ChWtAvZoom(Sender: TObject);
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
    //
    CMaxX, CMinX : double;
    iRec : integer;
    SprdSheet : TXlsFile;
    procedure UpdateRFlg(Sender: TObject);
    procedure FillRegTable(Sender: TObject);
    procedure ShowCurrentPoint;
  public
    //Public declarations
    HistOK  : boolean;
    VarbNoX : integer;
    AllSame : boolean;
    FileVarStr : string;
    procedure Choose_Wt_Field;
    procedure HideResultLabels;
  end;

var
  fmWtAv: TfmWtAv;

implementation

{$R *.DFM}

uses
  GDW_varb, Allsorts, gd_drv, dmGdtmpDB,
  GDW_regp, gd_HstVl, mathproc,
  TeePNG, TeeSVGCanvas,VCLTee.TeeThemes,
  TeeJPEG, TeExport;

const
  VtChAxisIdX = 0;
  VtChAxisIdY = 1;

var
  tmpMSWD, SD, SD1, SD2   : double;
  SXX, MeanSlope,
  MeanSlopeError : double;
  iRec, iRecCount         : integer;
  GetHistValuesForm       : TfmGetHistValues;

procedure TfmWtAv.HideResultLabels;
begin
  lWtAvMinusIncl.Visible := false;
  eWtAvMinus95Incl.Visible := false;
  eWtAvPlus95Incl.Visible := false;
  lWtAvPlusIncl.Visible := false;
  lWtAvIncl.Visible := false;
  ChCum.Series[i]
end;

procedure TfmWtAv.UpdateRFlg(Sender: TObject);
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

procedure TfmWtAv.FillRegTable(Sender: TObject);
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
         cdsRegYWt.AsFloat := 0.0;
         cdsRegXDev.AsFloat := Residual[i,2];
         if (ErrTyp[i] in ['1','2']) then cdsRegXWtType.AsString := '%'
                                     else cdsRegXWtType.AsString := 'a';
         if (PFlg[i] = 'Y') then cdsRegPFlag.AsString := 'Y'
                            else cdsRegPFlag.AsString := 'N';
         cdsRegProject.AsString := ProjectName;
         cdsRegXRatio.AsFloat := Xtra[i];
         cdsRegi.AsInteger := i;

         cdsReg.Next;
     end;
     cdsReg.First;
     iRec := 1;
  end;
  if ((Residual[iRec,1] > (2.5 * ErrorWt[iRec,1]))) then
  begin
    lResidual.Visible := true;
  end else
  begin
    lResidual.Visible := false;
  end;
end;

procedure TfmWtAv.FormShow(Sender: TObject);
var
  tmpStr, tmpStrIncl : string; // string[20]
  i, j      : integer;
  jinc, iinc : integer;
  temp   : double;
  VarT : double;
  Idf : shortint;
  tmpMin, tmpMax : double;
  tmpSingle, tmpAge, tmpX, tmpY : double;
  tmpHistOKstr : string;
  MaxX, MinX, MaxY, MinY, MaxAge : double;
  SDAugmented,
  SDMult,SDPlus,SDMinus : double;
  Range, IncrementAmount : double;
  T_Mult_Visual : double;
  tAge, tOldAge,
  tPb64initial, tPb74initial : double;
  Node : TTreeNode;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
  NumLabelledTics : integer;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  HideResultLabels;
  // AllSame - this is true if all the samples are to be calculated at the same
  // age or for the same initial composition. It is false for calculations where
  // each sample entry provides it own separate age or initial composition.
  //
  // HistOK - true if it is appropriate to calculate a histogram to represent the
  // range of calculated weighted average values
  //
  // VarbNoX has the following values and meanings for different AnalType values
  // VarbNoX = 1     For all AnalType values this refers to the X ratio
  // VarbNoX = 2     For all AnalType values this refers to the Y ratio
  // VarbNoX = 3     For all AnalType values this refers to the Z ratio
  // VarbNoX = 4     For AnalType values '1','2','4','5','6','7','9','B','E',
  //                 'F','G'this refers to the Mean Y ratio at a user-specified date
  //                 For Analtype = '3', this is the Mean Source mu
  //                 For Analtype = '8','A', this is the Mean T Pb238U
  // VarbNoX = 5     For AnalType values '1','2','7' this is the
  //                 Mean Epsilon
  //                 For Analtype = 'F', this is the Mean Gamma
  // VarbNoX = 6     For AnalType values '1','2','4','5','6','7','9','B','E',
  //                 'F','G', this is the Mean T at a ratio
  //                 For AnalType '3','8','H','A', this is the Mean T 207Pb206Pb
  //                 For AnalType 'I', this is the Mean T 39Ar/40Ar and gets dealt with elsewhere
  // VarbNoX = 7     For AnalType values '1','2','7','9','E','F','G', this is
  //                 the Mean T CHUR
  // VarbNoX = 8     For AnalType values '1','2','7','9','E','F','G', this is
  //                 the Mean T DM
  // VarbNoX = 9     For AnalType values '8','A', this is the Mean T concordia
  // VarbNoX = 10    For AnalType value 'F', this is the Mean T RD; for
  //                 AnalType values '1','2','7','9','E','G', this is
  //                 the Mean T 2DM
  // VarbNoX = 11    For AnalType values '3', this is the Mean Pb isotope
  //                 composition for a given post-formation age
  // VarbNoX = 12    For AnalType values '3', this is the Mean apparent source
  //                 mu for a given post-formation age
  // VarbNoX = 13    For AnalType values '2','7', this is the Mean T 2DM
  //                 calculated from Age-Epsilon value pairs
  //
  //TSystemTheme.ApplyStyle(ChCum);
  //TSystemTheme.ApplyStyle(ChWtAv);
  with dmGdwtmp.cdsReg do
  begin
    Active := false;
  end;
  bExport.Enabled := false;
  ChCum.Visible := false;
  CMaxX := 0.0;
  CMinX := 0.0;
  T_Mult:=TMultiplier(1.0*N_Rep);
  ChWtAv.Series[iEllipsesExcluded].Clear;
  ChWtAv.Series[iEllipsesIncluded].Clear;
  ChWtAv.Series[iEnvelopeLower].Clear;
  ChWtAv.Series[iEnvelopeUpper].Clear;
  ChWtAv.Series[iCurveTic].Clear;
  ChWtAv.Series[iCurveLine].Clear;
  ChWtAv.Series[iRegressionLine].Clear;
  ChWtAv.Series[iDataExcluded].Clear;
  ChWtAv.Series[iDataIncluded].Clear;
  ChWtAv.Series[iErrorExcluded].Clear;
  ChWtAv.Series[iErrorIncluded].Clear;
  ChWtAv.Series[iEllipseConcordia].Clear;
  ChWtAv.Series[iDataConcordia].Clear;
  ChWtAv.Series[iCurrent].Clear;
  ChWtAv.Title.Caption := Title;
  ChWtAv.BottomAxis.Title.TextFormat:=ttfHtml;
  ChWtAv.LeftAxis.Title.TextFormat:=ttfHtml;
  if not AllSame then
  begin
    ChWtAv.Series[iCurveLine].Legend.Text := 'DM curve';
  end;
  if (EllipseMagnif = 1.0) then lEllipseMagnif.Caption := 'Error bars are 1 sigma';
  if (EllipseMagnif > 1.0) then lEllipseMagnif.Caption := 'Error bars are 95% confidence';
  //tmpHistOKstr := 'HistOK is false';
  //if (HistOK) then tmpHistOKstr := 'HistOK is true';
  //ShowMessage('VarbNoX = '+IntToStr(VarbNoX)+'   AnalType = '+AnalType+'   '+tmpHistOKstr);
  //if ((VarbNoX in [1,2,3,4,5,6,7,8,9,10]) and (HistOK)) then
  if ((VarbNoX in [1,2,3,4,5,6,7,8,9,10,11,12,13])) then
  begin
    case VarbNoX of
      1 : begin
        lResultTitle.Caption := 'Mean '+XRatioStr[IAnalTyp];
        FileVarStr := 'Mean Xratio';
        ChWtAv.Visible := true;
        ChWtAv.LeftAxis.Title.Text := GraphXRatioStr[IAnalTyp];
        if CharInSet(AnalType,['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          ChWtAv.LeftAxis.AxisValuesFormat := '#####0.000000';
        end;
        lCaption2.Caption := ' ';
      end;
      2 : begin
        lResultTitle.Caption := 'Mean '+YRatioStr[IAnalTyp];
        FileVarStr := 'Mean Yratio';
        ChWtAv.Visible := true;
        ChWtAv.LeftAxis.Title.Text := GraphYRatioStr[IAnalTyp];
        if CharInSet(AnalType,['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          ChWtAv.LeftAxis.AxisValuesFormat := '#####0.000000';
        end;
        lCaption2.Caption := ' ';
      end;
      3 : begin
        lResultTitle.Caption := 'Mean '+ZRatioStr[IAnalTyp];
        FileVarStr := 'Mean Zratio';
        ChWtAv.Visible := true;
        ChWtAv.LeftAxis.Title.Text := GraphZRatioStr[IAnalTyp];
        if CharInSet(AnalType,['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          ChWtAv.LeftAxis.AxisValuesFormat := '#####0.000000';
        end;
        lCaption2.Caption := ' ';
      end;
      4 : begin
        bExport.Enabled := false;
        if (AnalType in ['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          if (AllSame) then
          begin
            lResultTitle.Caption := 'Mean '+YRatioStr[IAnalTyp]+' ('+FormatFloat('###0.0',Age)+' Ma)';
            FileVarStr := 'Mean Y ratio at '+FormatFloat('###0.0',Age)+' Ma';
            ChWtAv.LeftAxis.Title.Text := GraphYRatioStr[IAnalTyp]+' ('+FormatFloat('###0.0',Age)+' Ma)';
            ChWtAv.Visible := true;
            lCaption2.Caption := 'calculated for a common date = '+FormatFloat('###0.0',Age)+' +/- '+FormatFloat('###0.0',AgeError)+' Ma (1 sigma)';
          end else
          begin
            ChWtAv.Visible := true; //need to check if this should be false
            lResultTitle.Caption := 'Mean '+YRatioStr[IAnalTyp];
            FileVarStr := 'Mean Y ratio at date';
            ChWtAv.LeftAxis.Title.Text := GraphYRatioStr[IAnalTyp];
            lCaption2.Caption := 'calculated for individually specified dates and 1 sigma uncertainties';
          end;
          if CharInSet(AnalType,['1','2','4','5','6','7','9','B','E','F','G']) then
          begin
            ChWtAv.LeftAxis.AxisValuesFormat := '#####0.000000';
          end;
        end;
        if (AnalType in ['3']) then
        begin
          lResultTitle.Caption := 'Mean source 238U/204Pb';
          FileVarStr := 'Mean Source mu';
          ChWtAv.LeftAxis.Title.TextFormat:=ttfHtml;
          ChWtAv.LeftAxis.Title.Text := 'Model source <sup>238</sup>U/<sup>204</sup>Pb';
          //ChWtAv.LeftAxis.Title.Text := 'Model source 238U/204Pb';
          ChWtAv.LeftAxis.AxisValuesFormat := '#####0.000';
          if (not AllSame) then ChWtAv.BottomAxis.Title.Text := 'Date (Ma)';
          lCaption2.Caption := ' ';
        end;
        if (AnalType in ['8','A']) then
        begin
          lResultTitle.Caption := 'Mean T (206Pb/238U)';
          FileVarStr := 'Mean T Pb238U';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.TextFormat:=ttfHtml;
          ChWtAv.LeftAxis.Title.Text := 'T (<sup>206</sup>Pb/<sup>238</sup>U)';
          ChWtAv.LeftAxis.AxisValuesFormat := '#####0.00';
          lCaption2.Caption := ' ';
          if (DecayConstUncertainty[ord(at238UPb)] > 0.0) then
          begin
            lWtAvMinusIncl.Visible := true;
            eWtAvMinus95Incl.Visible := true;
            eWtAvPlus95Incl.Visible := true;
            lWtAvPlusIncl.Visible := true;
            lWtAvIncl.Visible := true;
            ChCum.Series[iCurveLinePlus].Visible := true;
            ChCum.Series[iCurveLinePlus].Legend.Visible := true;
          end;
        end;
        if (AllSame) then ChWtAv.Visible := true
                     else ChWtAv.Visible := true;  //need to check if this should be false
      end;
      5 : begin
        lResultTitle.Caption := 'Mean Epsilon';
        if (AnalType in ['F']) then lResultTitle.Caption := 'Mean Gamma';
        FileVarStr := 'Mean Epsilon';
        if (AnalType in ['F']) then FileVarStr := 'Mean Gamma';
        if (AllSame) then
        begin
          ChWtAv.Visible := true;
          ChWtAv.LeftAxis.Title.Text := 'Epsilon ('+FormatFloat('###0.0',Age)+' Ma)';
          if (AnalType in ['F']) then ChWtAv.LeftAxis.Title.Text := 'Gamma ('+FormatFloat('###0.0',Age)+' Ma)';
          lCaption2.Caption := 'calculated for a common date = '+FormatFloat('###0.0',Age)+' +/- '+FormatFloat('###0.0',AgeError)+' Ma (1 sigma)';
        end else
        begin
          ChWtAv.Visible := true; //need to check if this should be false
          ChWtAv.LeftAxis.Title.Text := 'Epsilon';
          if (AnalType in ['F']) then ChWtAv.LeftAxis.Title.Text := 'Gamma';
          lCaption2.Caption := 'calculated for individually specified dates and 1 sigma uncertainties';
        end;
        ChWtAv.LeftAxis.AxisValuesFormat := '#####0.000000';
      end;
      6 : begin
        if (AnalType in ['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          lResultTitle.Caption := 'Mean T ('+FormatFloat('###0.000000',InitRatio)+')';
          FileVarStr := 'Mean T ratio';
          ChWtAv.LeftAxis.Title.Text := 'T ('+FormatFloat('###0.000000',InitRatio)+')';
          lCaption2.Caption := 'calculated for a common initial ratio = '+FormatFloat('###0.000000',InitRatio)+' +/- '+FormatFloat('###0.000000',InitRatioError)+' (1 sigma)';
        end;
        if (AnalType in ['3','8','H','A']) then
        begin
          lResultTitle.Caption := 'Mean T (207Pb/206Pb)';
          FileVarStr := 'Mean T PbPb';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.TextFormat:=ttfHtml;
          ChWtAv.LeftAxis.Title.Text := 'T (<sup>207</sup>Pb/<sup>206</sup>Pb)';
          if (AnalType in ['8','H']) then
            ChWtAv.LeftAxis.Title.Text := 'T (<sup>207</sup>Pb<sup>*</sup>/<sup>206</sup>Pb<sup>*</sup>)';
          lCaption2.Caption := ' ';
          if ((AnalType in ['8','H','A']) and (DecayConstUncertainty[ord(at238UPb)] > 0.0)) then
          begin
            lWtAvMinusIncl.Visible := true;
            eWtAvMinus95Incl.Visible := true;
            eWtAvPlus95Incl.Visible := true;
            lWtAvPlusIncl.Visible := true;
            lWtAvIncl.Visible := true;
            ChCum.Series[iCurveLinePlus].Visible := true;
            ChCum.Series[iCurveLinePlus].Legend.Visible := true;
          end;
        end;
        if (AllSame) then ChWtAv.Visible := true
                     else ChWtAv.Visible := true; //need to check if this should be false
      end;
      7 : begin
        if (AnalType in ['1','2','7','9','E','G']) then
        begin
          lResultTitle.Caption := 'Mean T (CHUR)';
          FileVarStr := 'Mean T CHUR';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (CHUR)';
          lCaption2.Caption := ' ';
        end;
        if (AnalType in ['F']) then
        begin
          lResultTitle.Caption := 'Mean T (MA)';
          FileVarStr := 'Mean T MA';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (MA)';
          lCaption2.Caption := ' ';
        end;
        ChWtAv.Visible := true; //need to check if this should be true
      end;
      8 : begin
        if (AnalType in ['1','2','7','9','E','F','G']) then
        begin
          lResultTitle.Caption := 'Mean T (DM)';
          FileVarStr := 'Mean T DM';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (DM)';
          lCaption2.Caption := ' ';
        end;
        ChWtAv.Visible := true;  //false
      end;
      9 : begin
        // no longer applies here. Now use the Concordia weighted average form
        if (AnalType in ['8','A']) then
        begin
          lResultTitle.Caption := 'Mean T (concordia)';
          FileVarStr := 'Mean T concordia';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (Concordia)';
          lCaption2.Caption := ' ';
        end;
        if (AllSame) then ChWtAv.Visible := true  //false
                     else ChWtAv.Visible := true;
      end;
      10 : begin
        if (AnalType in ['F']) then
        begin
          lResultTitle.Caption := 'Mean T (RD)';
          FileVarStr := 'Mean T RD';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (RD)';
          lCaption2.Caption := ' ';
        end;
        if (AnalType in ['1','2','7','9','E','G']) then
        begin
          lResultTitle.Caption := 'Mean T (2DM)';
          FileVarStr := 'Mean T 2DM';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (2DM)';
            if (AllSame) then
            begin
              lResultTitle.Caption := 'Mean T (2DM)';
              FileVarStr := 'Mean T 2DM';
              ChWtAv.LeftAxis.Title.Text := 'T (2DM)';
              ChWtAv.Visible := true;
              lCaption2.Caption := 'calculated for a common date = '+FormatFloat('###0.0',Age)+' Ma    and ignoring crystallisation age uncertainties';
            end else
            begin
              ChWtAv.Visible := true;
              lResultTitle.Caption := 'Mean T (2DM)';
              FileVarStr := 'Mean T 2DM';
              bExport.Enabled := true;
              ChWtAv.LeftAxis.Title.Text := 'T (2DM)';
              lCaption2.Caption := 'calculated for individually specified dates and ignoring crystallisation age uncertainties';
            end;
        end;
        ChWtAv.Visible := true; //need to check if this should be true
      end;
      11 : begin
        bExport.Enabled := false;
        if (AnalType in ['3']) then
        begin
          lResultTitle.Caption := 'Mean initial composition '+'(Date)';
          FileVarStr := 'Mean Initial composition at date';
          ChWtAv.LeftAxis.Title.TextFormat:=ttfHtml;
          ChWtAv.LeftAxis.Title.Text := '<sup>207</sup>Pb/<sup>204</sup>Pb (Date)';
          ChWtAv.BottomAxis.Title.TextFormat:=ttfHtml;
          ChWtAv.BottomAxis.Title.Text := '<sup>206</sup>Pb/<sup>204</sup>Pb (Date)';
          lCaption2.Caption := ' ';
        end;
        if (AllSame) then ChWtAv.Visible := true
                     else ChWtAv.Visible := true;  //need to check if this should be false
      end;
      13 : begin
        if (AnalType in ['J']) then
        begin
          lResultTitle.Caption := 'Mean T (2DM)';
          FileVarStr := 'Mean T 2DM';
          bExport.Enabled := true;
          ChWtAv.LeftAxis.Title.Text := 'T (2DM)';
            if (AllSame) then
            begin
              lResultTitle.Caption := 'Problem';
              FileVarStr := 'Problem';
              ChWtAv.LeftAxis.Title.Text := 'Problem';
              ChWtAv.Visible := true;
              lCaption2.Caption := 'Problem';
            end else
            begin
              ChWtAv.Visible := true;
              lResultTitle.Caption := 'Mean T (2DM)';
              FileVarStr := 'Mean T 2DM';
              bExport.Enabled := true;
              ChWtAv.LeftAxis.Title.Text := 'T (2DM)';
              lCaption2.Caption := 'calculated for individually specified dates and ignoring crystallisation age uncertainties';
            end;
        end;
        ChWtAv.Visible := true; //need to check if this should be true
      end;
    end;
  end;
  lEvap.Visible := false;
  lCaption1.Caption := lResultTitle.Caption;
  //ShowMessage('1');
  //Application.ProcessMessages;
  Choose_Wt_Field;
  //ShowMessage('2');
  //Application.ProcessMessages;
  if (VarbNoX in [1..11,13]) then
  begin
    Icnt := 0;
    // No graphing in this section. Only calculations for values appropriate
    // to the weighted averages
    //ShowMessage('3');
    //Application.ProcessMessages;
    if (VarbNoX in [1..10,13]) then WtAver(NumberOfPoints,temp,tmpMSWD,SD1,SD2,Icnt);
    //ShowMessage('4');
    //Application.ProcessMessages;
    if (Icnt > 0) then
    begin
      for i := 1 to NumberOfPoints do
      begin
        Residual[i,1] := 0.0;
        Residual[i,2] := Xtra[i]-temp;
        if ErrTyp[i] in ['1','2'] then
        begin
          if (Xtra[i] <> 0.0) then Residual[i,2] := Residual[i,2]/Xtra[i]*100.0
                              else Residual[i,2] := 0.0;
        end;
      end;
      //eTitle.Text := Title;
      if ((IAnalTyp = 17)) then
      begin
        Str(BlanketZErrVal:8:4,tmpStr);
        lEvap.Caption := 'Minimum 1 sigma % error for evaporation 207Pb/206Pb data set = '
             + tmpStr;
        lEvap.Visible := true;
      end;
      Str(temp:12:6,tmpStr);
      eWtAv.Text := tmpStr;
      Str(temp:12:6,tmpStrIncl);
      //eWtAvIncl.Text := tmpStrIncl;
      Str(Icnt:3,tmpStr);
      eIcnt.Text := tmpStr;
      Str(NumberOfPoints:3,tmpStr);
      eNsamp.Text := tmpStr;
      lWtAvPlus.Visible := true;
      lWtAvMinus.Visible := true;
      eWtAvPlus95.Visible := true;
      eWtAvMinus95.Visible := true;
      //lWtAvPlusIncl.Visible := true;
      //lWtAvMinusIncl.Visible := true;
      //eWtAvPlus95Incl.Visible := true;
      //eWtAvMinus95Incl.Visible := true;
      Str(SD1:12:6,tmpStr);
      eWtAvExpectedSD.Text := tmpStr;
      Str((SD1*TMultiplier(1.0*N_Rep)):12:6,tmpStr);
      Str((SD1*TMultiplier(1.0*N_Rep)):12:6,tmpStrIncl);
      eWtAvExpected95.Text := tmpStr;
      Str(SD2:12:6,tmpStr);
      Str(SD2:12:6,tmpStrIncl);
      eWtAvObservedSD.Text := tmpStr;
      if (Icnt > 1) then
      begin
        //SDMult := SD2*TMultiplier(1.0*(Icnt-1));
        Str((SD2*TMultiplier(1.0*(Icnt-1))):12:6,tmpStr);
        Str((SD2*TMultiplier(1.0*(Icnt-1))):12:6,tmpStrIncl);
      end
      else begin
        //SDMult := SD2*TMultiplier(1.0*(N_Rep));
        Str((SD2*TMultiplier(1.0*(N_Rep))):12:6,tmpStr);
        Str((SD2*TMultiplier(1.0*(N_Rep))):12:6,tmpStrIncl);
      end;
      eWtAvObserved95.Text := tmpStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(Icnt-1),1.0*(N_Rep),tmpMSWD,1);
      eProbabilityOfF.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfF.Font.Color := clRed
                                     else eProbabilityOfF.Font.Color := clBlue;
      if (ProbabilityOfFit >= FAlpha) then
      begin
        if (SD1 >= SD2) then
        begin
          SDMult := SD1*TMultiplier(1.0*(N_Rep));
          Str((SD1*TMultiplier(1.0*(N_Rep))):12:6,tmpStr);
          Str((SD1*TMultiplier(1.0*(N_Rep))):12:6,tmpStrIncl);
        end;
        if (SD1 < SD2) then
        begin
          if (Icnt > 1) then
          begin
            SDMult := SD2*TMultiplier(1.0*(Icnt-1));
            Str((SD2*TMultiplier(1.0*(Icnt-1))):12:6,tmpStr);
            Str((SD2*TMultiplier(1.0*(Icnt-1))):12:6,tmpStrIncl);
          end
          else begin
            SDMult := SD2*TMultiplier(1.0*(N_Rep));
            Str((SD2*TMultiplier(1.0*(N_Rep))):12:6,tmpStr);
            Str((SD2*TMultiplier(1.0*(N_Rep))):12:6,tmpStrIncl);
          end;
        end;
      end;
      eWtAvObserved95.Text := tmpStr;
      lWtAvAugmentedSD.Visible := false;
      lWtAvAugmented95.Visible := false;
      eWtAvAugmentedSD.Visible := false;
      eWtAvAugmented95.Visible := false;
      if (ProbabilityOfFit < FAlpha) then
      begin
        SDAugmented := SD1*Sqrt(tmpMSWD);
        Str((SDAugmented):12:6,tmpStr);
        Str((SDAugmented):12:6,tmpStrIncl);
        eWtAvAugmentedSD.Text := tmpStr;
        SDMult := SDAugmented*TMultiplier(1.0*(Icnt-1));
        Str((SDMult):12:6,tmpStr);
        Str((SDMult):12:6,tmpStrIncl);
        eWtAvAugmented95.Text := tmpStr;
        lWtAvAugmentedSD.Visible := true;
        lWtAvAugmented95.Visible := true;
        eWtAvAugmentedSD.Visible := true;
        eWtAvAugmented95.Visible := true;
        {
        ProbabilityOfFit := ProbabilityOfF(1.0*(Icnt-1),1.0*(Icnt-1),t1,1);
        eProbabilityOfF.Text := FormatFloat('  0.000',ProbabilityOfFit);
        }
      end;
      Str(tmpMSWD:9:3,tmpStr);
      eMSWD.Text := tmpStr;
      lErrorsBased.Visible := true;
      if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
      begin
        Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
        T_Mult := TMultiplier(1.0*(N_Rep));
        lErrorsBased.Caption := 'Errors based on expected s.d. of mean and t = '
          +tmpStr;
        eWtAvPlus95.Text := eWtAvExpected95.Text;
        eWtAvMinus95.Text := eWtAvExpected95.Text;
        //calculate with decay constant uncertainties
        eWtAvPlus95Incl.Text := eWtAvExpected95.Text;
        eWtAvMinus95Incl.Text := eWtAvExpected95.Text;
      end;
      if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
      begin
        Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
        T_Mult := TMultiplier(1.0*(N_Rep));
        lErrorsBased.Caption := 'Errors based on observed s.d. of mean and t = '
          +tmpStr;
        eWtAvPlus95.Text := eWtAvObserved95.Text;
        eWtAvMinus95.Text := eWtAvObserved95.Text;
        //calculate with decay constant uncertainties
        eWtAvPlus95Incl.Text := eWtAvObserved95.Text;
        eWtAvMinus95Incl.Text := eWtAvObserved95.Text;
      end;
      if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
      begin
        Str(TMultiplier(1.0*(Icnt-1)):7:2,tmpStr);
        T_Mult := TMultiplier(1.0*(Icnt-1));
        lErrorsBased.Caption := 'Errors augmented by SQRT(MSWD). t = '
          +tmpStr;
        eWtAvPlus95.Text := eWtAvAugmented95.Text;
        eWtAvMinus95.Text := eWtAvAugmented95.Text;
        //calculate with decay constant uncertainties
        eWtAvPlus95Incl.Text := eWtAvAugmented95.Text;
        eWtAvMinus95Incl.Text := eWtAvAugmented95.Text;
      end;
      if ((AnalType in ['8','H','A']) and (VarbNoX in [6])) then
      begin
        ProbabilityOfFit := ProbabilityOfF(1.0*(Icnt-1),1.0*(N_Rep),tmpMSWD,1);
        eProbabilityOfF.Text := FormatFloat('  0.000',ProbabilityOfFit);
        if (ProbabilityOfFit < FAlpha) then eProbabilityOfF.Font.Color := clRed
                                       else eProbabilityOfF.Font.Color := clBlue;
        //ShowMessage('5');
        //Application.ProcessMessages;
        IncludeDCUncertainty := false;
        AgePlusAgeMinus := 'neither';
        Age:=PbPbAge(temp,IncludeDCUncertainty,AgePlusAgeMinus);
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
          Slope:=temp+SD1*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
          Slope:=temp+SD2*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
          Slope:=temp+SDAugmented*TMultiplier(1.0*(Icnt-1));
        IncludeDCUncertainty := false;
        AgePlusAgeMinus := 'neither';
        UprUprAgeError:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
        //ShowMessage('6');
        //Application.ProcessMessages;
        IncludeDCUncertainty := true;
        AgePlusAgeMinus := UncertaintyMinus;
        UprUprAgeErrorIncl:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
        //ShowMessage('plus age error ='+FormatFloat('####0.000',UprUprAgeErrorIncl/1.0e6));
        UprUprAgeErrorIncl:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
        //ShowMessage('7');
        //Application.ProcessMessages;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
          Slope:=temp-SD1*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
          Slope:=temp-SD2*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
          Slope:=temp-SDAugmented*TMultiplier(1.0*(Icnt-1));
        IncludeDCUncertainty := false;
        AgePlusAgeMinus := 'neither';
        if (Slope<0.0) then UprLwrAgeError:=Age
                       else UprLwrAgeError:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
        IncludeDCUncertainty := true;
        AgePlusAgeMinus := UncertaintyPlus;
        if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                       else UprLwrAgeErrorIncl:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
        //ShowMessage('minus age error ='+FormatFloat('####0.000',(UprLwrAgeErrorIncl+Age)/1.0e6));
        Age:=Age/1.0e6;
        UprUprAgeError:=UprUprAgeError/1.0e6;
        UprLwrAgeError:=UprLwrAgeError/1.0e6;
        UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
        UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
        //ShowMessage('8');
        //Application.ProcessMessages;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
        begin
          Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors based on expected s.d. of mean and t = '
            +tmpStr;
          eWtAvPlus95.Text := eWtAvExpected95.Text;
          eWtAvMinus95.Text := eWtAvExpected95.Text;
          //calculate with decay constant uncertainties
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvPlus95Incl.Text := tmpStr;
          Str(UprLwrAgeErrorIncl:9:2,tmpStr);
          eWtAvMinus95Incl.Text := tmpStr;
        end;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
        begin
          Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors based on observed s.d. of mean and t = '
            +tmpStr;
          eWtAvPlus95.Text := eWtAvObserved95.Text;
          eWtAvMinus95.Text := eWtAvObserved95.Text;
          //calculate with decay constant uncertainties
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvPlus95Incl.Text := tmpStr;
          Str(UprLwrAgeErrorIncl:9:2,tmpStr);
          eWtAvMinus95Incl.Text := tmpStr;
        end;
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
        begin
          Str(TMultiplier(1.0*(Icnt-1)):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors augmented by SQRT(MSWD). t = '
            +tmpStr;
          eWtAvPlus95.Text := eWtAvAugmented95.Text;
          eWtAvMinus95.Text := eWtAvAugmented95.Text;
          //calculate with decay constant uncertainties
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvPlus95Incl.Text := tmpStr;
          Str(UprLwrAgeErrorIncl:9:2,tmpStr);
          eWtAvMinus95Incl.Text := tmpStr;
        end;
        {
        if (ProbabilityOfFit < FAlpha) then
        begin
          Age:=PbPbAge(temp);
          Slope:=temp+SD1*Sqrt(t1)*TMultiplier(1.0*(Icnt-1));
          UprUprAgeError:=PbPbAge(Slope)-Age;
          Slope:=temp-SD1*Sqrt(t1)*TMultiplier(1.0*(Icnt-1));
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age-PbPbAge(Slope);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          Str((TMultiplier(1.0*(Icnt-1))):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors augmented by SQRT(MSWD).   t = '
            +tmpStr;
        end;
        }
        lErrorsBased.Visible := true;
        Str(Age:8:2,tmpStr);
        eWtAv.Text := tmpStr;
        Str(Icnt:3,tmpStr);
        eIcnt.Text := tmpStr;
        Str(NumberOfPoints:3,tmpStr);
        eNsamp.Text := tmpStr;
        Str(UprUprAgeError:9:2,tmpStr);
        eWtAvPlus95.Text := tmpStr;
        Str(UprLwrAgeError:9:2,tmpStr);
        eWtAvMinus95.Text := tmpStr;
        lWtAvPlus.Visible := true;
        lWtAvMinus.Visible := true;
        eWtAvPlus95.Visible := true;
        eWtAvMinus95.Visible := true;
        lWtAvPlusIncl.Visible := true;
        lWtAvMinusIncl.Visible := true;
        eWtAvPlus95Incl.Visible := true;
        eWtAvMinus95Incl.Visible := true;
      end;
      {
      if ((AnalType in ['3']) and (VarbNoX in [4])) then
      begin
        if (AllSame) then tAge := Age/1.0e6
        else tAge := Ratio[1,3];
        ModelMuSourceInitialValue(Mu,Age,OldAge,X0,Y0: double; var Pb64initial: double; var Pb74initial: double);
      end;
      }
      if ((AnalType in ['8','A']) and (VarbNoX in [4])) then
      begin
        IncludeDCUncertainty := false;
        AgePlusAgeMinus := 'neither';
        ProbabilityOfFit := ProbabilityOfF(1.0*(Icnt-1),1.0*(N_Rep),tmpMSWD,1);
        eProbabilityOfF.Text := FormatFloat('  0.000',ProbabilityOfFit);
        if (ProbabilityOfFit < FAlpha) then eProbabilityOfF.Font.Color := clRed
                                       else eProbabilityOfF.Font.Color := clBlue;
        Age:=Age238(temp,IncludeDCUncertainty,AgePlusAgeMinus);
        //ShowMessage('Age Slope = '+FormatFloat('###0.000000',temp)+'__'+FormatFloat('###0.000',Age238(temp,IncludeDCUncertainty,AgePlusAgeMinus)/1.0e6)+' Ma');
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
          Slope:=temp+SD1*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
          Slope:=temp+SD2*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
          Slope:=temp+SDAugmented*TMultiplier(1.0*(Icnt-1));
        //ShowMessage('Plus Slope = '+FormatFloat('###0.000000',Slope)+'__'+FormatFloat('###0.000',Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus)/1.0e6)+' Ma');
        UprUprAgeError:=Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
          Slope:=temp-SD1*TMultiplier(1.0*N_Rep);
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
          Slope:=temp-SD2*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
          Slope:=temp-SDAugmented*TMultiplier(1.0*(Icnt-1));
        //ShowMessage('Minus Slope = '+FormatFloat('###0.000000',Slope)+'__'+FormatFloat('###0.000',Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus)/1.0e6)+' Ma');
        if (Slope<0.0) then UprLwrAgeError:=Age
                       else UprLwrAgeError:=Age-Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
        IncludeDCUncertainty := true;
        AgePlusAgeMinus := UncertaintyPlus;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
          Slope:=temp+SD1*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
          Slope:=temp+SD2*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
          Slope:=temp+SDAugmented*TMultiplier(1.0*(Icnt-1));
        //ShowMessage('Plus Slope = '+FormatFloat('###0.000000',Slope)+'__'+FormatFloat('###0.000',Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus)/1.0e6)+' Ma');
        UprUprAgeErrorIncl:=Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
        AgePlusAgeMinus := UncertaintyMinus;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
          Slope:=temp-SD1*TMultiplier(1.0*N_Rep);
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
          Slope:=temp-SD2*TMultiplier(1.0*(N_Rep));
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
          Slope:=temp-SDAugmented*TMultiplier(1.0*(Icnt-1));
        //ShowMessage('Minus Slope = '+FormatFloat('###0.000000',Slope)+'__'+FormatFloat('###0.000',Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus)/1.0e6)+' Ma');
        if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                       else UprLwrAgeErrorIncl:=Age-Age238(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
        Age:=Age/1.0e6;
        UprUprAgeError:=UprUprAgeError/1.0e6;
        UprLwrAgeError:=UprLwrAgeError/1.0e6;
        UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
        UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 >= SD2)) then
        begin
          Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors based on expected s.d. of mean and t = '
            +tmpStr;
        end;
        if ((ProbabilityOfFit >= FAlpha) and (SD1 < SD2)) then
        begin
          Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors based on observed s.d. of mean and t = '
            +tmpStr;
        end;
        if ((ProbabilityOfFit < FAlpha) and (SD1 < SD2)) then
        begin
          Str(TMultiplier(1.0*(Icnt-1)):7:2,tmpStr);
          lErrorsBased.Caption := 'Date errors augmented by SQRT(MSWD). t = '
            +tmpStr;
        end;
        lErrorsBased.Visible := true;
        Str(Age:8:2,tmpStr);
        eWtAv.Text := tmpStr;
        Str(Icnt:3,tmpStr);
        eIcnt.Text := tmpStr;
        Str(NumberOfPoints:3,tmpStr);
        eNsamp.Text := tmpStr;
        Str(UprUprAgeError:9:2,tmpStr);
        eWtAvPlus95.Text := tmpStr;
        Str(UprLwrAgeError:9:2,tmpStr);
        eWtAvMinus95.Text := tmpStr;
        Str(UprUprAgeErrorIncl:9:2,tmpStr);
        eWtAvPlus95Incl.Text := tmpStr;
        Str(UprLwrAgeErrorIncl:9:2,tmpStr);
        eWtAvMinus95Incl.Text := tmpStr;
        lWtAvPlus.Visible := true;
        lWtAvMinus.Visible := true;
        eWtAvPlus95.Visible := true;
        eWtAvMinus95.Visible := true;
        lWtAvPlusIncl.Visible := true;
        lWtAvMinusIncl.Visible := true;
        eWtAvPlus95Incl.Visible := true;
        eWtAvMinus95Incl.Visible := true;
      end;
    end else
    begin
      lEvap.Visible := false;
      eWtAv.Text := '';
      Str(Icnt:3,tmpStr);
      eIcnt.Text := tmpStr;
      Str(NumberOfPoints:3,tmpStr);
      eNsamp.Text := tmpStr;
      lWtAvPlus.Visible := false;
      lWtAvMinus.Visible := false;
      eWtAvPlus95.Visible := false;
      eWtAvMinus95.Visible := false;
      lWtAvPlusIncl.Visible := false;
      lWtAvMinusIncl.Visible := false;
      eWtAvPlus95Incl.Visible := false;
      eWtAvMinus95Incl.Visible := false;
      eWtAvExpectedSD.Text := '';
      eWtAvExpected95.Text := '';
      eWtAvObservedSD.Text := '';
      eWtAvObserved95.Text := '';
      lWtAvAugmentedSD.Visible := false;
      lWtAvAugmented95.Visible := false;
      eWtAvAugmentedSD.Visible := false;
      eWtAvAugmented95.Visible := false;
      eMSWD.Text := '';
      lErrorsBased.Visible := false;
    end;
  end;
  // All graphing should follow this point in the code of this subroutine
  iinc := 0;
  jinc := 0;
  MaxX := -1e9;
  MinX :=  9e9;
  MaxY := -1e9;
  MinY :=  9e9;
  MaxAge := 0.0;
  if ((Icnt > 0) and AllSame and (VarbNoX in [1..3,5,7,8,10])) then
  begin
    ChWtAv.Series[iRegressionLine].AddXY(0.5,temp);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iRegressionLine].AddXY(i,temp);
    end;
    ChWtAv.Series[iRegressionLine].AddXY(0.5+1.0*NumberOfPoints,temp);
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5,temp-SDMult);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeLower].AddXY(i,temp-SDMult);
    end;
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5+1.0*NumberOfPoints,temp-SDMult);
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5,temp+SDMult);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeUpper].AddXY(i,temp+SDMult);
    end;
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5+1.0*NumberOfPoints,temp+SDMult);
  end;
  if ((Icnt > 0) and AllSame and (VarbNoX in [6]) and (AnalType in ['1'..'7','9','B'..'H'])) then
  begin
    ChWtAv.Series[iRegressionLine].AddXY(0.5,temp);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iRegressionLine].AddXY(i,temp);
    end;
    ChWtAv.Series[iRegressionLine].AddXY(0.5+1.0*NumberOfPoints,temp);
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5,temp-SDMult);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeLower].AddXY(i,temp-SDMult);
    end;
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5+1.0*NumberOfPoints,temp-SDMult);
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5,temp+SDMult);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeUpper].AddXY(i,temp+SDMult);
    end;
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5+1.0*NumberOfPoints,temp+SDMult);
  end;
  if ((Icnt > 0) and AllSame and (VarbNoX in [4]) and (AnalType in ['1'..'7','9','B'..'H'])) then
  begin
    ChWtAv.Series[iRegressionLine].AddXY(0.5,temp);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iRegressionLine].AddXY(i,temp);
    end;
    ChWtAv.Series[iRegressionLine].AddXY(0.5+1.0*NumberOfPoints,temp);
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5,temp-SDMult);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeLower].AddXY(i,temp-SDMult);
    end;
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5+1.0*NumberOfPoints,temp-SDMult);
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5,temp+SDMult);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeUpper].AddXY(i,temp+SDMult);
    end;
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5+1.0*NumberOfPoints,temp+SDMult);
  end;
  if ((Icnt > 0) and AllSame and (VarbNoX in [4,6]) and (AnalType in ['8','A'])) then
  begin
    ChWtAv.Series[iRegressionLine].AddXY(0.5,Age);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iRegressionLine].AddXY(i,Age);
    end;
    ChWtAv.Series[iRegressionLine].AddXY(0.5+1.0*NumberOfPoints,Age);
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5,Age-UprLwrAgeError);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeLower].AddXY(i,Age-UprLwrAgeError);
    end;
    ChWtAv.Series[iEnvelopeLower].AddXY(0.5+1.0*NumberOfPoints,Age-UprLwrAgeError);
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5,Age+UprUprAgeError);
    for i := 1 to NumberOfPoints do
    begin
      ChWtAv.Series[iEnvelopeUpper].AddXY(i,Age+UprUprAgeError);
    end;
    ChWtAv.Series[iEnvelopeUpper].AddXY(0.5+1.0*NumberOfPoints,Age+UprUprAgeError);
  end;
  if (EllipseMagnif > 1.0)
    then T_Mult_Visual:=TMultiplier(1.0*N_Rep)
    else T_Mult_Visual := 1.0;
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  for i := 1 to NumberOfPoints do
  begin
    if (PFlg[i] = 'Y') then
    begin
      if ((AllSame) and (VarbNoX in [1..3])) then
      begin
        if (RFlg[i] = 'Y') then
        begin
          iinc := iinc + 1;
          Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra1[i]*T_Mult_Visual);
        end;
        if (RFlg[i] = 'N') then
        begin
          jinc := jinc + 1;
          Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra1[i]*T_Mult_Visual);
        end;
        if (MaxY < Ratio[i,VarbNoX]) then MaxY := Ratio[i,VarbNox];
        if (MinY > Ratio[i,VarbNoX]) then MinY := Ratio[i,VarbNoX];
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(i,Xtra[i]);
        end;
      end;
      if ((AllSame) and (VarbNox in [4,5,7,8,9,10])) then
      begin
        if (AnalType in ['1'..'2','4'..'7','9','B'..'H']) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (RFlg[i] = 'Y') then
          begin
            iinc := iinc + 1;
            Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (AnalType in ['3']) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (RFlg[i] = 'Y') then
          begin
            iinc := iinc + 1;
            Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(i,Xtra[i]);
        end;
        if (AnalType in ['8','A']) then
        begin
          temp := Xtra[i];
          Age:=Age238(Xtra[i],IncludeDCUncertainty,UncertaintyNeither);
          Slope:=temp+Xtra1[i]*TMultiplier(1.0*N_Rep);
          UprUprAgeError:=Age238(Slope,IncludeDCUncertainty,UncertaintyNeither)-Age;
          Slope:=temp-Xtra1[i]*TMultiplier(1.0*N_Rep);
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age-Age238(Slope,IncludeDCUncertainty,UncertaintyNeither);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          if (Age > MaxAge) then MaxAge := Age;
          Xtra[i] := Age;
          Xtra1[i] := UprUprAgeError;
          Xtra2[i] := UprLwrAgeError;
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (RFlg[i] = 'Y') then
          begin
            iinc := iinc + 1;
            Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(i,Xtra[i]);
        end;
      end;
      if ((AllSame) and (VarbNoX in [6])) then
      begin
        if (AnalType in ['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (RFlg[i] = 'Y') then
          begin
            iinc := iinc + 1;
            Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (AnalType in ['3']) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (RFlg[i] = 'Y') then
          begin
            iinc := iinc + 1;
            Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (AnalType in ['8','H','A']) then
        begin
          //ShowMessage('9');
          //Application.ProcessMessages;
          temp := Xtra[i];
          IncludeDCUncertainty := false;
          AgePlusAgeMinus := 'neither';
          Age:=PbPbAge(temp,IncludeDCUncertainty,AgePlusAgeMinus);
          Slope:=temp+Xtra1[i]*TMultiplier(1.0*N_Rep);
          IncludeDCUncertainty := false;
          AgePlusAgeMinus := 'neither';
          UprUprAgeError:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
          IncludeDCUncertainty := true;
          AgePlusAgeMinus := UncertaintyPlus;
          UprUprAgeErrorIncl:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
          Slope:=temp-Xtra1[i]*TMultiplier(1.0*N_Rep);
          IncludeDCUncertainty := false;
          AgePlusAgeMinus := 'neither';
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
          IncludeDCUncertainty := true;
          AgePlusAgeMinus := UncertaintyMinus;
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          if (Age > MaxAge) then MaxAge := Age;
          Xtra[i] := Age;
          Xtra1[i] := UprUprAgeError;
          Xtra2[i] := UprLwrAgeError;
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
        end;
        if (AnalType in ['8','A']) then
        begin
          if (RFlg[i] = 'Y') then
          begin
            iinc := iinc + 1;
            Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(i,Xtra[i]);
        end;
      end;
      if ((not AllSame) and (VarbNoX in [4])) then
      begin
        if (AnalType in ['1','2','3','4','5','6','7','9','B','E','F','G']) then
        //if (AnalType in [atRbSr,atSmNd,atPbPb,at238UPb,at235UPb,atThPb,atLuHf,atLaCe,atKAr,atKCa,atReOs,atLaBa]) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (MaxX < Ratio[i,3]) then MaxX := Ratio[i,3];
          if (MinX > Ratio[i,3]) then MinX := Ratio[i,3];
          if (RFlg[i] = 'Y') then
          begin
            //ChWtAv.Series[iErrorIncluded].AddXY(Age,Ratio[i,3]);
            Series11.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
            iinc := iinc + 1;
          end;
          if (RFlg[i] = 'N') then
          begin
            //ChWtAv.Series[iErrorExcluded].AddXY(Age,Ratio[i,3]);
            Series12.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
            jinc := jinc + 1;
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(Ratio[i,3],Xtra[i]);
        end;
      end;
      if ((not AllSame) and (VarbNoX in [5])) then
      begin
        if (AnalType in ['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (MaxX < Ratio[i,3]) then MaxX := Ratio[i,3];
          if (MinX > Ratio[i,3]) then MinX := Ratio[i,3];
          if (RFlg[i] = 'Y') then
          begin
            //ChWtAv.Series[iErrorIncluded].AddXY(Age,Ratio[i,3]);
            Series11.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
            iinc := iinc + 1;
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            //ChWtAv.Series[iErrorExcluded].AddXY(Age,Ratio[i,3]);
            Series12.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(Ratio[i,3],Xtra[i]);
        end;
      end;
      if ((not AllSame) and (VarbNoX in [6])) then
      begin
        if (AnalType in ['1','2','4','5','6','7','9','B','E','F','G']) then
        begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (MaxX < Ratio[i,3]) then MaxX := Ratio[i,3];
          if (MinX > Ratio[i,3]) then MinX := Ratio[i,3];
          if (RFlg[i] = 'Y') then
          begin
            //ChWtAv.Series[iErrorIncluded].AddXY(InitRatio,Ratio[i,3]);
            Series11.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
            iinc := iinc + 1;
          end;
          if (RFlg[i] = 'N') then
          begin
            jinc := jinc + 1;
            //ChWtAv.Series[iErrorExcluded].AddXY(InitRatio,Ratio[i,3]);
            Series12.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(Ratio[i,3],Xtra[i]);
        end;
      end;
      if ((Icnt > 0) and (not AllSame) and (VarbNoX in [10])) then
      begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (MaxX < Ratio[i,3]) then MaxX := Ratio[i,3];
          if (MinX > Ratio[i,3]) then MinX := Ratio[i,3];
        if (RFlg[i] = 'Y') then
        begin
          iinc := iinc + 1;
          Series11.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
        end;
        if (RFlg[i] = 'N') then
        begin
          jinc := jinc + 1;
          Series12.Add(Ratio[i,3],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(Ratio[i,3],Xtra[i]);
        end;
        ChWtAv.Series[iRegressionLine].AddXY(0.95*MinX,temp);
        ChWtAv.Series[iRegressionLine].AddXY(1.05*MaxX,temp);
        ChWtAv.Series[iEnvelopeLower].AddXY(0.95*MinX,temp-SDMult);
        ChWtAv.Series[iEnvelopeLower].AddXY(1.05*MaxX,temp-SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(0.95*MinX,temp+SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(1.05*MaxX,temp+SDMult);
      end;
      if ((Icnt > 0) and (not AllSame) and (VarbNoX in [13])) then
      begin
          if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
          if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
          if (MaxX < Ratio[i,1]) then MaxX := Ratio[i,1];
          if (MinX > Ratio[i,1]) then MinX := Ratio[i,1];
        if (RFlg[i] = 'Y') then
        begin
          iinc := iinc + 1;
          Series11.Add(Ratio[i,1],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
        end;
        if (RFlg[i] = 'N') then
        begin
          jinc := jinc + 1;
          Series12.Add(Ratio[i,1],Xtra[i],0.0,0.0,Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(Ratio[i,1],Xtra[i]);
        end;
        ChWtAv.Series[iRegressionLine].AddXY(0.95*MinX,temp);
        ChWtAv.Series[iRegressionLine].AddXY(1.05*MaxX,temp);
        ChWtAv.Series[iEnvelopeLower].AddXY(0.95*MinX,temp-SDMult);
        ChWtAv.Series[iEnvelopeLower].AddXY(1.05*MaxX,temp-SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(0.95*MinX,temp+SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(1.05*MaxX,temp+SDMult);
      end;
      if ((AllSame) and (VarbNoX in [11])) then
      begin
        if (AnalType in ['3']) then
        begin
            if (MaxY < (Xtra2[i])) then MaxY := Xtra2[i];
            if (MinY > (Xtra2[i])) then MinY := Xtra2[i];
            if (MaxX < (Xtra1[i])) then MaxX := Xtra1[i];
            if (MinX > (Xtra1[i])) then MinX := Xtra1[i];
          if (RFlg[i] = 'Y') then
          begin
            //ChWtAv.Series[iErrorIncluded].AddXY(Age,Ratio[i,3]);
            Series11.Add(Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual,0.0,0.0,0.0,0.0);
            iinc := iinc + 1;
          end;
          if (RFlg[i] = 'N') then
          begin
            //ChWtAv.Series[iErrorExcluded].AddXY(Age,Ratio[i,3]);
            Series12.Add(Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual,0.0,0.0,0.0,0.0);
            jinc := jinc + 1;
          end;
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(Xtra1[i]*T_Mult_Visual,Xtra2[i]*T_Mult_Visual);
        end;
      end;
      {
      if ((VarbNox in [6]) and (AnalType in ['8'])) then
      begin
        temp := Xtra[i];
        Age:=PbPbAge(temp);
        Slope:=temp+Xtra1[i]*TMultiplier(1.0*N_Rep);
        UprUprAgeError:=PbPbAge(Slope)-Age;
        Slope:=temp-Xtra1[i]*TMultiplier(1.0*N_Rep);
        if (Slope<0.0) then UprLwrAgeError:=Age
                       else UprLwrAgeError:=Age-PbPbAge(Slope);
        Age:=Age/1.0e6;
        UprUprAgeError:=UprUprAgeError/1.0e6;
        UprLwrAgeError:=UprLwrAgeError/1.0e6;
        if (Age > MaxAge) then MaxAge := Age;
        Xtra[i] := Age;
        Xtra1[i] := UprUprAgeError;
        Xtra2[i] := UprLwrAgeError;
        if (MaxX < Ratio[i,1]) then MaxX := Ratio[i,1];
        if (MinX > Ratio[i,1]) then MinX := Ratio[i,1];
        if (MaxY < (Xtra[i]+T_Mult_Visual*Xtra1[i])) then MaxY := Xtra[i]+T_Mult_Visual*Xtra1[i];
        if (MinY > (Xtra[i]-T_Mult_Visual*Xtra2[i])) then MinY := Xtra[i]-T_Mult_Visual*Xtra2[i];
        if (RFlg[i] = 'Y') then
        begin
          Series11.Add(i,Xtra[i],0.0,0.0,Xtra1[i],Xtra2[i]);
        end;
        if (RFlg[i] = 'N') then
        begin
          Series12.Add(i,Xtra[i],0.0,0.0,Xtra1[i],Xtra2[i]);
        end;
        if (i = 1) then
        begin
          ChWtAv.Series[iCurrent].AddXY(i,Xtra[i]);
        end;
      end;
      }
    end; //if PFlg[i] = 'Y'
  end; //for i = 1 to NumberOfPoints
  //ShowMessage('MinX = '+FormatFloat('####0.000',MinX)+'   MaxX = '+FormatFloat('####0.000',MaxX));
  if (MaxX <= MinX) then MaxX := MinX + 0.005*MinX;
  if (MaxY <= MinY) then MaxY := MinY + 0.005*MinY;
  MaxX := MaxX + 0.01*(MaxX-MinX);
  MinX := MinX - 0.01*(MaxX-MinX);
  MaxY := MaxY + 0.01*(MaxY-MinY);
  MinY := MinY - 0.01*(MaxY-MinY);
  //ShowMessage('MinX = '+FormatFloat('####0.000',MinX)+'   MaxX = '+FormatFloat('####0.000',MaxX));
  if ((not AllSame) and (VarbNoX in [4])) then
  begin
    if ((Icnt > 0)and (AnalType in ['1','2','3','4','5','6','7','9','B','E','F','G'])) then
    begin
      tmpMin := MinX;
      tmpMax := MaxX;
      tmpX := tmpMin;
      ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
      ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
      ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
      ChWtAv.Series[iCurveLine].AddXY(tmpX,DMRatioAtAge(tmpX));
      repeat
        ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
        ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
        ChWtAv.Series[iCurveLine].AddXY(tmpX,DMRatioAtAge(tmpX));
        tmpX := tmpX + 0.1*(tmpMax-tmpMin);
      until (tmpX > tmpMax);
      tmpX := tmpMax;
      ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
      ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
      ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
      ChWtAv.Series[iCurveLine].AddXY(tmpX,DMRatioAtAge(tmpX));
    end;
  end;
  if ((not AllSame) and (VarbNoX in [5])) then
  begin
    if ((Icnt > 0)and (AnalType in ['1','2','4','5','6','7','9','B','E','F','G'])) then
    begin
      tmpMin := MinX;
      tmpMax := MaxX;
      tmpX := tmpMin;
      ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
      ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
      ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
      ChWtAv.Series[iCurveLine].AddXY(tmpX,DMEpsilonAtAge(tmpX));
      repeat
        ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
        ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
        ChWtAv.Series[iCurveLine].AddXY(tmpX,DMEpsilonAtAge(tmpX));
        tmpX := tmpX + 0.1*(tmpMax-tmpMin);
      until (tmpX > tmpMax);
      tmpX := tmpMax;
      ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
      ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
      ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
      ChWtAv.Series[iCurveLine].AddXY(tmpX,DMEpsilonAtAge(tmpX));
    end;
  end;
  if ((not AllSame) and (VarbNoX in [6])) then
  begin
    if ((Icnt > 0)and (AnalType in ['1','2','4','5','6','7','9','B','E','F','G'])) then
    begin
      tmpMin := MinX;
      tmpMax := MaxX;
      tmpX := tmpMin;
      ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
      ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
      ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
      repeat
        ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
        ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
        ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
        tmpX := tmpX + 0.1*(tmpMax-tmpMin);
      until (tmpX > tmpMax);
      tmpX := tmpMax;
      ChWtAv.Series[iRegressionLine].AddXY(tmpX,temp);
      ChWtAv.Series[iEnvelopeLower].AddXY(tmpX,temp-SDMult);
      ChWtAv.Series[iEnvelopeUpper].AddXY(tmpX,temp+SDMult);
    end;
  end;
  //Concordia curve
  {
  if (AnalType in ['8']) then
  begin
    //ChWtAv.Series[iCurveLine].Clear;
    tmpX := exp(DecayConst[ord(at235UPb)]*MaxAge*1.0e6)-1.0;
    if (MaxX < tmpX) then MaxX := tmpX;
    tmpY := exp(DecayConst[ord(at238UPb)]*MaxAge*1.0e6)-1.0;
    if (MaxY < tmpY) then MaxY := tmpY;
    tmpAge := 0.0;
    j := 1;
    repeat
      ChWtAv.Series[iCurveLine].AddXY(tmpX,tmpY);
      tmpAge := tmpAge + 20.0;
      j := j+1;
    until (tmpAge > (MaxAge+50.0));
  end;
  }
  ChWtAv.BottomAxis.SetMinMax(MinX,MaxX);
  ChWtAv.LeftAxis.SetMinMax(MinY,MaxY);
  //ChWtAv.BottomAxis.Automatic := true;
  if ((not AllSame) and (VarbNoX in [10,13]))
    then ChWtAv.BottomAxis.Automatic := false
    else ChWtAv.BottomAxis.Automatic := true;
  IncrementAmount := ChWtAv.Axes.Left.Increment;
  //ShowMessage('Increment = '+FormatFloat('####0.000000',IncrementAmount));
  if (IncrementAmount <= 0.0) then IncrementAmount := 0.5;
  Range := MaxY-MinY;
  NumLabelledTics := Round(Range/IncrementAmount);
  //ShowMessage('Range = '+FormatFloat('###0.0000000',Range)+'__'+Int2Str(NumLabelledTics));
  //ShowMessage('Minimum = '+FormatFloat('###0.0000000',MinY)+'__'+'Maximum = '+FormatFloat('###0.0000000',MaxY));
  //ShowMessage('IncrementAmount = '+FormatFloat('###0.0000000',IncrementAmount)+'__'+Int2Str(NumLabelledTics));
  if (NumLabelledTics > 10) then
  begin
    IncrementAmount := 1.0*Round(Range/10.0);
    //ShowMessage('Set IncrementAmount = '+FormatFloat('###0.0000000',IncrementAmount)+'__'+Int2Str(NumLabelledTics));
    ChWtAv.LeftAxis.Automatic := false;
    ChWtAv.LeftAxis.Increment := IncrementAmount;
    ChWtAv.LeftAxis.SetMinMax(MinY,MaxY);
  end;
  //ShowMessage('Range = '+FormatFloat('####0.000000',Range));
  ChWtAv.Axes.Left.AxisValuesFormat := '####0.0##';
  if (Range <= 0.7) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.000';
  if (Range <= 0.07) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.0000';
  if (Range <= 0.007) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.00000';
  if (Range <= 0.0007) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.000000';
  if (Range <= 0.00007) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.000000';
  Range := MaxX-MinX;
  ChWtAv.Axes.Bottom.AxisValuesFormat := '####0.0##';
  if (Range <= 0.7) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.00';
  if (Range <= 0.07) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.000';
  if (Range <= 0.007) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.0000';
  if (Range <= 0.0007) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.00000';
  if (Range <= 0.00007) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.000000';

  FillRegTable(Sender);
  iRec := 1;
  ChWtAv.Foot.Caption := GeodateVersionStr;
  if (EllipseMagnif = 1.0) then ChWtAv.SubFoot.Caption := '1 sigma analytical uncertainties';
  if (EllipseMagnif > 1.0) then ChWtAv.SubFoot.Caption := '95% conf. uncertainties excl. d.c. uncert.';
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
  bbCumHistClick(Sender);
end;

procedure TfmWtAv.bbOKClick(Sender: TObject);
begin
  Close;
end;


procedure TfmWtAv.Choose_Wt_Field;
var
  t1,t2        : double;
  LWt          : array[1..2] of double;
  i            : integer;
  t3, t4       : double;
  tFormationAge,
  MuRes, MuSmp, Pb64i, Pb74i, Pb64si, Pb74si : double;
begin
  HistOK:=false;
  //ShowMessage('# points = '+IntToStr(NumberOfPoints));
  for i:=1 to NumberOfPoints do
  begin
    case VarbNoX of
      1..2 : begin   //1 = XRatio,  2 = YRatio
        HistOK:=true;
        Xtra[i]:=Ratio[i,VarbNoX];
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
        Xtra1[i] := LWt[VarbNoX];
        Xtra2[i] := Xtra1[i];
      end;
      3 : begin //ZRatio
        HistOK:=true;
        if (IAnalTyp in [0..7,9..16]) then
        begin
          if (Ratio[i,VarbNoX] > 0.0) then Xtra[i]:=Ratio[i,VarbNoX]
          else begin
            Xtra[i]:=0.0;
            RFlg[i]:='N';
          end;
          if (ZPrec[i] > 0.00001) then Xtra1[i]:=ZPrec[i]
          else begin
            Xtra1[i]:=0.0;
            RFlg[i]:='N';
          end;
        end;
        if (IAnalTyp in [8]) then
        begin
          if (Ratio[i,VarbNoX] > 0.0)
             then Xtra[i]:=Ratio[i,VarbNoX]
             else begin
               if ((Ratio[i,2] > 0.000001)) then
                 Xtra[i] := Ratio[i,1]/Ratio[i,2]/U238U235
               else begin
                 Xtra[i]:=0.0;
                 RFlg[i]:='N';
               end;
             end;
          if (ZPrec[i] > 0.00001) then
          begin
            if ((Xtra[i]/100.0*(BlanketZErrVal/10.0)) <= ZPrec[i])
            then Xtra1[i]:=ZPrec[i]
            else begin
               Xtra1[i]:=Xtra[i]/100.0*(BlanketZErrVal/10.0);
            end;
          end else
          begin
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
              Xtra1[i] := Sqrt(LWt[1]*LWt[1]+LWt[2]*LWt[2]-2.0*R[i]*LWt[1]*LWt[2]);
              Xtra1[i] := Xtra1[i]* Xtra[i]/100.0;
            end
            else begin
              Xtra1[i]:=0.0;
              RFlg[i]:='N';
            end;
          end;
          Xtra2[i] := Xtra1[i];
        end;
        if (IAnalTyp in [17]) then   //evaporation Pb
        begin
          if (Ratio[i,VarbNoX] > 0.0) then Xtra[i]:=Ratio[i,VarbNoX];
          if (ZPrec[i] > 0.000001) then
          begin
            if ((Xtra[i]/100.0*BlanketZErrVal) <= ZPrec[i])
            then Xtra1[i]:=ZPrec[i]
            else begin
               Xtra1[i]:=Xtra[i]/100.0*BlanketZErrVal;
            end;
          end;
          Xtra2[i] := Xtra1[i];
        end;
      end;
      4 : begin //Ratio(Date) or mu(Date) or T(206Pb/238U)
        HistOK:=true;
        if ((IAnalTyp in [1,2,4,5,6,7,9,11,14,15,16]) and (AllSame = true)) then
        begin
          Xtra[i]:=InitialRatio(Age,i);
          //Xtra1[i]:=RoError(Age,0.0,i)/T_Mult;  // original version before allowing for 1 sigma uncertainty
          Xtra1[i]:=RoError(Age,AgeError,i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [1,2,4,5,6,7,9,11,14,15,16]) and (AllSame = false)) then
        begin
          Xtra[i]:=InitialRatio(Ratio[i,3],i);
          Xtra1[i]:=RoError(Ratio[i,3],ZPrec[i],i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [3]) and (AllSame = true)) then
        begin
          CalcMuErr(Age,i,t3,t4);
          Xtra[i]:=t3;
          Xtra1[i]:=t4/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [3]) and (AllSame = false)) then
        begin
          CalcMuErr(Ratio[i,3],i,t3,t4);
          Xtra[i]:=t3;
          Xtra1[i]:=t4/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [8]) and (AllSame = true)) then
        begin
          Xtra[i]:=Ratio[i,2];
          if ((Ratio[i,2] > 0.000001)) then
          begin
            case ErrTyp[i] of
              '1' : begin
                 LWt[2]:=ErrorWt[i,2]*Ratio[i,2]/100.0;
               end;
              '2' : begin
                 LWt[2]:=ErrorWt[i,2];
               end;
              '3' : begin
                 LWt[2]:=ErrorWt[i,2]*Ratio[i,1]/100.0;
               end;
              '4' : begin
                 LWt[2]:=ErrorWt[i,2];
               end;
            end;
            Xtra1[i] := LWt[2];
          end else
          begin
              Xtra1[i]:=0.0;
              RFlg[i]:='N';
          end;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [10]) and (AllSame = true)) then
        begin
          if ((Ratio[i,1] > 0.000001)) then
          begin
            Xtra[i] := 1.0/Ratio[i,1];
            case ErrTyp[i] of
              '1' : begin
                 LWt[1]:=ErrorWt[i,1]*(1.0/Ratio[i,1])/100.0;
               end;
              '2' : begin
                 LWt[1]:=ErrorWt[i,1]*(1.0/Ratio[i,1])/100.0;
               end;
              '3' : begin
                 LWt[1]:=ErrorWt[i,1]/Ratio[i,1]*100.0;
                 LWt[1]:=ErrorWt[i,1]*(1.0/Ratio[i,1])/100.0;
               end;
              '4' : begin
                 LWt[1]:=ErrorWt[i,1]/Ratio[i,1]*100.0;
                 LWt[1]:=ErrorWt[i,1]*(1.0/Ratio[i,1])/100.0;
               end;
            end;
            Xtra1[i] := LWt[1];
          end else
          begin
              Xtra1[i]:=0.0;
              RFlg[i]:='N';
          end;
          Xtra2[i] := Xtra1[i];
        end;
      end;
      5 : begin //Epsilon(Date)
        HistOK:=true;
        if ((IAnalTyp in [1,2,7,9,14,15,16]) and (AllSame = true)) then
        begin
          Xtra[i]:=EpsilonGamma(Age,i,IAnalTyp);
          //Xtra1[i]:=EpGammaError(Age,0.0,i,IAnalTyp)/T_Mult;  // original version before allowing for 1 sigma uncertainty
          Xtra1[i]:=EpGammaError(Age,AgeError,i,IAnalTyp)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [1,2,7,9,14,15,16]) and (AllSame = false)) then
        begin
          Xtra[i]:=EpsilonGamma(Ratio[i,3],i,IAnalTyp);
          Xtra1[i]:=EpGammaError(Ratio[i,3],ZPrec[i],i,IAnalTyp)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
      end;
      6 : begin //T(Ratio) or T(207Pb/206Pb)
        HistOK:=true;
        if ((IAnalTyp in [1,2,4,5,6,7,9,11,14,15,16]) and (AllSame = true)) then
        begin
          Xtra[i]:=ModelAge(InitRatio,i);
          //Xtra1[i]:=ModelAgeError(InitRatio,0.0,i)/T_Mult;  // original version before allowing for 1 sigma uncertainty
          Xtra1[i]:=ModelAgeError(InitRatio,InitRatioError,i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if ((IAnalTyp in [1,2,4,5,6,7,9,11,14,15,16]) and (AllSame = false)) then
        begin
          Xtra[i]:=ModelAge(Ratio[i,3],i);
          Xtra1[i]:=ModelAgeError(Ratio[i,3],ZPrec[i],i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
        if (IAnalTyp in [3]) then
        begin
          HistOK:=true;
          Xtra[i]:=CalcModelPbAge(Ratio[i,1],Ratio[i,2])/1.0e6;
          Xtra1[i]:=PbModel_Age_Error(i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;

        if (IAnalTyp in [8]) then
        begin
          HistOK:=true;
          AnalType8 := 'N';
          if (Ratio[i,3] > 0.000001) then
          begin
            Xtra[i]:=Ratio[i,3];
            //ShowMessage('Ratio 3 '+FormatFloat('##0.000000',Xtra[i]));
          end else
          begin
            if ((Ratio[i,2] > 0.000001)) then
            begin
              Xtra[i] := Ratio[i,1]/Ratio[i,2]/U238U235;
              Ratio[i,3] := Xtra[i];
              //ShowMessage('Ratio 1/2 '+FormatFloat('##0.000000',Xtra[i]));
            end else
            begin
              Xtra[i]:=0.0;
              RFlg[i]:='N';
            end;
          end;
          if (ZPrec[i] > 0.00001) then
          begin
            if ((Xtra[i]/100.0*BlanketZErrVal) <= ZPrec[i]) then
              Xtra1[i]:=ZPrec[i]
            else begin
              Xtra1[i]:=Xtra[i]/100.0*BlanketZErrVal;
              ZPrec[i] := Xtra1[i];
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
                 Xtra1[i] := Sqrt(LWt[1]*LWt[1]+LWt[2]*LWt[2]-2.0*R[i]*LWt[1]*LWt[2]);
                 ZPrec[i] := Xtra1[i];
                 //ShowMessage('Xtra1 1/2 a '+FormatFloat('##0.000000',Xtra1[i]));
                 Xtra1[i] := Xtra1[i]*Xtra[i]/100.0;
                 //ShowMessage('Xtra1 1/2 % '+FormatFloat('##0.000000',Xtra1[i]));
             end else
             begin
                 Xtra1[i]:=0.0;
                 RFlg[i]:='N';
             end;
             Xtra2[i] := Xtra1[i];
          end;
        end;

        if (IAnalTyp in [17]) then
        begin
          HistOK:=true;
          AnalType8 := 'N';
          if (Ratio[i,3] > 0.0) then Xtra[i]:=Ratio[i,3]
          else begin
              Xtra[i]:=0.0;
              RFlg[i]:='N';
          end;
          if (ZPrec[i] > 0.000001) then
          begin
             if ((Xtra[i]/100.0*BlanketZErrVal) <= ZPrec[i])
             then Xtra1[i]:=ZPrec[i]
             else begin
                  Xtra1[i]:=BlanketZErrVal/Xtra[i]*100.0;
             end;
          end
          else begin
                 Xtra1[i]:=0.0;
                 RFlg[i]:='N';
          end;
          Xtra2[i] := Xtra1[i];
        end;
        if (IAnalTyp in [10]) then
        begin
          HistOK:=true;
          AnalType8 := 'N';
          if (Ratio[i,2] > 0.0)
          then
            Xtra[i]:=Ratio[i,2]
          else begin
            Xtra[i]:=0.0;
            RFlg[i]:='N';
          end;
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
            Xtra1[i] := LWt[2]*Ratio[i,2]/100.0;
            if (Xtra1[i] <= 0.0) then RFlg[i] := 'N';
          end
          else begin
            Xtra1[i]:=0.0;
            RFlg[i]:='N';
          end;
          Xtra2[i] := Xtra1[i];
        end;
      end;
      7 : begin //T(CHUR)
        HistOK:=true;
        if (IAnalTyp in [1,2,7,9,14,15,16]) then
        begin
          Xtra[i]:=CHUR_Age(Ratio[i,1],Ratio[i,2]);
          Xtra1[i]:=CHUR_Age_Error(i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
      end;
      8 : begin //T(DM)
        HistOK:=true;
        if (IAnalTyp in [1,2,7,9,14,15,16]) then
        begin
          Xtra[i] := DM_Age(Ratio[i,1],Ratio[i,2]);
          Xtra1[i] := DM_Age_Error(i)/T_Mult;
          Xtra2[i] := Xtra1[i];
        end;
      end;
      9 : begin //T(concordia)
        HistOK:=true;
        if (IAnalTyp in [8]) then
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
          end else
          begin
              Xtra1[i] := 0.0;
              Xtra2[i] := 0.0;
              Xtra3[i] := 0.0;
              RFlg[i] := 'N';
          end;
        end;
      end;
      10 : begin //T(RD) and T(2DM)
        //if ((DM[iAnalTyp,1] = 0.0) and (CC[iAnalTyp,1] = 0.0)) then
        //begin
          HistOK:=true;
          if (IAnalTyp in [15]) then
          begin
            Xtra[i] := RD_Age(Ratio[i,1],Ratio[i,2]);
            Xtra1[i] := RD_Age_Error(i)/T_Mult;
            Xtra2[i] := Xtra1[i];
          end;
          if ((IAnalTyp in [1,2,7,9,14,16]) and (AllSame = true)) then
          begin
            Xtra[i] := DM2_Age(Age,Ratio[i,1],Ratio[i,2]);
            Xtra1[i] := DM_Age_Error(i)/T_Mult;
            Xtra2[i] := Xtra1[i];
          end;
          if ((IAnalTyp in [1,2,7,9,14,16]) and (AllSame = false)) then
          begin
            Xtra[i] := DM2_Age(Ratio[i,3],Ratio[i,1],Ratio[i,2]);
            Xtra1[i] := DM_Age_Error(i)/T_Mult;
            Xtra2[i] := Xtra1[i];
          end;
        //end else
        //begin
        //  if (i=1) then MessageDlg('Linear models required. Modify the GeoDate.INI file',mtWarning,[mbOK],0);
        //end;
      end;
      11,12 : begin //11 = Post-formation uranogenic Pb isotope composition
                    //12 = Post-formation model source 238U/204Pb
        HistOK:=false;
        if (FormationAge > 0.0) then
        begin
          tFormationAge := FormationAge;
        end else
        begin
          tFormationAge := Ratio[i,3];
        end;
        if (i=1) then
        begin
          //ShowMessage('tFormationAge = '+FormatFloat('###0.00',tFormationAge));
          //ShowMessage('RequiredAge = '+FormatFloat('###0.00',RequiredAge));
        end;
        if ((tFormationAge > 0.0) and (RequiredAge >= 0.0) and (IAnalTyp in [3])) then
        begin
          Slope := (exp5t(tFormationAge*1.0e6)-1.0)/(U238U235*(exp8t(tFormationAge*1.0e6)-1.0));
          MuRes := (Slope*(MuV[mu_choice,2]-Ratio[i,1])+Ratio[i,2]-MuV[mu_choice,3]);
          MuRes := MuRes/((exp5t(MuV[mu_choice,1])-exp5t(tFormationAge*1.0e6))/U238U235-Slope*(exp8t(MuV[mu_choice,1])-exp8t(tFormationAge*1.0e6)));
          Pb64i := MuV[mu_choice,2]+MuRes*(exp8t(MuV[mu_choice,1])-exp8t(tFormationAge*1.0e6));
          Pb74i := MuV[mu_choice,3]+(MuRes/U238U235)*(exp5t(MuV[mu_choice,1])-exp5t(tFormationAge*1.0e6));
          MuSmp := (Ratio[i,1]-Pb64i)/(exp8t(MuV[mu_choice,1])-exp8t(tFormationAge*1.0e6));
          Pb64si := Pb64i+MuSmp*(exp8t(tFormationAge*1.0e6)-exp8t(RequiredAge*1.0e6));
          Pb74si := Pb74i+(MuSmp/U238U235)*(exp5t(tFormationAge*1.0e6)-exp5t(RequiredAge*1.0e6));
          Xtra[i] := MuSmp;
          Xtra1[i] := Pb64si;
          Xtra2[i] := Pb74si;
        end;
      end;
      13 : begin   //1 = Age (Ma),  2 = Epsilon
        HistOK:=true;
        if ((IAnalTyp in [2,7]) and (AllSame = false)) then
        begin
          LWt[2] := ErrorWt[i,2];
          Xtra[i] := DM2_Age_From_Epsilon(Ratio[i,1],Ratio[i,2]);
          t1 := DM2_Age_From_Epsilon(Ratio[i,1],Ratio[i,2]-LWt[2]);
          t2 := DM2_Age_From_Epsilon(Ratio[i,1],Ratio[i,2]+LWt[2]);
          Xtra1[i] := 0.5*(Abs(Xtra[i]-t1)+Abs(t2-Xtra[i]));
          Xtra2[i] := Xtra1[i];
        end;
      end;
    end;
  end;
end;

procedure TfmWtAv.ChWtAvClick(Sender: TObject);
var
  i, itmp : integer;
  x, y : double;
  tx, ty : double;
  tmpStr : string;
  Node : TTreeNode;
begin
  //Series1.GetCursorValues(x,y);
  for i := 0 to ChWtAv.SeriesCount-1 do
  begin
    if (i in [7,8,9]) then
    begin
      itmp := ChWtAv.Series[i].GetCursorValueIndex;
      if (itmp <> -1) then
      begin
        //ShowMessage('Clicked series: '+ChartReg.Series[i].Name+' at point: '+IntToStr(itmp));
        dmGDWtmp.cdsReg.Locate('i',itmp+1,[]);
        tmpStr := dmGDWtmp.cdsRegSample_no.AsString;
        ShowCurrentPoint;
        //tx := dmGdwtmp.cdsRegXRatio.AsFloat;
        //ty := dmGdwtmp.cdsRegYRatio.AsFloat;
        //ChWtAv.Series[iCurrent].Clear;
        //ChWtAv.Series[iCurrent].AddXY(tx,ty);
        //ChWtAv.Enabled := true;
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

procedure TfmWtAv.ChWtAvDblClick(Sender: TObject);
begin
  ChWtAvClick(Sender);
  dmGDWtmp.cdsReg.Edit;
  if (dmGDWtmp.cdsRegRFlag.AsString = 'Y') then dmGDWtmp.cdsRegRFlag.AsString := 'N'
  else dmGDWtmp.cdsRegRFlag.AsString := 'Y';
  dmGDWtmp.cdsReg.Post;
end;

procedure TfmWtAv.ChWtAvZoom(Sender: TObject);
var
  IncrementAmountLeft, IncrementAmountBottom : double;
  NumLabelledTicsLeft, NumLabelledTicsBottom : integer;
  RangeLeft, RangeBottom : double;
  MinX, MaxX,
  MinY, MaxY : double;
begin
  MinY := ChWtAv.Axes.Left.Minimum;
  MaxY := ChWtAv.Axes.Left.Maximum;
  MinX := ChWtAv.Axes.Bottom.Minimum;
  MaxX := ChWtAv.Axes.Bottom.Maximum;
  IncrementAmountLeft := ChWtAv.Axes.Left.Increment;
  IncrementAmountBottom := ChWtAv.Axes.Bottom.Increment;
  //ShowMessage('IncrementLeft = '+FormatFloat('####0.000000',IncrementAmountLeft));
  if (IncrementAmountLeft <= 0.0) then IncrementAmountLeft := 0.5;
  if (IncrementAmountBottom <= 0.0) then IncrementAmountBottom := 0.5;
  RangeLeft := MaxY-MinY;
  NumLabelledTicsLeft := Round(RangeLeft/IncrementAmountLeft);
  NumLabelledTicsBottom := Round(RangeBottom/IncrementAmountBottom);
  //ShowMessage('RangeLeft = '+FormatFloat('###0.0000000',RangeLeft)+'__'+Int2Str(NumLabelledTicsLeft));
  //ShowMessage('Minimum = '+FormatFloat('###0.0000000',MinY)+'__'+'Maximum = '+FormatFloat('###0.0000000',MaxY));
  //ShowMessage('IncrementAmountLeft = '+FormatFloat('###0.0000000',IncrementAmountLeft)+'__'+Int2Str(NumLabelledTicsLeft));
  if (NumLabelledTicsLeft > 10) then
  begin
    IncrementAmountLeft := 1.0*Round(RangeLeft/10.0);
    //ShowMessage('Set IncrementAmountLeft = '+FormatFloat('###0.0000000',IncrementAmountLeft)+'__'+Int2Str(NumLabelledTicsLeft));
    ChWtAv.LeftAxis.Automatic := false;
    ChWtAv.LeftAxis.Increment := IncrementAmountLeft;
    ChWtAv.LeftAxis.SetMinMax(MinY,MaxY);
  end;
  if (NumLabelledTicsBottom > 10) then
  begin
    IncrementAmountBottom := 1.0*Round(RangeBottom/10.0);
    //ShowMessage('Set IncrementAmountBottom = '+FormatFloat('###0.0000000',IncrementAmountBottom)+'__'+Int2Str(NumLabelledTicsBottom));
    ChWtAv.BottomAxis.Automatic := false;
    ChWtAv.BottomAxis.Increment := IncrementAmountBottom;
    ChWtAv.BottomAxis.SetMinMax(MinX,MaxX);
  end;
  RangeLeft := MaxY-MinY;
  //ShowMessage('RangeLeft = '+FormatFloat('####0.000000',RangeLeft));
  ChWtAv.Axes.Left.AxisValuesFormat := '####0.0##';
  if (RangeLeft <= 0.7) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.000';
  if (RangeLeft <= 0.07) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.0000';
  if (RangeLeft <= 0.007) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.00000';
  if (RangeLeft <= 0.0007) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.000000';
  if (RangeLeft <= 0.00007) then ChWtAv.Axes.Left.AxisValuesFormat := '###0.000000';
  RangeBottom := MaxX-MinX;
  ChWtAv.Axes.Bottom.AxisValuesFormat := '####0.0##';
  if (RangeBottom <= 0.7) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.00';
  if (RangeBottom <= 0.07) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.000';
  if (RangeBottom <= 0.007) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.0000';
  if (RangeBottom <= 0.0007) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.00000';
  if (RangeBottom <= 0.00007) then ChWtAv.Axes.Bottom.AxisValuesFormat := '###0.000000';
end;

procedure TfmWtAv.dbcbIncludeClick(Sender: TObject);
begin
  if (RFlg[i] = 'Y') then RFlg[i] := 'N'
                     else RFlg[i] := 'Y';
end;

procedure TfmWtAv.dbnSamplesClick(Sender: TObject; Button: TNavigateBtn);
var
  ii : integer;
  tmpX , tmpY : double;
begin
  {
  ChWtAv.Series[iCurrent].Clear;
  ii := dmGDWtmp.cdsRegi.AsInteger;
  case VarbNoX of
    1 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    2 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    3 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    4 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    5 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    6 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    7 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    8 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    9 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    10 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    11 : begin
      if ((AllSame)) then
      begin
        tmpX := Xtra1[ii];
        tmpY := Xtra2[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Xtra1[ii];
        tmpY := Xtra2[ii];
      end;
    end;
    12 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    13 : begin
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,1];
        tmpY := Xtra[ii];
      end;
    end;
  end;
  ChWtAv.Series[iCurrent].AddXY(tmpX,tmpY);
  }
  ShowCurrentPoint;
end;

procedure TfmWtAv.ShowCurrentPoint;
var
  ii : integer;
  tmpX , tmpY : double;
begin
  ChWtAv.Series[iCurrent].Clear;
  ii := dmGDWtmp.cdsRegi.AsInteger;
  case VarbNoX of
    1 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    2 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    3 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    4 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    5 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    6 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    7 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    8 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    9 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    10 : begin
      if ((AllSame)) then
      begin
        tmpX := 1.0*ii;
        tmpY := Xtra[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,3];
        tmpY := Xtra[ii];
      end;
    end;
    11 : begin
      if ((AllSame)) then
      begin
        tmpX := Xtra1[ii];
        tmpY := Xtra2[ii];
      end;
      if ((not AllSame)) then
      begin
        tmpX := Xtra1[ii];
        tmpY := Xtra2[ii];
      end;
    end;
    12 : begin
      tmpX := 1.0*ii;
      tmpY := Xtra[ii];
    end;
    13 : begin
      if ((not AllSame)) then
      begin
        tmpX := Ratio[ii,1];
        tmpY := Xtra[ii];
      end;
    end;
  end;
  ChWtAv.Series[iCurrent].AddXY(tmpX,tmpY);
end;

procedure TfmWtAv.TreeView1Click(Sender: TObject);
var
  tmpStr : string;
  i : integer;
  tx, ty : double;
begin
  i := TreeView1.Selected.AbsoluteIndex;
  tmpStr := TreeView1.Items.Item[i].Text;
  dmGdwtmp.cdsReg.Locate('Sample_No',tmpStr,[]);
  ShowCurrentPoint;
  {
  tx := i + 1;
  ty := Xtra[i+1];
  //ty := dmGdwtmp.cdsRegYRatio.AsFloat;
  ChWtAv.Series[iCurrent].Clear;
  ChWtAv.Series[iCurrent].AddXY(tx,ty);
  ChWtAv.Enabled := true;
  }
end;

procedure TfmWtAv.bbRecalculateClick(Sender: TObject);
var
  iCode : integer;
begin
  //Val(eTicksEvery.Text,TicksEvery,iCode);
  //if (iCode <>0) then TicksEvery := 10.0;
  //eTicksEvery.Text := FormatFloat('###0.0',TicksEvery);
  UpDateRFlg(Sender);
  FormShow(Sender);
end;

procedure TfmWtAv.bbSaveSheetClick(Sender: TObject);
begin
  {
  SaveDialogModels.InitialDir := TTPath;
  SaveDialogModels.FileName := ProjectName + ' ' + FileVarStr;
  if SaveDialogModels.Execute then
  begin
    Drive3 := ExtractFileDir(SaveDialogModels.FileName);
    TTPath := ExtractFilePath(SaveDialogModels.FileName);
  end;
  }
end;


procedure TfmWtAv.bbSpreadSheetClick(Sender: TObject);
var
  i, iRow, iCol    : integer;
  StepSize, t1, t2, temp, tOldAge : double;
  tmpStr   : string;
  StepIncrement : integer;
  MinX,MaxX,MinY,MaxY : double;
  zero : double;
  tPb64initial, tPb74initial : double;
begin
  zero := 0.0;
  try
    MinX := ChWtAv.BottomAxis.Minimum;
    MaxX := ChWtAv.BottomAxis.Maximum;
    MinY := ChWtAv.LeftAxis.Minimum;
    MaxY := ChWtAv.LeftAxis.Maximum;
      SaveDialogSprdSheet.InitialDir := TTPath;
      SaveDialogSprdSheet.FileName := ProjectName+'_WtAver';
      if SaveDialogSprdSheet.Execute then
      begin
        Drive3 := ExtractFileDir(SaveDialogSprdSheet.FileName);
        TTPath := ExtractFilePath(SaveDialogSprdSheet.FileName);
        try
          SprdSheet := TXlsFile.Create(true);
          SprdSheet.NewFile(1);
          iRow := 1;
          iCol := 1;
          tmpStr := 'Sample';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 2;
          tmpStr := Element[IAnalTyp,1];
          if (VarbNox = 13) then tmpStr := Element[19,1];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 3;
          tmpStr := Element[IAnalTyp,2];
          if (VarbNox = 13) then tmpStr := Element[19,2];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 4;
          tmpStr := XRatioStr[IAnalTyp];
          if (VarbNox = 13) then tmpStr := XRatioStr[19];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 5;
          tmpStr := 'Precision';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 6;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 8;
          tmpStr := YRatioStr[IAnalTyp];
          if (VarbNox = 13) then tmpStr := YRatioStr[19];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 9;
          tmpStr := 'Precision';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 10;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 12;
          tmpStr := 'R';
          if (VarbNox = 13) then tmpStr := ' ';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 13;
          tmpStr := ZRatioStr[IAnalTyp];
          if (VarbNox = 13) then tmpStr := ZRatioStr[19];
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
          tmpStr := 'X Residual';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 18;
          tmpStr := 'Y Residual';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 19;
          tmpStr := 'X wt aver line';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 20;
          tmpStr := 'Y wt aver line';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 21;
          tmpStr := 'X lwr err envelope';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 22;
          tmpStr := 'Y lwr err envelope';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 23;
          tmpStr := 'X upr err envelope';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 24;
          tmpStr := 'Y upr err envelope';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 25;
          tmpStr := 'X calc';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 26;
          tmpStr := 'X +95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 27;
          tmpStr := 'X -95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 28;
          tmpStr := 'Y calc';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 29;
          tmpStr := 'Y +95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 30;
          tmpStr := 'Y -95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          if ((VarbNox in [4]) and (AnalType in ['3'])) then
          begin
            iCol := 31;
            tmpStr := '206Pb/204Pb model initial';
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            iCol := 32;
            tmpStr := '207Pb/204Pb model initial';
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          end;
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
              if (ErrTyp[i] in ['1','3']) then
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
              SprdSheet.SetCellValue(iRow,iCol,Residual[i,1]);
              iCol := 18;
              //tmpStr := FloatToStr(Residual[i,2]);
              SprdSheet.SetCellValue(iRow,iCol,Residual[i,2]);
              iCol := 25;
              //tmpStr := FloatToStr(1.0*i);
              SprdSheet.SetCellValue(iRow,iCol,1.0*i);
              iCol := 26;
              //tmpStr := FloatToStr(0.0);
              SprdSheet.SetCellValue(iRow,iCol,zero);
              iCol := 27;
              //tmpStr := FloatToStr(0.0);
              SprdSheet.SetCellValue(iRow,iCol,zero);
              iCol := 28;
              //tmpStr := FloatToStr(Xtra[i]);
              SprdSheet.SetCellValue(iRow,iCol,Xtra[i]);
              iCol := 29;
              //tmpStr := FloatToStr(Xtra1[i]);
              SprdSheet.SetCellValue(iRow,iCol,Xtra1[i]);
              iCol := 30;
              //tmpStr := FloatToStr(Xtra2[i]);
              SprdSheet.SetCellValue(iRow,iCol,Xtra2[i]);
            end;
          end;
          iRow := iRow + 1;
          //Wt average line
          StepIncrement := 20; //originally 20
          StepSize := (MaxX-MinX)/(1.0*StepIncrement);
          temp:=ChWtAv.Series[iRegressionLine].YValue[1];
          j := 2;
          for i := 0 to StepIncrement do
          begin
            iCol := 19;
            iRow := j;
            //tmpStr := FloatToStr(StepSize*i);
            SprdSheet.SetCellValue(iRow,iCol,StepSize*i);
            iCol := 20;
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            j := j+1;
          end;
          //Lower error envelope
          StartAtX := MinX;
          EndAtX := MaxX;
          t1 := 5.0;
          TicsAtX := (EndAtX-StartAtX)/t1;
          i := 2;
          t1 := StartAtX;
          temp:=ChWtAv.Series[iEnvelopeLower].YValue[1];
          j := 2;
          for i := 0 to StepIncrement do
          begin
            iCol := 21;
            iRow := j;
            //tmpStr := FloatToStr(StepSize*i);
            SprdSheet.SetCellValue(iRow,iCol,StepSize*i);
            iCol := 22;
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            j := j+1;
          end;
          //Upper error envelope
          temp:=ChWtAv.Series[iEnvelopeUpper].YValue[1];
          j := 2;
          for i := 0 to StepIncrement do
          begin
            iCol := 23;
            iRow := j;
            //tmpStr := FloatToStr(StepSize*i);
            SprdSheet.SetCellValue(iRow,iCol,StepSize*i);
            iCol := 24;
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            j := j+1;
          end;
          //Data
          iRow := 1;
          iCol := 1;
          for i := 1 to NumberOfPoints do
          begin
            iCol := 25;
            iRow := i+1;
            //temp := ChWtAv.Series[iErrorIncluded].XValue[i];
            temp := 1.0*i;
            if ((VarbNox in [4]) and (AllSame) and (AnalType in ['3'])) then
            begin
              temp := Age;
            end;
            if ((VarbNox in [4]) and (not AllSame) and (AnalType in ['1','2','3','4','5','6','7','9','B','E','F','G'])) then
            begin
              temp := Ratio[i,3];
            end;
            if ((VarbNox in [5]) and (not AllSame) and (AnalType in ['1','2','4','5','6','7','9','B','E','F','G'])) then
            begin
              temp := Ratio[i,3];
            end;
            if ((VarbNox in [6]) and (not AllSame) and (AnalType in ['1','2','4','5','6','7','9','B','E','F','G'])) then
            begin
              temp := Ratio[i,3];
            end;
            if ((VarbNox in [10]) and (not AllSame) and (AnalType in ['1','2','4','5','6','7','9','B','E','F','G'])) then
            begin
              temp := Ratio[i,3];
            end;
            if ((VarbNox in [13]) and (not AllSame) and (AnalType in ['2','7'])) then
            begin
              temp := 0.0;
            end;
            if ((VarbNox in [11]) and (AllSame) and (AnalType in ['3'])) then
            begin
              temp := Xtra1[i];
            end;

            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            iCol := 26;
            //temp := ChWtAv.Series[iErrorIncluded].YValue[i];
            temp := 0.0;
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            iCol := 27;
            //temp := ChWtAv.Series[iErrorIncluded].YValue[i];
            temp := 0.0;
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);

            iCol := 28;
            //temp := ChWtAv.Series[iErrorIncluded].YValue[i];
            temp := Xtra[i];
            if ((VarbNox in [4]) and (AllSame) and (AnalType in ['3'])) then
            begin
              temp := Xtra[i];
            end;
            if ((VarbNox in [11]) and (AllSame) and (AnalType in ['3'])) then
            begin
              temp := Xtra2[i];
            end;
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            iCol := 29;
            //temp := ChWtAv.Series[iErrorIncluded].YValue[i];
            temp := Xtra1[i];
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            iCol := 30;
            //temp := ChWtAv.Series[iErrorIncluded].YValue[i];
            temp := Xtra2[i];
            //tmpStr := FloatToStr(temp);
            SprdSheet.SetCellValue(iRow,iCol,temp);
            if ((VarbNox in [4]) and (AnalType in ['3'])) then
            begin
              if AllSame then temp := Age;
              if not AllSame then temp := Ratio[i,3];
              tOldAge := 3700.0;
              t1 := 11.152;
              t2 := 12.998;
              ModelMuSourceInitialValue(Xtra[i],temp,tOldAge,t1,t2,tPb64initial,tPb74initial);
              iCol := 31;
              SprdSheet.SetCellValue(iRow,iCol,tPb64initial);
              iCol := 32;
              SprdSheet.SetCellValue(iRow,iCol,tPb74initial);
            end;
            j := j+1;
          end;
          SprdSheet.Save(SaveDialogSprdSheet.FileName);
        finally
          FreeAndNil(SprdSheet);
        end;
      end;
  finally
  end;
end;

procedure TfmWtAv.cbLegendClick(Sender: TObject);
begin
  if (cbLegend.Checked) then
  begin
    ChWtAv.Legend.Visible := true;
  end
  else begin
    ChWtAv.Legend.Visible := false;
  end;
end;

procedure TfmWtAv.bbCumHistClick(Sender: TObject);
var
  iCode : integer;
  i, j        : integer;
  X1, temp, temp1, temp2,
  tWtAv, tWtAvPlus95, tWtAvMinus95,
  x2, y2      : double;
  MinX, MinY, MaxX, MaxY : double;
  tUncert, tXtra1, tXtra2, tXtra3, tXtra4 : double;
  Spectrum2 : TCumArrayType;
  SpectrumWtAv : TCumArrayType;
  ShowCumGraph : boolean;
begin
  ChCum.Series[iEllipsesExcluded].Clear;
  ChCum.Series[iEllipsesIncluded].Clear;
  ChCum.Series[iEnvelopeLower].Clear;
  ChCum.Series[iEnvelopeUpper].Clear;
  ChCum.Series[iCurveTic].Clear;
  ChCum.Series[iCurveLine].Clear;
  ChCum.Series[iRegressionLine].Clear;
  ChCum.Series[iDataExcluded].Clear;
  ChCum.Series[iDataIncluded].Clear;
  ChCum.Series[iErrorExcluded].Clear;
  ChCum.Series[iErrorIncluded].Clear;
  ChCum.Series[iEllipseConcordia].Clear;
  ChCum.Series[iDataConcordia].Clear;
  ChCum.Series[iCurrent].Clear;
  ChCum.Series[iCurveLinePlus].Clear;
  ChCum.Title.Caption := Title;
  ChCum.Series[iCurveLinePlus].Visible := false;
  ChCum.Series[iCurveLinePlus].Legend.Visible := false;
  //CminX := 0.0;
  //CMaxX := 0.0;
  ShowCumGraph := true;
  if (Sender = bCumHist) then
  begin
    GetHistValuesForm := TfmGetHistValues.Create(Self);
    if (CMaxX = 0.0) then
    begin
      CMaxX := Xtra[1];
      CMinX := Xtra[1];
      for i := 1 to NumberOfPoints do
      begin
        if ((RFlg[i] = 'Y') and (PFlg[i]='Y')) then
        begin
          if (CMaxX < Xtra[i]) then CMaxX := Xtra[i];
          if (CMinX > Xtra[i]) then CMinX := Xtra[i];
        end;
      end;
      if (CMaxX <= CMinX) then CMaxX := CMinX + 0.1*CMinX;
      StartAtX := CMinX;
      EndAtX := CMaxX;
      if (StartAtX > ChWtAv.LeftAxis.Minimum) then StartAtX := ChWtAv.LeftAxis.Minimum;
      if (EndAtX < ChWtAv.LeftAxis.Maximum) then EndAtX := ChWtAv.LeftAxis.Maximum;
    end;
    GetHistValuesForm.eMinimum.Text := FormatFloat('###0.00000',StartAtX);
    GetHistValuesForm.eMaximum.Text := FormatFloat('###0.00000',EndAtX);
    GetHistValuesForm.NumInt := 10;
    GetHistValuesForm.eNumInt.Text := FormatFloat('###0',10);
    GetHistValuesForm.WidthInt := (EndAtX-StartAtX)/GetHistValuesForm.NumInt;
    if (GetHistValuesForm.WidthInt < 0.000001) then GetHistValuesForm.WidthInt := 0.000001;
    GetHistValuesForm.ShowModal;
    if (GetHistValuesForm.ModalResult = mrOK) then
    begin
      GetHistValuesForm.Close;
    end else
    begin
      ShowCumGraph := false;
    end;
  end else
  begin
    StartAtX := ChWtAv.LeftAxis.Minimum;
    EndAtX := ChWtAv.LeftAxis.Maximum;
    ChCum.BottomAxis.SetMinMax(StartAtX,EndAtX);
  end;
  //calculate cumulative spectrum without considering tracer and decay constant uncertainties
  tXtra1 := 0.0;
  tXtra2 := 0.0;
  tXtra3 := 0.0;
  tXtra4 := 0.0;
  FillChar(Spectrum,SizeOf(Spectrum),0);
  for i := 1 to NumberOfPoints do
  begin
    tUncert := 0.0;
    if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
    begin
      if (Xtra[i] <> 0.0) then
      begin
        tXtra1 := 100.0*Xtra1[i]/Xtra[i];
        tXtra2 := 100.0*Xtra2[i]/Xtra[i];
        tUncert := 100.0*((Xtra1[i]+Xtra2[i])/(2.0))/Xtra[i];
      end;
      tUncert := Sqrt(tUncert*tUncert + tXtra3*tXtra3 + tXtra4*tXtra4);
      tUncert := tUncert*Xtra[i]/100.0;
      //if (i < 6) then
      //begin
      //  ShowMessage('Excluding i = '+IntToStr(i)+'__'+FormatFloat('###0.000000',Xtra[i])+'__'+FormatFloat('###0.000000',tUncert));
      //end;
      for j := 1 to Steps do begin
        X1 := StartAtX + 1.0*j*(EndAtX-StartAtX)/Steps;
        Spectrum[j] := Spectrum[j] + Gauss(X1,Xtra[i],tUncert);
      end;
    end;
  end;
  //ShowMessage('AnalType = '+AnalType);
  case AnalType of
    '0' : begin
        tXtra3 := TracerUncertainty[ord(atGeneral)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atGeneral)]; // decay constant uncertainty already expressed as a percentage
    end;
    '1' : begin
        tXtra3 := TracerUncertainty[ord(atRbSr)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atRbSr)]; // decay constant uncertainty expressed as a percentage
    end;
    '2' : begin
        tXtra3 := TracerUncertainty[ord(atSmNd)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atSmNd)]; // decay constant uncertainty expressed as a percentage
    end;
    '3' : begin
        tXtra3 := TracerUncertainty[ord(at238UPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(at238UPb)]; // decay constant uncertainty expressed as a percentage
    end;
    '4' : begin
        tXtra3 := TracerUncertainty[ord(at238UPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(at238UPb)]; // decay constant uncertainty expressed as a percentage
    end;
    '5' : begin
        tXtra3 := TracerUncertainty[ord(at235UPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(at235UPb)]; // decay constant uncertainty expressed as a percentage
    end;
    '6' : begin
        tXtra3 := TracerUncertainty[ord(atThPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atThPb)]; // decay constant uncertainty expressed as a percentage
    end;
    '7' : begin
        tXtra3 := TracerUncertainty[ord(atLuHf)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atLuHf)]; // decay constant uncertainty expressed as a percentage
    end;
    '8' : begin
        tXtra3 := TracerUncertainty[ord(at238UPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(at238UPb)]; // decay constant uncertainty expressed as a percentage
    end;
    '9' : begin
        tXtra3 := TracerUncertainty[ord(atLaCe)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atLaCe)]; // decay constant uncertainty expressed as a percentage
    end;
    'A' : begin
        tXtra3 := TracerUncertainty[ord(at238UPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(at238UPb)]/DecayConst[ord(at238UPb)]; // decay constant uncertainty expressed as a percentage
    end;
    'B' : begin
        tXtra3 := TracerUncertainty[ord(atKAr)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atKAr)]; // decay constant uncertainty expressed as a percentage
    end;
    'C' : begin
        tXtra3 := TracerUncertainty[ord(atKAr)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atKAr)]; // decay constant uncertainty expressed as a percentage
    end;
    'D' : begin
        tXtra3 := TracerUncertainty[ord(atKAr)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atKAr)]; // decay constant uncertainty expressed as a percentage
    end;
    'E' : begin
        tXtra3 := TracerUncertainty[ord(atKCa)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atKCa)]; // decay constant uncertainty expressed as a percentage
    end;
    'F' : begin
        tXtra3 := TracerUncertainty[ord(atReOs)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atReOs)]; // decay constant uncertainty expressed as a percentage
    end;
    'G' : begin
        tXtra3 := TracerUncertainty[ord(atLaBa)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atLaBa)]; // decay constant uncertainty expressed as a percentage
    end;
    'H' : begin
        tXtra3 := TracerUncertainty[ord(at238UPb)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(at238UPb)]; // decay constant uncertainty expressed as a percentage
    end;
    'I' : begin
        tXtra3 := TracerUncertainty[ord(atKAr)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atKAr)]; // decay constant uncertainty expressed as a percentage
    end;
    'J' : begin
        tXtra3 := TracerUncertainty[ord(atEps2DM)];  // tracer uncertainty already expressed as a percentage
        tXtra4 := 0.0;
        tXtra4 := DecayConstUncertainty[ord(atEps2DM)]; // decay constant uncertainty expressed as a percentage
    end;
  end;
  //ShowMessage('DC val = '+FormatFloat('###0.0000000000',DecayConst[ord(at238UPb)]*1.0e6)+'__'+FormatFloat('###0.000000',DecayConstUncertainty[ord(at238UPb)]));
  //calculate cumulative spectrum with tracer and decay constant uncertainties
  FillChar(Spectrum2,SizeOf(Spectrum2),0);
  for i := 1 to NumberOfPoints do
  begin
    tUncert := 0.0;
    if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
    begin
      if (Xtra[i] <> 0.0) then
      begin
        tXtra1 := 100.0*Xtra1[i]/Xtra[i];
        tXtra2 := 100.0*Xtra2[i]/Xtra[i];
        tUncert := 100.0*((Xtra1[i]+Xtra2[i])/(2.0))/Xtra[i];
      end;
      tUncert := Sqrt(tUncert*tUncert);
      //tUncert := tUncert*Xtra[i]/100.0;
      //if (i < 6) then
      //begin
      //  ShowMessage('Including i = '+IntToStr(i)+'__'+FormatFloat('###0.000000',Xtra[i])+'__'+FormatFloat('###0.000000',tUncert));
      //end;
      tUncert := Sqrt(tUncert*tUncert + tXtra3*tXtra3 + tXtra4*tXtra4);
      tUncert := tUncert*Xtra[i]/100.0;
      //if (i < 6) then
      //begin
      //  ShowMessage('(3 and 4) i = '+IntToStr(i)+'__'+FormatFloat('###0.000000',tXtra3)+'__'+FormatFloat('###0.000000',tXtra4));
      //  ShowMessage('Including i = '+IntToStr(i)+'__'+FormatFloat('###0.000000',Xtra[i])+'__'+FormatFloat('###0.000000',tUncert));
      //end;
      for j := 1 to Steps do begin
        X1 := StartAtX + 1.0*j*(EndAtX-StartAtX)/Steps;
        Spectrum2[j] := Spectrum2[j] + Gauss(X1,Xtra[i],tUncert);
      end;
    end;
  end;
  temp1 := 0.0;
  temp2 := 0.0;
  for i := 1 to Steps do
  begin
    if (temp1 < Spectrum[i]) then temp1 := Spectrum[i];
    if (temp2 < Spectrum2[i]) then temp2 := Spectrum2[i];
  end;
  if (temp1 = 0.0) then temp1 := 1.0e-9;
  if (temp1 = 0.0) then temp2 := 1.0e-9;
  for i := 0 to Steps do
  begin
    x2 := StartAtX + 1.0*i*(EndAtX-StartAtX)/Steps;
    if (i > 0) then y2 := 100.0 * Spectrum[i]/temp1
               else y2 := 100.0 * Spectrum[1]/temp1;
    ChCum.Series[iCurveLine].AddXY(x2,y2);
    if (i > 0) then y2 := 100.0 * Spectrum2[i]/temp2
               else y2 := 100.0 * Spectrum2[1]/temp2;
    ChCum.Series[iCurveLinePlus].AddXY(x2,y2);
  end;
  iCode := 0;
  Val(eWtAv.Text,tWtAv,iCode);
  if (iCode = 0) then
  begin
    ChCum.Series[iRegressionLine].AddXY(tWtAv,0.0);
    ChCum.Series[iRegressionLine].AddXY(tWtAv,10.0);
    ChCum.Series[iRegressionLine].AddXY(tWtAv,20.0);
    ChCum.Series[iRegressionLine].AddXY(tWtAv,30.0);
    ChCum.Series[iRegressionLine].AddXY(tWtAv,50.0);
    ChCum.Series[iRegressionLine].AddXY(tWtAv,101.0);
  end;
  iCode := 0;
  Val(eWtAvPlus95.Text,tWtAvPlus95,iCode);
  tXtra1 := tWtAvPlus95;
  tWtAvPlus95 := tWtAv + tWtAvPlus95;
  if (iCode = 0) then
  begin
    ChCum.Series[iEnvelopeUpper].AddXY(tWtAvPlus95,0.0);
    ChCum.Series[iEnvelopeUpper].AddXY(tWtAvPlus95,10.0);
    ChCum.Series[iEnvelopeUpper].AddXY(tWtAvPlus95,20.0);
    ChCum.Series[iEnvelopeUpper].AddXY(tWtAvPlus95,30.0);
    ChCum.Series[iEnvelopeUpper].AddXY(tWtAvPlus95,50.0);
    ChCum.Series[iEnvelopeUpper].AddXY(tWtAvPlus95,100.0);
  end;
  iCode := 0;
  Val(eWtAvMinus95.Text,tWtAvMinus95,iCode);
  tXtra2 := tWtAvMinus95;
  tWtAvMinus95 := tWtAv - tWtAvMinus95;
  if (iCode = 0) then
  begin
    ChCum.Series[iEnvelopeLower].AddXY(tWtAvMinus95,0.0);
    ChCum.Series[iEnvelopeLower].AddXY(tWtAvMinus95,10.0);
    ChCum.Series[iEnvelopeLower].AddXY(tWtAvMinus95,20.0);
    ChCum.Series[iEnvelopeLower].AddXY(tWtAvMinus95,30.0);
    ChCum.Series[iEnvelopeLower].AddXY(tWtAvMinus95,50.0);
    ChCum.Series[iEnvelopeLower].AddXY(tWtAvMinus95,100.0);
  end;
  //calculate cumulative spectrum for weighted average
  FillChar(SpectrumWtAv,SizeOf(SpectrumWtAv),0);
  if (iCode = 0) then
  begin
    tUncert := (tXtra1 + tXtra2)/2.0/T_Mult;
    //ShowMessage('WtAv = '+FormatFloat('###0.000000',tWtAv)+'__'+FormatFloat('###0.000000',tUncert));
    for j := 1 to Steps do begin
      X1 := StartAtX + 1.0*j*(EndAtX-StartAtX)/Steps;
      SpectrumWtAv[j] := SpectrumWtAv[j] + Gauss(X1,tWtAv,tUncert);
    end;
  end;
  temp := 0.0;
  for i := 1 to Steps do
  begin
    if (temp < SpectrumWtAv[i]) then temp := SpectrumWtAv[i];
  end;
  if (temp = 0.0) then temp := 1.0e-9;
  //ShowMessage('WtAv = '+FormatFloat('###0.000000',temp)+'__'+FormatFloat('###0.000000',tUncert));
  for i := 0 to Steps do
  begin
    x2 := StartAtX + 1.0*i*(EndAtX-StartAtX)/Steps;
    if (i > 0) then y2 := 100.0 * SpectrumWtAv[i]/temp
               else y2 := 100.0 * SpectrumWtAv[1]/temp;
    ChCum.Series[iEllipsesIncluded].AddXY(x2,y2);
  end;
  ChCum.BottomAxis.SetMinMax(StartAtX,EndatX);
  ChCum.LeftAxis.SetMinMax(0.0,100.0);
  ChCum.LeftAxis.Automatic := true;
  ChCum.Visible := true;
  ChCum.Foot.Caption := GeodateVersionStr;
end;

procedure TfmWtAv.bbExportmdlToXMLClick(Sender: TObject);
var
  iCode : integer;
begin
  {
  if ((VarbNox = 6) and (AnalType in ['8','A','H'])) then
  begin
    AnalType8 := 'N';
    Model := 1;
    try
      ResultsForm := TfmResults.Create(Self);
      ResultsForm.GDWSender := 'WtAv';
      try
        Val(eMSWD.Text,Msum,iCode);
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
          Val(eWtAv.Text,UprIntercept,iCode);
        except
          UprIntercept := 0.0;
        end;
        try
          Val(eWtAvPlus95.Text,UprUprAgeError,iCode);
        except
          UprUprAgeError := 0.0;
        end;
        try
          Val(eWtAvminus95.Text,UprLwrAgeError,iCode);
        except
          UprLwrAgeError := 0.0;
        end;
      end;
      if (AnalType in ['H']) then
      begin
        Val(eWtAv.Text,Age,iCode);
        Val(eWtAvPlus95.Text,AgeError,iCode);
        Val(eWtAvminus95.Text,AgeError,iCode);
      end;
      ResultsForm.ShowModal;
    finally
      ResultsForm.Free;
    end;
  end;
  if ((VarbNox = 4) and (AnalType in ['8','A'])) then
  begin
    AnalType8 := 'N';
    Model := 1;
    try
      ResultsForm := TfmResults.Create(Self);
      ResultsForm.GDWSender := 'WtAv';
      try
        Val(eMSWD.Text,Msum,iCode);
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
          Val(eWtAv.Text,UprIntercept,iCode);
        except
          UprIntercept := 0.0;
        end;
        try
          Val(eWtAvPlus95.Text,UprUprAgeError,iCode);
        except
          UprUprAgeError := 0.0;
        end;
        try
          Val(eWtAvminus95.Text,UprLwrAgeError,iCode);
        except
          UprLwrAgeError := 0.0;
        end;
      end;
      ResultsForm.ShowModal;
    finally
      ResultsForm.Free;
    end;
  end;
  if ((VarbNox in [7,8]) and (AnalType in ['1','2','7','9','E','F','G'])) then
  begin
    AnalType8 := 'N';
    Model := 1;
    try
      ResultsForm := TfmResults.Create(Self);
      ResultsForm.GDWSender := 'WtAv';
      Intercept := 0.0;
      InitRatioError := 0.0;
      Epsilon1 := 0.0;
      EpError1 := 0.0;
      try
        Val(eMSWD.Text,Msum,iCode);
      except
        Msum := 0.0;
      end;
      try
        Val(eIcnt.Text,NumberOfPointsRegressed,iCode);
      except
        NumberOfPointsRegressed := 0;
      end;
      if (AnalType in ['1','2','7','9','E','F','G']) then
      begin
        try
          Val(eWtAv.Text,Age,iCode);
        except
          Age := 0.0;
        end;
        try
          Val(eWtAvExpected95.Text,AgeError,iCode);
        except
          UprUprAgeError := 0.0;
        end;
      end;
      ResultsForm.ShowModal;
    finally
      ResultsForm.Free;
    end;
  end;
  }
end;

procedure TfmWtAv.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {
  try
    GraphColour[1,1] := VtChWtAv.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Red;
    GraphColour[1,2] := VtChWtAv.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Blue;
    GraphColour[1,3] := VtChWtAv.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Green;
    GraphColour[2,1] := VtChWtAv.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Red;
    GraphColour[2,2] := VtChWtAv.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Blue;
    GraphColour[2,3] := VtChWtAv.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Green;
    GraphColour[7,1] := VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Red;
    GraphColour[7,2] := VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Blue;
    GraphColour[7,3] := VtChWtAv.Plot.SeriesCollection.Item[7].Pen.VtColor.Green;
    GraphColour[9,1] := VtChCum.Plot.SeriesCollection.Item[1].Pen.VtColor.Red;
    GraphColour[9,2] := VtChCum.Plot.SeriesCollection.Item[1].Pen.VtColor.Blue;
    GraphColour[9,3] := VtChCum.Plot.SeriesCollection.Item[1].Pen.VtColor.Green;
    UpDateRFlg(Sender);
  finally
  end;
  }
end;

end.


