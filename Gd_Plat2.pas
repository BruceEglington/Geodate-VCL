unit Gd_Plat2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DBCtrls, ExtCtrls, Grids, DBGrids, Printers, OleCtrls,
  AxCtrls, VclTee.TeeGDIPlus, VCLTee.TeeErrorPoint, VCLTee.Series,
  VCL.FlexCel.Core, FlexCel.XlsAdapter, FlexCel.Render, FlexCel.Preview,
  VCLTee.TeEngine, VCLTee.TeeSpline, VCLTee.TeeProcs, VCLTee.Chart,
  VCLTee.TeeComma, Vcl.Mask, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  SVGIconVirtualImageList, Vcl.ComCtrls;

type
  TfmPlatAr = class(TForm)
    Panel1: TPanel;
    bbSpreadSheet: TBitBtn;
    bbOK: TBitBtn;
    bbCumHist: TBitBtn;
    SaveDialogModels: TSaveDialog;
    bbStoreMdl: TBitBtn;
    Panel6: TPanel;
    Panel5: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lWtAvAugmentedSD: TLabel;
    Label6: TLabel;
    lWtAvPlus: TLabel;
    lWtAvMinus: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lWtAvAugmented95: TLabel;
    lErrorsBased: TLabel;
    eWtAv: TEdit;
    eIcnt: TEdit;
    eWtAvExpectedSD: TEdit;
    eWtAvPlus95: TEdit;
    eWtAvMinus95: TEdit;
    eWtAvExpected95: TEdit;
    eWtAvObservedSD: TEdit;
    eWtAvObserved95: TEdit;
    eMSWD: TEdit;
    eWtAvAugmentedSD: TEdit;
    eWtAvAugmented95: TEdit;
    Panel4: TPanel;
    Splitter1: TSplitter;
    TeeCommander1: TTeeCommander;
    ChPlat: TChart;
    Series8: TLineSeries;
    Series5: TLineSeries;
    Series4: TLineSeries;
    Series3: TLineSeries;
    Series10: TPointSeries;
    Series9: TLineSeries;
    Series1: TLineSeries;
    Series6: TPointSeries;
    Series2: TPointSeries;
    Series11: TErrorPointSeries;
    Series12: TErrorPointSeries;
    Series13: TLineSeries;
    Series14: TPointSeries;
    Series7: TPointSeries;
    pCheckBoxes: TPanel;
    cbCurrentSample: TCheckBox;
    Splitter2: TSplitter;
    pResiduals: TPanel;
    lResidual: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    dbnReg: TDBNavigator;
    dbRegSample: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit6: TDBEdit;
    DBEdit7: TDBEdit;
    DBEdit8: TDBEdit;
    DBEdit9: TDBEdit;
    DBEdit10: TDBEdit;
    DBCheckBox1: TDBCheckBox;
    bbUpdate: TBitBtn;
    cbLegend: TCheckBox;
    lModifyGraphSettings: TLabel;
    pTopLeft: TPanel;
    pResultTitle: TPanel;
    Label1: TLabel;
    lResultTitle: TLabel;
    eTitle: TEdit;
    Panel8: TPanel;
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
    SaveDialogSprdSheet: TSaveDialog;
    Label5: TLabel;
    eProbabilityOfF: TEdit;
    Label7: TLabel;
    eNsamp: TEdit;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    Series15: TLineSeries;
    pTreeSmp: TPanel;
    TreeView1: TTreeView;
    lWtAvPlusIncl: TLabel;
    eWtAvPlus95Incl: TEdit;
    lWtAvMinusIncl: TLabel;
    eWtAvMinus95Incl: TEdit;
    lWtAvIncl: TLabel;
    procedure FormShow(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure bbSpreadSheetClick(Sender: TObject);
    procedure bbCumHistClick(Sender: TObject);
    procedure bbStoreMdlClick(Sender: TObject);
    procedure cbLegendClick(Sender: TObject);
    procedure cbCurrentSampleClick(Sender: TObject);
    procedure bbUpdateClick(Sender: TObject);
    procedure UpdateRFlg(Sender: TObject);
    procedure ChPlatZoom(Sender: TObject);
    procedure dbnRegClick(Sender: TObject; Button: TNavigateBtn);
    procedure TreeView1Click(Sender: TObject);
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
    iRec : integer;
    SprdSheet : TXlsFile;
    procedure ShowCurrentPoint;
  public
    { Public declarations }
    HistOK  : boolean;
    VarbNoX : integer;
    AllSame : boolean;
    FileVarStr : string;
    procedure Choose_Wt_Field;
    //function Ar40Ar39Age(X : double; ArArJ : double; ArArJ1sig : double) : double;
    //function Ar40Ar39AgeWithDCErr(X : double; ArArJ : double; ArArJ1sig : double;
    //                              T_Mult : double) : double;
    procedure HideResultLabels;
  end;

var
  fmPlatAr: TfmPlatAr;

implementation

{$R *.DFM}

uses
  GDW_varb, Allsorts, gd_drv, dmGdtmpDB, VCLTee.TeeThemes,
  GDW_regp, gd_HstVl, mathproc;

const
  VtChAxisIdX = 0;
  VtChAxisIdY = 1;

var
  tmpMSWD,
  SD1, SD2   : double;
  GetHistValuesForm : TfmGetHistValues;

{
function TfmPlatAr.Ar40Ar39Age(X : double; ArArJ : double; ArArJ1sig : double) : double;
begin
  if ((X * ArArJ) > -1.0) then Result:=Ln(1.0+X*ArArJ)/(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])
  else Result:=0.0;
end;

function TfmPlatAr.Ar40Ar39AgeWithDCErr(X : double; ArArJ : double; ArArJ1sig : double;
                                        T_Mult : double) : double;
var
  sigArArJ, sigDCKAr, sigDCKCa : double;
  tArAr, tDecayConst : double;
begin
  sigArArJ := ArArJ1sig;
  sigArArJ := 0.0;   // temporary to check influence of decay constants alone
  sigDCKAr := DecayConstUncertainty[ord(atKAr)]*DecayConst[ord(atKAr)]/100.0;
  sigDCKCa := DecayConstUncertainty[ord(atKCa)]*DecayConst[ord(atKCa)]/100.0;
  tArAr := ArArJ+sigArArJ;
  tDecayConst := (DecayConst[ord(atKAr)]-sigDCKAr) + (DecayConst[ord(atKCa)]-sigDCKCa);
  if ((X * ArArJ) > -1.0) then Result:=Ln(1.0+X*tArAr)/tDecayConst
  else Result:=0.0;
end;
}

procedure TfmPlatAr.HideResultLabels;
begin
  lWtAvMinusIncl.Visible := false;
  eWtAvMinus95Incl.Visible := false;
  eWtAvPlus95Incl.Visible := false;
  lWtAvPlusIncl.Visible := false;
  lWtAvIncl.Visible := false;
  ChCum.Series[i]
end;

procedure TfmPlatAr.FormShow(Sender: TObject);
var
  tmpStr : string[20];
  i      : integer;
  jinc, iinc : integer;
  temp   : double;
  tmpT_Mult : double;
  tmpMin, tmpMax : double;
  tmpSingle, tmpAge, tmpX, tmpY : double;
  MaxX, MinX, MaxY, MinY : double;
  SumAr, StartAr, EndAr, SumTotal : double;
  SDAugmented,
  SDMult,SDPlus,SDMinus : double;
  tAge, tUpper, tLower : double;
  tAgeMinus, tAgePlus : double;
  Ar40Ar39, Ar40Ar39sig : double;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
  NumLabelledTics : integer;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  HideResultLabels;
  //TSystemTheme.ApplyStyle(ChCum);
  //TSystemTheme.ApplyStyle(ChPlat);
  bbStoreMdl.Enabled := false;
  ChCum.Visible := false;
  CMaxX := 0.0;
  CMinX := 0.0;
  T_Mult:=TMultiplier(1.0*N_Rep);
  tmpT_Mult := T_Mult;
  if (EllipseMagnif = 1.0) then tmpT_Mult := 1.0;

  ChPlat.Series[iEllipsesExcluded].Clear;
  ChPlat.Series[iEllipsesIncluded].Clear;
  ChPlat.Series[iEnvelopeLower].Clear;
  ChPlat.Series[iEnvelopeUpper].Clear;
  ChPlat.Series[iCurveTic].Clear;
  ChPlat.Series[iCurveLine].Clear;
  ChPlat.Series[iRegressionLine].Clear;
  ChPlat.Series[iDataExcluded].Clear;
  ChPlat.Series[iDataIncluded].Clear;
  ChPlat.Series[iErrorExcluded].Clear;
  ChPlat.Series[iErrorIncluded].Clear;
  ChPlat.Series[iEllipseConcordia].Clear;
  ChPlat.Series[iDataConcordia].Clear;
  ChPlat.Series[iCurrent].Clear;
  ChPlat.Title.Caption := Title;
  ChPlat.BottomAxis.Title.TextFormat:=ttfHtml;
  ChPlat.BottomAxis.Title.Text := 'Cumulative <sup>39</sup>Ar';
  //ShowMessage('1');
  Choose_Wt_Field;
  if ((VarbNoX in [6])) then
  begin
    case VarbNoX of
      6 : begin
        if (AnalType in ['C']) then
        begin
          lResultTitle.Caption := 'Mean T (39Ar/40Ar)';
          FileVarStr := 'Mean T ArAR';
          ChPlat.Visible := true;
          ChPlat.LeftAxis.Title.TextFormat:=ttfHtml;
          ChPlat.LeftAxis.Title.Text := 'Mean T (<sup>39</sup>Ar/<sup>40</sup>Ar)';
        end;
        if (AnalType in ['D']) then
        begin
          lResultTitle.Caption := 'Mean T (40Ar/39Ar)';
          FileVarStr := 'Mean T ArAR';
          ChPlat.Visible := true;
          ChPlat.LeftAxis.Title.TextFormat:=ttfHtml;
          ChPlat.LeftAxis.Title.Text := 'Mean T (<sup>40</sup>Ar/<sup>39</sup>Ar)';
        end;
        if (AnalType in ['I']) then
        begin
          lResultTitle.Caption := 'Mean T (40Ar*/39Ar)';
          FileVarStr := 'Mean T ArAr';
          ChPlat.Visible := true;
          ChPlat.LeftAxis.Title.TextFormat:=ttfHtml;
          ChPlat.LeftAxis.Title.Text := 'Mean T (<sup>40</sup>Ar<sup>*</sup>/<sup>39</sup>Ar)';
        end;
      end;
    end;
    //ShowMessage('2');
    Icnt := 0;
    WtAver(NumberOfPoints,temp,tmpMSWD,SD1,SD2,Icnt);
    //ShowMessage('3');
    if (Icnt > 0) then
    begin
      ArArJ := Ratio[1,3];
      ArArJ1sig := ErrorWt[1,3];
      // if value for Ar-Ar J value uncertainty is too large to be actual
      // value, then assume it is percentage and convert to actual;
      if (ArArJ1sig > 0.0001) then ArArJ1sig := ArArJ1sig*ArArJ/100.0;
      eTitle.Text := Title;
      Str(temp:12:6,tmpStr);
      eWtAv.Text := tmpStr;
      Str(Icnt:3,tmpStr);
      eIcnt.Text := tmpStr;
      Str(NumberOfPoints:3,tmpStr);
      eNsamp.Text := tmpStr;
      lWtAvPlus.Visible := false;
      lWtAvMinus.Visible := false;
      eWtAvPlus95.Visible := false;
      eWtAvMinus95.Visible := false;
      Str(SD1:12:6,tmpStr);
      eWtAvExpectedSD.Text := tmpStr;
      Str((SD1*TMultiplier(1.0*N_Rep)):12:6,tmpStr);
      eWtAvExpected95.Text := tmpStr;
      SDMult := SD1*TMultiplier(1.0*(N_Rep));
      Str(SD2:12:6,tmpStr);
      eWtAvObservedSD.Text := tmpStr;
      if (Icnt > 1) then Str((SD2*TMultiplier(1.0*(Icnt-1))):12:6,tmpStr)
                    else Str((SD2*TMultiplier(1.0*(N_Rep))):12:6,tmpStr);
      eWtAvObserved95.Text := tmpStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(Icnt-1),1.0*(N_Rep),tmpMSWD,1);
      if (ProbabilityOfFit >= FAlpha) then
      begin
        if (SD1 >= SD2) then
        begin
          SDMult := SD1*TMultiplier(1.0*(N_Rep));
          Str((SD1*TMultiplier(1.0*(N_Rep))):12:6,tmpStr);
        end;
        if (SD1 < SD2) then
        begin
          if (Icnt > 1) then
          begin
            SDMult := SD2*TMultiplier(1.0*(Icnt-1));
            Str((SD2*TMultiplier(1.0*(Icnt-1))):12:6,tmpStr);
          end
          else begin
            SDMult := SD2*TMultiplier(1.0*(N_Rep));
            Str((SD2*TMultiplier(1.0*(N_Rep))):12:6,tmpStr);
          end;
        end;
      end;
      lWtAvAugmentedSD.Visible := false;
      lWtAvAugmented95.Visible := false;
      eWtAvAugmentedSD.Visible := false;
      eWtAvAugmented95.Visible := false;
      if (ProbabilityOfFit < FAlpha) then
      begin
        SDAugmented := SD1*Sqrt(tmpMSWD);
        Str(SDAugmented:12:6,tmpStr);
        eWtAvAugmentedSD.Text := tmpStr;
        SDMult := SDAugmented*TMultiplier(1.0*(Icnt-1));
        Str(SDMult:12:6,tmpStr);
        eWtAvAugmented95.Text := tmpStr;
        lWtAvAugmentedSD.Visible := true;
        lWtAvAugmented95.Visible := true;
        eWtAvAugmentedSD.Visible := true;
        eWtAvAugmented95.Visible := true;
      end;
      Str(tmpMSWD:9:3,tmpStr);
      eMSWD.Text := tmpStr;
      eProbabilityOfF.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfF.Font.Color := clRed
                                     else eProbabilityOfF.Font.Color := clBlue;
      lErrorsBased.Visible := false;
      //ShowMessage('4');
      if ((AnalType in ['C']) and (VarbNoX in [6])) then
      begin
        if (ProbabilityOfFit >= FAlpha) then
        begin
          Ar40Ar39 := temp;
          Age:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
          if (SD1 >= SD2) then
          begin
            Ar40Ar39 := temp;
            Ar40Ar39sig := Ar40Ar39*(SD1/temp);
            Slope:=Ar40Ar39+Ar40Ar39sig*TMultiplier(1.0*N_Rep);
          end;
          if (SD1 < SD2) then
          begin
            Ar40Ar39 := temp;
            Ar40Ar39sig := Ar40Ar39*(SD2/temp);
            tmpT_Mult := TMultiplier(1.0*(Icnt-1));
            Slope:=Ar40Ar39+Ar40Ar39sig*TMultiplier(1.0*(Icnt-1));
          end;
          UprUprAgeError:=Age-Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult)-Age;
          if (SD1 >= SD2) then
          begin
            Ar40Ar39 := temp;
            Ar40Ar39sig := Ar40Ar39*(SD1/temp);
            tmpT_Mult := TMultiplier(1.0*N_Rep);
            Slope:=Ar40Ar39-Ar40Ar39sig*TMultiplier(1.0*N_Rep);
          end;
          if (SD1 < SD2) then
          begin
            Ar40Ar39 := temp;
            Ar40Ar39sig := Ar40Ar39*(SD2/temp);
            tmpT_Mult := TMultiplier(1.0*(Icnt-1));
            Slope:=Ar40Ar39-Ar40Ar39sig*TMultiplier(1.0*(Icnt-1));
          end;
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Ar40Ar39Age(Slope,ArArJ,ArArJ1sig)-Age;
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl := Age-Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          if (SD1 >= SD2) then
          begin
            Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
            lErrorsBased.Caption := 'Date errors based on expected s.d. of mean and t = '
              +tmpStr;
          end;
          if (SD1 < SD2) then
          begin
            Str(TMultiplier(1.0*(Icnt-1)):7:2,tmpStr);
            lErrorsBased.Caption := 'Date errors based on observed s.d. of mean and t = '
              +tmpStr;
          end;
        end;
        //ShowMessage('5');
        if (ProbabilityOfFit < FAlpha) then
        begin
          Ar40Ar39 := temp;
          Age:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
          Ar40Ar39sig := Ar40Ar39*(SD1/temp);
          tmpT_Mult := TMultiplier(1.0*(Icnt-1));
          Slope:=Ar40Ar39+Ar40Ar39sig*Sqrt(tmpMSWD)*TMultiplier(1.0*(Icnt-1));
          UprUprAgeError:=Age-Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult)-Age;
          Slope:=Ar40Ar39-Ar40Ar39sig*Sqrt(tmpMSWD)*TMultiplier(1.0*(Icnt-1));
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age - Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl := Age-Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          Str((TMultiplier(1.0*(Icnt-1))):7:2,tmpStr);
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
        lWtAvPlus.Visible := true;
        lWtAvMinus.Visible := true;
        eWtAvPlus95.Visible := true;
        eWtAvMinus95.Visible := true;
        if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
        begin
          lWtAvIncl.Visible := true;
          lWtAvPlusIncl.Visible := true;
          eWtAvPlus95Incl.Visible := true;
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvPlus95Incl.Text := tmpStr;
          lWtAvMinusIncl.Visible := true;
          eWtAvMinus95Incl.Visible := true;
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvMinus95Incl.Text := tmpStr;
        end;
      end;
      if ((AnalType in ['D']) and (VarbNoX in [6])) then
      begin
        if (ProbabilityOfFit >= FAlpha) then
        begin
          Ar40Ar39 := temp;
          //ShowMessage('temp = '+FormatFloat('#####0.0000000000',temp));
          Age:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
          if (SD1 >= SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD1/temp);
            //ShowMessage('1 Ar40Ar39sig = '+FormatFloat('#####0.0000000000',Ar40Ar39sig));
            tmpT_Mult := TMultiplier(1.0*N_Rep);
            Slope:=Ar40Ar39+Ar40Ar39sig*TMultiplier(1.0*N_Rep);
          end;
          if (SD1 < SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD2/temp);
            //ShowMessage('2 Ar40Ar39sig = '+FormatFloat('#####0.0000000000',Ar40Ar39sig));
            tmpT_Mult := TMultiplier(1.0*(Icnt-1));
            Slope:=Ar40Ar39+Ar40Ar39sig*TMultiplier(1.0*(Icnt-1));
          end;
          UprUprAgeError:=Ar40Ar39Age(Slope,ArArJ,ArArJ1sig) - Age;
          UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult)-Age;
          if (SD1 >= SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD1/temp);
            //ShowMessage('3 Ar40Ar39sig = '+FormatFloat('#####0.0000000000',Ar40Ar39sig));
            tmpT_Mult := TMultiplier(1.0*N_Rep);
            Slope:=Ar40Ar39-Ar40Ar39sig*TMultiplier(1.0*N_Rep);
          end;
          if (SD1 < SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD2/temp);
            //ShowMessage('4 Ar40Ar39sig = '+FormatFloat('#####0.0000000000',Ar40Ar39sig));
            tmpT_Mult := TMultiplier(1.0*(Icnt-1));
            Slope:=Ar40Ar39-Ar40Ar39sig*TMultiplier(1.0*(Icnt-1));
          end;
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age - Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl := Age-Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          if (SD1 >= SD2) then
          begin
            Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
            lErrorsBased.Caption := 'Date errors based on expected s.d. of mean and t = '
              +tmpStr;
          end;
          if (SD1 < SD2) then
          begin
            Str(TMultiplier(1.0*(Icnt-1)):7:2,tmpStr);
            lErrorsBased.Caption := 'Date errors based on observed s.d. of mean and t = '
              +tmpStr;
          end;
        end;
        //ShowMessage('5');
        if (ProbabilityOfFit < FAlpha) then
        begin
          Ar40Ar39 := temp;
          Age:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
          Ar40Ar39sig := Ar40Ar39*(SD1/temp);
          //ShowMessage('5 Ar40Ar39sig = '+FormatFloat('#####0.0000000000',Ar40Ar39sig));
          tmpT_Mult := TMultiplier(1.0*(Icnt-1));
          Slope:=Ar40Ar39+Ar40Ar39sig*Sqrt(tmpMSWD)*TMultiplier(1.0*(Icnt-1));
          UprUprAgeError:=Ar40Ar39Age(Slope,ArArJ,ArArJ1sig) - Age;
          UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult)-Age;
          Slope:=Ar40Ar39-Ar40Ar39sig*Sqrt(tmpMSWD)*TMultiplier(1.0*(Icnt-1));
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age - Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl := Age-Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          Str((TMultiplier(1.0*(Icnt-1))):7:2,tmpStr);
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
        lWtAvPlus.Visible := true;
        lWtAvMinus.Visible := true;
        eWtAvPlus95.Visible := true;
        eWtAvMinus95.Visible := true;
        if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
        begin
          lWtAvIncl.Visible := true;
          lWtAvPlusIncl.Visible := true;
          eWtAvPlus95Incl.Visible := true;
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvPlus95Incl.Text := tmpStr;
          lWtAvMinusIncl.Visible := true;
          eWtAvMinus95Incl.Visible := true;
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvMinus95Incl.Text := tmpStr;
        end;
      end;
      if ((AnalType in ['I']) and (VarbNoX in [6])) then
      begin
        if (ProbabilityOfFit >= FAlpha) then
        begin
          Ar40Ar39 := temp;
          Age:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
          if (SD1 >= SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD1/temp);
            tmpT_Mult := TMultiplier(1.0*N_Rep);
            Slope:=Ar40Ar39+Ar40Ar39sig*TMultiplier(1.0*N_Rep);
          end;
          if (SD1 < SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD2/temp);
            tmpT_Mult := TMultiplier(1.0*(Icnt-1));
            Slope:=Ar40Ar39+Ar40Ar39sig*TMultiplier(1.0*(Icnt-1));
          end;
          UprUprAgeError:=Ar40Ar39Age(Slope,ArArJ,ArArJ1sig) - Age;
          UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult)-Age;
          if (SD1 >= SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD1/temp);
            tmpT_Mult := TMultiplier(1.0*N_Rep);
            Slope:=Ar40Ar39-Ar40Ar39sig*TMultiplier(1.0*N_Rep);
          end;
          if (SD1 < SD2) then
          begin
            Ar40Ar39sig := Ar40Ar39*(SD2/temp);
            tmpT_Mult := TMultiplier(1.0*(Icnt-1));
            Slope:=Ar40Ar39-Ar40Ar39sig*TMultiplier(1.0*(Icnt-1));
          end;
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age - Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl := Age - Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          if (SD1 >= SD2) then
          begin
            Str(TMultiplier(1.0*(N_Rep)):7:2,tmpStr);
            lErrorsBased.Caption := 'Date errors based on expected s.d. of mean and t = '
              +tmpStr;
          end;
          if (SD1 < SD2) then
          begin
            Str(TMultiplier(1.0*(Icnt-1)):7:2,tmpStr);
            lErrorsBased.Caption := 'Date errors based on observed s.d. of mean and t = '
              +tmpStr;
          end;
        end;
        //ShowMessage('5');
        if (ProbabilityOfFit < FAlpha) then
        begin
          Ar40Ar39 := temp;
          Age:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
          Ar40Ar39sig := Ar40Ar39*(SD1/temp);
          tmpT_Mult := TMultiplier(1.0*(Icnt-1));
          Slope:=Ar40Ar39+Ar40Ar39sig*Sqrt(tmpMSWD)*TMultiplier(1.0*(Icnt-1));
          UprUprAgeError:=Ar40Ar39Age(Slope,ArArJ,ArArJ1sig) - Age;
          UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult)-Age;
          Slope:=Ar40Ar39-Ar40Ar39sig*Sqrt(tmpMSWD)*TMultiplier(1.0*(Icnt-1));
          if (Slope<0.0) then UprLwrAgeError:=Age
                         else UprLwrAgeError:=Age - Ar40Ar39Age(Slope,ArArJ,ArArJ1sig);
          if (Slope<0.0) then UprLwrAgeErrorIncl:=Age
                         else UprLwrAgeErrorIncl := Age-Ar40Ar39AgeWithDCErr(Slope,ArArJ,ArArJ1sig,tmpT_Mult);
          Age:=Age/1.0e6;
          UprUprAgeError:=UprUprAgeError/1.0e6;
          UprLwrAgeError:=UprLwrAgeError/1.0e6;
          UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
          UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
          Str((TMultiplier(1.0*(Icnt-1))):7:2,tmpStr);
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
        if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
        begin
          lWtAvIncl.Visible := true;
          lWtAvPlusIncl.Visible := true;
          eWtAvPlus95Incl.Visible := true;
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvPlus95Incl.Text := tmpStr;
          lWtAvMinusIncl.Visible := true;
          eWtAvMinus95Incl.Visible := true;
          Str(UprUprAgeErrorIncl:9:2,tmpStr);
          eWtAvMinus95Incl.Text := tmpStr;
        end;
      end;
    end else
    begin
      eWtAv.Text := '';
      Str(Icnt:3,tmpStr);
      eIcnt.Text := tmpStr;
      Str(NumberOfPoints:3,tmpStr);
      eNsamp.Text := tmpStr;
      lWtAvPlus.Visible := false;
      lWtAvMinus.Visible := false;
      eWtAvPlus95.Visible := false;
      eWtAvMinus95.Visible := false;
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
    iinc := 0;
    jinc := 0;
    MaxX := -1e9;
    MinX :=  0.0;
    MaxY := -1e9;
    MinY :=  9e9;
    SumAr := 0.0;
    //mean
    SumTotal := 0.0;
    for i := 1 to NumberOfPoints do
    begin
      SumTotal := SumTotal + Xtra3[i];
    end;
    if (SumTotal = 0.0) then SumTotal := 1.0;
    for i := 1 to NumberOfPoints do
    begin
        if ((not AllSame) and (VarbNoX in [6])) then
        begin
          if (AnalType in ['C','D','I']) then
          begin
            StartAr := SumAr*100.0/SumTotal;
            SumAr := SumAr + Xtra3[i];
            EndAr := SumAr*100.0/SumTotal;
            if (MaxX < SumAr) then MaxX := SumAr;
            if (PFlg[i] = 'Y') then
            begin
              Ar40Ar39 := Xtra[i];
              tAge:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig);
              Ar40Ar39sig := Ar40Ar39*(Xtra1[i]/Xtra[i]);
              tAge := (Ar40Ar39Age((Xtra[i]),ArArJ,ArArJ1sig))/1.0e6;
              tAgePlus := (Ar40Ar39Age(Ar40Ar39+Ar40Ar39sig*tmpT_Mult,ArArJ,ArArJ1sig))/1.0e6;
              tAgeMinus := (Ar40Ar39Age(Ar40Ar39-Ar40Ar39sig*tmpT_Mult,ArArJ,ArArJ1sig))/1.0e6;
              if (tAge <= 0.0) then tAge := 0.0;
              if (tAgeMinus <= 0.0) then tAgeMinus := 0.0;
              if (tAgePlus <= 0.0) then tAgePlus := 0.0;
              tLower := tAge - tAgeMinus;
              if ((tAge-tLower) <= 0.0) then tLower := tAge;
              if (tLower <= 0.0) then tLower := tAge;
              tUpper := tAgePlus - tAge;
              if (MaxY < (tAge+tUpper))
                then MaxY := tAge+tUpper;
              if (MinY > (tAge-tLower))
                then MinY := tAge-tLower;
              if (RFlg[i] = 'Y') then
              begin
                //Minus error
                iinc := iinc + 1;
                Series11.Add((EndAr+StartAr)/2.0,tAgeMinus,(EndAr-StartAr)/2.0,(EndAr-StartAr)/2.0,0.0,0.0);
                //Plus error
                iinc := iinc + 1;
                Series11.Add((EndAr+StartAr)/2.0,tAgePlus,(EndAr-StartAr)/2.0,(EndAr-StartAr)/2.0,0.0,0.0);
                //left of box
                iinc := iinc + 1;
                //Series11.Add(StartAr,tAge,0.0,0.0,tAge-tUpper,tLower-tAge);
                Series11.Add(StartAr,tAge,0.0,0.0,tUpper,tLower);
                //right of box
                iinc := iinc + 1;
                //Series11.Add(EndAr,tAge,0.0,0.0,tAge-tUpper,tLower-tAge);
                Series11.Add(EndAr,tAge,0.0,0.0,tUpper,tLower);
              end;
              if (RFlg[i] = 'N') then
              begin
                //ShowMessage(IntToStr(i)+'***'+FormatFloat('###0.000000',Xtra[i])+'***'+FormatFloat('###0.000000',Xtra1[i]));
                //Minus error
                jinc := jinc + 1;
                Series12.Add((EndAr+StartAr)/2.0,tAgeMinus,(EndAr-StartAr)/2.0,(EndAr-StartAr)/2.0,0.0,0.0);
                //Plus error
                jinc := jinc + 1;
                Series12.Add((EndAr+StartAr)/2.0,tAgePlus,(EndAr-StartAr)/2.0,(EndAr-StartAr)/2.0,0.0,0.0);
                //left of box
                iinc := iinc + 1;
                //Series12.Add(StartAr,tAge,0.0,0.0,tAge-tUpper,tLower-tAge);
                Series12.Add(StartAr,tAge,0.0,0.0,tUpper,tLower);
                //right of box
                iinc := iinc + 1;
                //Series12.Add(EndAr,tAge,0.0,0.0,tAge-tUpper,tLower-tAge);
                Series12.Add(EndAr,tAge,0.0,0.0,tUpper,tLower);
              end;
            end;
          end;
        end;
    end;
    MinX := 0.0;
    MaxX := 100.0;
    if (MaxX <= MinX) then MaxX := MinX + 0.1*MinX;
    if (MaxY <= MinY) then MaxY := MinY + 0.1*MinY;
    if ((Icnt > 0) and (not AllSame) and (VarbNoX in [6]) and (AnalType in ['C','D','I'])) then
    begin
      tmpMin := MinX;
      tmpMax := MaxX;
      tmpX := tmpMin;
      ChPlat.Series[iRegressionLine].AddXY(tmpX,(Ar40Ar39Age((temp),ArArJ,ArArJ1sig))/1.0e6);
      ChPlat.Series[iEnvelopeLower].AddXY(tmpX,(Ar40Ar39Age((temp-SDMult),ArArJ,ArArJ1sig))/1.0e6);
      ChPlat.Series[iEnvelopeUpper].AddXY(tmpX,(Ar40Ar39Age((temp+SDMult),ArArJ,ArArJ1sig))/1.0e6);
      repeat
        ChPlat.Series[iRegressionLine].AddXY(tmpX,(Ar40Ar39Age((temp),ArArJ,ArArJ1sig))/1.0e6);
        ChPlat.Series[iEnvelopeLower].AddXY(tmpX,(Ar40Ar39Age((temp-SDMult),ArArJ,ArArJ1sig))/1.0e6);
        ChPlat.Series[iEnvelopeUpper].AddXY(tmpX,(Ar40Ar39Age((temp+SDMult),ArArJ,ArArJ1sig))/1.0e6);
        tmpX := tmpX + 0.1*(tmpMax-tmpMin);
      until (tmpX > tmpMax);
      tmpX := tmpMax;
      ChPlat.Series[iRegressionLine].AddXY(tmpX,(Ar40Ar39Age((temp),ArArJ,ArArJ1sig))/1.0e6);
      ChPlat.Series[iEnvelopeLower].AddXY(tmpX,(Ar40Ar39Age((temp-SDMult),ArArJ,ArArJ1sig))/1.0e6);
      ChPlat.Series[iEnvelopeUpper].AddXY(tmpX,(Ar40Ar39Age((temp+SDMult),ArArJ,ArArJ1sig))/1.0e6);
    end;
    MaxX := MaxX + 0.1*(MaxX-MinX);
    MinX := MinX - 0.1*(MaxX-MinX);
    MaxY := MaxY + 0.1*(MaxY-MinY);
    MinY := MinY - 0.1*(MaxY-MinY);
    ChPlat.BottomAxis.SetMinMax(MinX,MaxX);
    ChPlat.LeftAxis.SetMinMax(MinY,MaxY);
    //ChPlat.BottomAxis.Automatic := false;
  //Current sample from tmpRes.DB file
  //ChPlat.Series[iCurrent].XValue[0] := dmGdwtmp.cdsRegXRatio.AsFloat;
  //ChPlat.Series[iCurrent].YValue[0] := dmGdwtmp.cdsRegYRatio.AsFloat;
  end;
  if (EllipseMagnif = 1.0) then ChPlat.SubFoot.Caption := '1 sigma uncertainties';
  if (EllipseMagnif > 1.0) then ChPlat.SubFoot.Caption := '95% conf. uncertainties';
  ChPlat.Legend.Visible := cbLegend.Checked;
  ChPlat.Foot.Caption := GeodateVersionStr;
end;

procedure TfmPlatAr.bbOKClick(Sender: TObject);
begin
  Close;
end;


procedure TfmPlatAr.Choose_Wt_Field;
var
  LWt          : array[1..2] of double;
  i            : integer;
begin
  HistOK:=false;
  for i:=1 to NumberOfPoints do
  begin
      case VarbNoX of
        6 : begin   {}
          {
          if (AnalType in ['C']) then
          begin
            if (Abs(Ratio[i,2]) > 0.0) then
            begin
              Xtra[i]:=Ratio[i,1]/Ratio[i,2];
            end else
            begin
              Xtra[i] := 0.0;
            end;
            case ErrTyp[i] of
              '1' : begin
                 LWt[1] := Sqrt(Abs(ErrorWt[i,1]*ErrorWt[i,1] - ErrorWt[i,2]*ErrorWt[i,2]));
                 LWt[2] := ErrorWt[i,2];
               end;
              '2' : begin
                 LWt[2] := 1.0;
                 if (Abs(Ratio[i,2]) > 0.0) then LWt[2] := 100.0*ErrorWt[i,2]/Ratio[i,2];
                 LWt[1] := Sqrt(Abs(ErrorWt[i,1]*ErrorWt[i,1] - LWt[2]*LWt[2]))*Xtra[i]/100.0;
                 LWt[2] := ErrorWt[i,2];
               end;
              '3' : begin
                 LWt[1] := 1.0;
                 if (Abs(Ratio[i,1]) > 0.0) then LWt[1] := 100.0*ErrorWt[i,1]/Ratio[i,1];
                 LWt[1] := Sqrt(Abs(LWt[1]*LWt[1] - ErrorWt[i,2]*ErrorWt[i,2]))*Xtra[i]/100.0;
                 LWt[2]:=ErrorWt[i,2];
               end;
              '4' : begin
                 LWt[1] := 1.0;
                 if (Abs(Ratio[i,1]) > 0.0) then LWt[1] := 100.0*ErrorWt[i,1]/Ratio[i,1];
                 LWt[2] := 1.0;
                 if (Abs(Ratio[i,2]) > 0.0) then LWt[2] := 100.0*ErrorWt[i,2]/Ratio[i,2];
                 LWt[1]:=Sqrt(Abs(LWt[1]*LWt[1] - LWt[2]*LWt[2]))*Xtra[i]/100.0;
                 LWt[2]:=ErrorWt[i,2];
               end;
            end;
            Xtra1[i] := LWt[1];
            if (Xtra[i] > 0.0) then Xtra1[i] := 1.0/Xtra[i] * Xtra1[i]/Xtra[i]
                               else Xtra1[i] := 999.9;
            Xtra2[i] := Xtra1[i];
            if (Xtra[i] > 0.0) then Xtra[i] := 1.0/Xtra[i]
                               else Xtra[i] := 999.9;

            //ShowMessage(IntToStr(i)+'***'+FormatFloat('###0.000000',Xtra[i])+'***'+FormatFloat('###0.000000',LWt[1]));
          end;
          if (AnalType in ['D']) then
          begin
            Xtra[i]:=Ratio[i,1];
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
                 LWt[2]:=ErrorWt[i,2]*Ratio[i,2]/100.0;
               end;
              '4' : begin
                 LWt[1]:=ErrorWt[i,1];
                 LWt[2]:=ErrorWt[i,2];
               end;
            end;
            Xtra1[i] := LWt[1];
            if (Xtra[i] > 0.0) then Xtra1[i] := 1.0/Xtra[i] * Xtra1[i]/Xtra[i]
                               else Xtra1[i] := 999.9;
            Xtra2[i] := Xtra1[i];
            if (Xtra[i] > 0.0) then Xtra[i] := 1.0/Xtra[i]
                               else Xtra[i] := 999.9;
          end;
          if (AnalType in ['C','D']) then
          begin
            Xtra3[i] := Conc[i,2];
          end;
          }
          if (AnalType in ['I']) then
          begin
            Xtra[i] := 1.0/Ratio[i,2];
            Xtra3[i] := Ratio[i,1];
            case ErrTyp[i] of
              '1' : begin
                 LWt[1]:=ErrorWt[i,1];
                 LWt[2]:=ErrorWt[i,2];
               end;
              '2' : begin
                 LWt[1]:=ErrorWt[i,1];
                 LWt[2]:=100.0*ErrorWt[i,2]/Ratio[i,2];
               end;
              '3' : begin
                 LWt[1]:=100.0*ErrorWt[i,1]/Ratio[i,1];
                 LWt[2]:=ErrorWt[i,2];
               end;
              '4' : begin
                 LWt[1]:=100.0*ErrorWt[i,1]/Ratio[i,1];
                 LWt[2]:=100.0*ErrorWt[i,2]/Ratio[i,2];
               end;
            end;
            Xtra1[i] := LWt[2]*Xtra[i]/100.0;
            if (Xtra[i] > 0.0) then Xtra1[i] := 1.0/Xtra[i] * Xtra1[i]/Xtra[i]
                               else Xtra1[i] := 999.9;
            Xtra2[i] := Xtra1[i];
            if (Xtra[i] > 0.0) then Xtra[i] := 1.0/Xtra[i]
                               else Xtra[i] := 999.9;
          end;
        end;
      end;
  end;
end;


procedure TfmPlatAr.ChPlatZoom(Sender: TObject);
begin
  //
end;

procedure TfmPlatAr.ShowCurrentPoint;
var
  ii : integer;
  tmpX , tmpY : double;
begin
  ChPlat.Series[iCurrent].Clear;
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
  ChPlat.Series[iCurrent].AddXY(tmpX,tmpY);
end;

procedure TfmPlatAr.TreeView1Click(Sender: TObject);
var
  tmpStr : string;
  i : integer;
  tx, ty : double;
begin
  i := TreeView1.Selected.AbsoluteIndex;
  tmpStr := TreeView1.Items.Item[i].Text;
  dmGdwtmp.cdsReg.Locate('Sample_No',tmpStr,[]);
  ShowCurrentPoint;
end;

procedure TfmPlatAr.dbnRegClick(Sender: TObject; Button: TNavigateBtn);
var
  ii : integer;
  tmpX , tmpY : double;
begin
  ShowCurrentPoint;
end;

procedure TfmPlatAr.bbSpreadSheetClick(Sender: TObject);
var
  i, iRow, iCol    : integer;
  StepSize, t1, temp : double;
  tAge, tLower, tUpper, tmpT_Mult : double;
  tmpStr   : string;
  StepIncrement : integer;
  MinX,MaxX,MinY,MaxY : double;
  zero : double;
  Ar40Ar39, Ar40Ar39sig : double;
begin
  zero := 0.0;
  T_Mult:=TMultiplier(1.0*N_Rep);
  tmpT_Mult := T_Mult;
  try
    MinX := ChPlat.BottomAxis.Minimum;
    MaxX := ChPlat.BottomAxis.Maximum;
    MinY := ChPlat.LeftAxis.Minimum;
    MaxY := ChPlat.LeftAxis.Maximum;
      SaveDialogSprdSheet.InitialDir := TTPath;
      SaveDialogSprdSheet.FileName := ProjectName+'_Plateau';
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
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 3;
          tmpStr := Element[IAnalTyp,2];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 4;
          tmpStr := XRatioStr[IAnalTyp];
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 5;
          tmpStr := 'Precision';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 6;
          tmpStr := '1 sigma';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 8;
          tmpStr := YRatioStr[IAnalTyp];
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
          tmpStr := ZRatioStr[IAnalTyp];
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
          tmpStr := 'Included';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 26;
          tmpStr := '+95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 27;
          tmpStr := '-95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 28;
          tmpStr := 'Excluded';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 29;
          tmpStr := '+95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 30;
          tmpStr := '-95%';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
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
              Ar40Ar39 := Xtra[i];
              tAge:=Ar40Ar39Age(Ar40Ar39,ArArJ,ArArJ1sig)/1.0e6;
              //Ar40Ar39sig := Ar40Ar39*(Xtra1[i]/Xtra[i]);
              Ar40Ar39sig := Xtra1[i];
              tLower := (Ar40Ar39Age(Ar40Ar39-Ar40Ar39sig*tmpT_Mult,ArArJ,ArArJ1sig))/1.0e6;
              tUpper := (Ar40Ar39Age(Ar40Ar39+Ar40Ar39sig*tmpT_Mult,ArArJ,ArArJ1sig))/1.0e6;
              tLower := tAge - tLower;
              if ((tAge-tLower) <= 0.0) then tLower := tAge;
              if (tLower <= 0.0) then tLower := 0.0;
              tUpper := tUpper - tAge;
              if (tUpper <= 0.0) then tUpper := tAge;
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
              if (RFlg[i] = 'Y') then
              begin
                iCol := 25;
                //tmpStr := FloatToStr(1.0*i);
                SprdSheet.SetCellValue(iRow,iCol,tAge);
                iCol := 26;
                //tmpStr := FloatToStr(0.0);
                SprdSheet.SetCellValue(iRow,iCol,tUpper);
                iCol := 27;
                //tmpStr := FloatToStr(0.0);
                SprdSheet.SetCellValue(iRow,iCol,tLower);
              end;
              if (RFlg[i] = 'N') then
              begin
                iCol := 28;
                //tmpStr := FloatToStr(Xtra[i]);
                SprdSheet.SetCellValue(iRow,iCol,tAge);
                iCol := 29;
                //tmpStr := FloatToStr(Xtra1[i]);
                SprdSheet.SetCellValue(iRow,iCol,tUpper);
                iCol := 30;
                //tmpStr := FloatToStr(Xtra2[i]);
                SprdSheet.SetCellValue(iRow,iCol,tLower);
              end;
            end;
          end;
          iRow := iRow + 1;
          //Wt average line
          StepIncrement := 20; //originally 20
          StepSize := (MaxX-MinX)/(1.0*StepIncrement);
          temp:=ChPlat.Series[iRegressionLine].YValue[1];
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
          temp:=ChPlat.Series[iEnvelopeLower].YValue[1];
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
          temp:=ChPlat.Series[iEnvelopeUpper].YValue[1];
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
          SprdSheet.Save(SaveDialogSprdSheet.FileName);
        finally
          FreeAndNil(SprdSheet);
        end;
      end;
  finally
  end;
end;


procedure TfmPlatAr.bbCumHistClick(Sender: TObject);
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
  tAge, tLower, tUpper : double;
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
  ShowCumGraph := true;
  if (Sender = bbCumHist) then
  begin
    GetHistValuesForm := TfmGetHistValues.Create(Self);
    if (CMaxX = 0.0) then
    begin
      tAge := (Ar40Ar39Age((Xtra[1]),ArArJ,ArArJ1sig))/1.0e6;
      CMaxX := tAge;
      CMinX := tAge;
      for i := 1 to NumberOfPoints do
      begin
        if ((RFlg[i] = 'Y') and (PFlg[i]='Y')) then
        begin
          tAge := (Ar40Ar39Age((Xtra[i]),ArArJ,ArArJ1sig))/1.0e6;
          tUpper := (Ar40Ar39Age((Xtra[i]+Xtra1[i]),ArArJ,ArArJ1sig))/1.0e6;
          tLower := (Ar40Ar39Age((Xtra[i]-Xtra2[i]),ArArJ,ArArJ1sig))/1.0e6;
          tLower := tAge - tLower;
          if (tLower <= 0.0) then tLower := 0.0;
          tUpper := tUpper - tAge;
          if (tUpper <= 0.0) then tUpper := 0.0;
          if (CMaxX < tAge) then CMaxX := tAge;
          if (CMinX > tAge) then CMinX := tAge;
        end;
      end;
      if (CMaxX <= CMinX) then CMaxX := CMinX + 0.1*CMinX;
      StartAtX := CMinX;
      EndAtX := CMaxX;
      if (StartAtX > ChPlat.LeftAxis.Minimum) then StartAtX := ChPlat.LeftAxis.Minimum;
      if (EndAtX < ChPlat.LeftAxis.Maximum) then EndAtX := ChPlat.LeftAxis.Maximum;
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
    StartAtX := ChPlat.LeftAxis.Minimum;
    EndAtX := ChPlat.LeftAxis.Maximum;
    ChCum.BottomAxis.SetMinMax(StartAtX,EndAtX);
  end;
  ChCum.Visible := true;
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
      tAge := (Ar40Ar39Age((Xtra[i]),ArArJ,ArArJ1sig))/1.0e6;
      tUpper := (Ar40Ar39Age((Xtra[i]+Xtra1[i]),ArArJ,ArArJ1sig))/1.0e6;
      tLower := (Ar40Ar39Age((Xtra[i]-Xtra2[i]),ArArJ,ArArJ1sig))/1.0e6;
      tUncert := 100.0*((tUpper+tLower)/(2.0))/tAge;
      if (tAge <> 0.0) then
      begin
        tXtra1 := 100.0*Xtra1[i]/Xtra[i];
        tXtra2 := 100.0*Xtra2[i]/Xtra[i];
        tUncert := 100.0*((Xtra1[i]+Xtra2[i])/(2.0))/Xtra[i];
      end;
      tUncert := Sqrt(tUncert*tUncert + tXtra3*tXtra3 + tXtra4*tXtra4);
      tUncert := tUncert*Xtra[i]/100.0;
      for j := 1 to Steps do begin
        X1 := StartAtX + 1.0*j*(EndAtX-StartAtX)/Steps;
        Spectrum[j] := Spectrum[j] + Gauss(X1,tAge,tUncert);
      end;
      tLower := tAge - tLower;
      if (tLower <= 0.0) then tLower := 0.0;
      tUpper := tUpper - tAge;
      if (tUpper <= 0.0) then tUpper := 0.0;
    end;
  end;
  temp := 0.0;
  for i := 1 to Steps do
  begin
    if (temp < Spectrum[i]) then temp := Spectrum[i];
  end;
  if (temp = 0.0) then temp := 1.0e-9;
  for i := 0 to Steps do
  begin
    x2 := StartAtX + 1.0*i*(EndAtX-StartAtX)/Steps;
    if (i > 0) then y2 := 100.0 * Spectrum[i]/(temp)
               else y2 := 100.0 * Spectrum[1]/temp;
    ChCum.Series[iCurveLine].AddXY(x2,y2);
  end;
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

procedure TfmPlatAr.bbStoreMdlClick(Sender: TObject);
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
  }
end;

procedure TfmPlatAr.UpdateRFlg(Sender: TObject);
var
  i      : integer;
  tmpStr : string[1];
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

procedure TfmPlatAr.bbUpdateClick(Sender: TObject);
var
  i : integer;
  tx, ty : double;
begin
  UpdateRFlg(Sender);
  ChPlat.Series[iDataIncluded].Clear;
  ChPlat.Series[iDataExcluded].Clear;
  with dmGdwtmp.cdsReg do
  begin
    First;
    i := 1;
    repeat
      if (dmGdwtmp.cdsRegRFlag.AsString = 'Y') then
      begin
        tx := dmGdwtmp.cdsRegXRatio.AsFloat;
        ty := dmGdwtmp.cdsRegYRatio.AsFloat;
        ChPlat.Series[iDataIncluded].AddXY(tx,ty);
      end else
      begin
        if (dmGdwtmp.cdsRegPFlag.AsString = 'Y') then
        begin
          tx := dmGdwtmp.cdsRegXRatio.AsFloat;
          ty := dmGdwtmp.cdsRegYRatio.AsFloat;
          ChPlat.Series[iDataExcluded].AddXY(tx,ty);
        end;
      end;
      Next;
      i := i + 1;
    until dmGdwtmp.cdsReg.EOF;
    First;
    for i := 1 to iRec-1 do
    begin
      Next;
    end;
  end;
end;

procedure TfmPlatAr.cbCurrentSampleClick(Sender: TObject);
begin
  if (cbCurrentSample.Checked) then
  begin
    ChPlat.Series[iCurrent].Visible := true;
  end
  else begin
    ChPlat.Series[iCurrent].Visible := false;
  end;
end;

procedure TfmPlatAr.cbLegendClick(Sender: TObject);
begin
  if (cbLegend.Checked) then
  begin
    ChPlat.Legend.Visible := true;
  end
  else begin
    ChPlat.Legend.Visible := false;
  end;
end;

end.


