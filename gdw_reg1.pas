unit gdw_reg1;

interface

uses
  Windows, Messages, System.SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, Printers, Grids, DBGrids, DBCtrls,
  System.UITypes,
  OleCtrls, Mask, TeEngine, TeeProcs, Chart, TeeEdit, TeeComma,
  VCL.FlexCel.Core, FlexCel.XlsAdapter, FlexCel.Render, FlexCel.Preview,
  VclTee.TeeGDIPlus, VCLTee.TeeErrorPoint, VCLTee.Series, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, TeeTree, SVGIconVirtualImageList,
  Vcl.Touch.GestureMgr;

type
  TfmRegressionResult = class(TForm)
    eTitle: TEdit;
    Panel1: TPanel;
    eCentroidStr: TLabel;
    eSlopeStr: TLabel;
    eInterceptStr: TLabel;
    eCentroidX: TEdit;
    eSlope: TEdit;
    eIntercept: TEdit;
    lCentroidX: TLabel;
    lCentroidY: TLabel;
    eCentroidY: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    eInterceptErr: TEdit;
    eSlopeErr: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    PanelDate: TPanel;
    lRoStr: TLabel;
    lEpsilonStr: TLabel;
    lDateStr: TLabel;
    eDate: TEdit;
    eRo: TEdit;
    eEpsilon: TEdit;
    lMuErrPlusOnly: TLabel;
    lRoErrPlusOnly: TLabel;
    lDateErrPlusOnly: TLabel;
    eDateErr: TEdit;
    eRoErr: TEdit;
    eEpsilonErr: TEdit;
    lEpsilon95Percent: TLabel;
    lRo95Percent: TLabel;
    lDate95Percent: TLabel;
    Panel3: TPanel;
    eMSWDStr: TLabel;
    eMSWD: TEdit;
    lMSWDon: TLabel;
    eNumberOfPointsRegressed: TEdit;
    lAugmented: TLabel;
    lIsochronErrorchron: TLabel;
    Panel4: TPanel;
    eIterationStr: TLabel;
    pButtonsTop: TPanel;
    sbRegressionResult: TStatusBar;
    lFCrit: TLabel;
    lMSWDForced: TLabel;
    lDateErrMinusOnly: TLabel;
    eDateErrMinus: TEdit;
    lMuErrMinusOnly: TLabel;
    lRoErrMinusOnly: TLabel;
    eRoErrMinus: TEdit;
    eMuErrMinus: TEdit;
    lDateMinusErrAdjusted: TLabel;
    lDateAdjusted: TLabel;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintDialog1: TPrintDialog;
    lConstrain: TLabel;
    eXConstrain: TEdit;
    lConstrainAnd: TLabel;
    eYConstrain: TEdit;
    lConstrainNear: TLabel;
    eConstrainAge: TEdit;
    lConstrainMa: TLabel;
    pGraphics: TPanel;
    pResiduals: TPanel;
    dbnReg: TDBNavigator;
    lResidual: TLabel;
    cbRegressionLine: TCheckBox;
    cbErrorEllipses: TCheckBox;
    cbErrorEnvelope: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    Label13: TLabel;
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
    lMSWDof: TLabel;
    eNumberOfPoints: TEdit;
    lProbabilityOfFit: TLabel;
    eProbabilityOfFit: TEdit;
    lEllipseMagnif: TLabel;
    bbReregress: TBitBtn;
    eDateAdjusted: TEdit;
    eDatePlusErrAdjusted: TEdit;
    eDateMinusErrAdjusted: TEdit;
    pLeft: TPanel;
    Panel8: TPanel;
    pRight: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pCheckBoxes: TPanel;
    ChartReg: TChart;
    Series2: TPointSeries;
    Series1: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    Series6: TPointSeries;
    Series7: TPointSeries;
    Series8: TLineSeries;
    Series9: TLineSeries;
    Series10: TPointSeries;
    cbCurrentSample: TCheckBox;
    cbLegend: TCheckBox;
    ChartEditor1: TChartEditor;
    TeeCommanderReg: TTeeCommander;
    SaveDialogSprdSheet: TSaveDialog;
    cbTicLabels: TCheckBox;
    Series13: TLineSeries;
    Series14: TPointSeries;
    Series11: TPointSeries;
    Series12: TPointSeries;
    lTicksEvery: TLabel;
    eTicksEvery: TEdit;
    lModifyGraphSettings: TLabel;
    lMaxAgeConcordia: TLabel;
    eMaxAgeConcordia: TEdit;
    pTreeSmp: TPanel;
    pGraphicsResiduals: TPanel;
    Splitter4: TSplitter;
    TreeView1: TTreeView;
    bClose: TButton;
    bSpreadSheet: TButton;
    bExport: TButton;
    bReRegress: TButton;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    lDatePlusErrAdjusted: TLabel;
    lDateErrPlusOnlyIncl: TLabel;
    eDateErrIncl: TEdit;
    lDateErrMinusOnlyIncl: TLabel;
    eDateErrMinusIncl: TEdit;
    lDate95PercentIncl: TLabel;
    lDatePlusErrAdjustedIncl: TLabel;
    eDatePlusErrAdjustedIncl: TEdit;
    lDateMinusErrAdjustedIncl: TLabel;
    eDateMinusErrAdjustedIncl: TEdit;
    lDateAdjustedIncl: TLabel;
    lDateDCincl: TLabel;
    lDateDCexcl: TLabel;
    lDatePlusMinus: TLabel;
    lDatePlusMinusIncl: TLabel;
    lRoPlusMinus: TLabel;
    lEpsilonPlusMinus: TLabel;
    Series15: TLineSeries;
    Series16: TLineSeries;
    cbConcordiaUncertainties: TCheckBox;
    lAdditionalStr: TLabel;
    lLwrDateDCincl: TLabel;
    lLwrDatePlusErrIncl: TLabel;
    lLwrDatePlusErrAdjustedIncl: TLabel;
    eLwrDatePlusErrAdjustedIncl: TEdit;
    eLwrDatePlusErrIncl: TEdit;
    lLwrDateMinusErrAdjustedIncl: TLabel;
    lLwrDateMinusErrIncl: TLabel;
    eLwrDateMinusErrIncl: TEdit;
    eLwrDateMinusErrAdjustedIncl: TEdit;
    lLwrDate95PercentIncl: TLabel;
    lLwrDateAdjustedIncl: TLabel;
    eDateIncl: TEdit;
    eDateAdjustedIncl: TEdit;
    eLwrDateIncl: TEdit;
    eLwrDateAdjustedIncl: TEdit;
    procedure bbCloseClick(Sender: TObject);
    procedure bbSpreadSheetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dbnRegClick(Sender: TObject; Button: TNavigateBtn);
    procedure VtChartRegClick(Sender: TObject);
    procedure VtChartRegPointSelected(Sender: TObject; var Series,
      DataPoint, MouseFlags, Cancel: Smallint);
    procedure cbRegressionLineClick(Sender: TObject);
    procedure cbErrorEllipsesClick(Sender: TObject);
    procedure cbErrorEnvelopeClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbReRegressClick(Sender: TObject);
    procedure bbUpdateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbCurrentSampleClick(Sender: TObject);
    procedure cbLegendClick(Sender: TObject);
    procedure cbTicLabelsClick(Sender: TObject);
    procedure bbExporttoXMLClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure ChartRegClick(Sender: TObject);
    procedure ChartRegMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ChartRegDblClick(Sender: TObject);
    procedure cbConcordiaUncertaintiesClick(Sender: TObject);
    procedure ChartRegZoom(Sender: TObject);
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
    //  8 = Wetherill
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
    ChartRow : integer;
    RowNumber : integer;
    NumberOfGraphRows : integer;
    SprdSheet : TXlsFile;
  public
    { Public declarations }
    procedure CalcTMult (N : double);
    procedure CalcWeight;
    procedure Regress( var Lud_pp : double);
    procedure RegressModel ( var Lud_pp : double);
    procedure CalcAgeError;
    procedure Scrn_Results;
    procedure CheckConstrain;
    procedure Regress_Data;
    procedure FillRegTable(Sender: TObject);
    procedure UpdateRFlg(Sender: TObject);
    procedure SetAllChartData;
    procedure DrawEllipse ( i : integer);
    procedure SprdSheetEllipse (Xls : TExcelFile; i : integer);
    procedure HideResultLabels;
  end;

var
  fmRegressionResult: TfmRegressionResult;

implementation

uses
  allsorts, dmGdtmpDB, gdw_varb, gdw_regp, GDErnOpt, Gd_cwt, Gd_cnstr,
  Gd_AxVl, Gd_sht, dmGdMSWD, mathproc,
  TeePNG, TeeSVGCanvas, VCLTee.TeeThemes,
  TeeJPEG, TeExport;

{$R *.DFM}

var
  MinX, MaxX, MinY, MaxY  : double;
  iRec, iRecCount         : integer;
  ConstrainForm           : TfmConstrain;
  ConcordiaWtTypeForm     : TfmConcordiaWtType;
  GetErrorchronOptionForm : TfmGetErrorchronOption;
  GetAxisValuesForm       : TfmAxOpt;
  SprdSheetForm           : TfmSheet;

procedure TfmRegressionResult.bbCloseClick(Sender: TObject);
begin
  try
    UpdateRFlg(Sender);
    with dmGdwtmp.cdsReg do
    begin
      Active := false;
    end;
  finally
    Close;
  end;
end;

procedure TfmRegressionResult.UpdateRFlg(Sender: TObject);
var
  i      : integer;
  tmpStr : string;
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

procedure TfmRegressionResult.CalcTMult (N : double);
begin
  T_Mult:=TMultiplier(N);
end;{CalcTMult}

procedure TfmRegressionResult.CalcWeight;
var
  J           : integer;
begin
  FillChar(Weight,SizeOf(Weight),0);
  FillChar(Wt,SizeOf(Weight),0);
  for J:=1 to NumberOfPoints do begin
    case ErrTyp[J] of
      '1' : begin
              Weight[J,1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Weight[J,2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '2' : begin
              Weight[J,1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Weight[J,2]:=ErrorWt[J,2];
            end;
      '3' : begin
              Weight[J,1]:=ErrorWt[J,1];
              Weight[J,2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '4' : begin
              Weight[J,1]:=ErrorWt[J,1];
              Weight[J,2]:=ErrorWt[J,2];
            end;
    end;{case}
    if (Weight[J,1]<=0.0) then Weight[J,1]:=1.0e-5;
    if (Weight[J,2]<=0.0) then Weight[J,2]:=1.0e-5;
    Wt[J,1]:=Weight[J,1];
    Wt[J,2]:=Weight[J,2];
    case Model of
      1,4,5,6 : begin
        Weight[J,1]:=1.0/(Weight[J,1]*Weight[J,1]);
        Weight[J,2]:=1.0/(Weight[J,2]*Weight[J,2]);
      end;
      2 : begin
        Weight[J,1]:=1.0;
        Weight[J,2]:=1.0/(Slope*Slope);
      end;
      3 : begin
      end;
    end;
  end;//for
end;{procedure CalcWeight}

procedure TfmRegressionResult.Regress( var Lud_pp : double);
var
  Al, D1, D2, D3, Difference,
  temp, temp1,
  tx, ty, C, T1     :  double;
  Iteration_Str     :  string; //string[3]
  Lud_pp_Str        :  string;  //string[5]
  Reg_done          :  Boolean;
  k, J              : integer;
  MSWD_Str          : string;  //string[6]
  Slope_Str         : string; //string[10]
  MaxX              : double;
begin
  Iteration:=0;
  if Model=4 then Iteration:=1;
  Reg_done:=false;
  Converg_done:=false;
  case Model of
    1,5,6 : begin
      for J:=1 to NumberofPoints do begin
        RR[J]:=R[J];
      end;
    end;
    2 : begin
      for J:=1 to NumberofPoints do begin
        RR[J]:=0.0;
      end;
    end;
  end;
  if (AnalType8='U') or (AnalType8='L') then begin
    Str(Lud_pp:6:3,Lud_pp_Str);
    sbRegressionResult.Panels[1].Text := 'Discordance factor p   '+Lud_pp_Str;
  end
  else begin
    sbRegressionResult.Panels[1].Text := '';
  end;
  sbRegressionResult.Update;
  //ShowMessage('1');
  repeat
    Iteration:=Iteration+1;
    TempSlope:=Slope;
    Str(Iteration:3,Iteration_Str);
    sbRegressionResult.Panels[0].Text := 'Iteration '+Iteration_Str;
    if (Model in [2,3,4,5]) then
    begin
      Str(Msum:8:3,MSWD_Str);
      sbRegressionResult.Panels[2].Text := 'MSWD = '+MSWD_Str;
    end;
    if (Model in [1]) then
    begin
      Str(Slope:10:6,Slope_Str);
      sbRegressionResult.Panels[3].Text := 'Slope = '+Slope_Str;
    end;
    sbRegressionResult.Refresh;
    FillChar(Sum,SizeOf(Sum),0);
    FillChar(U,SizeOf(U),0);
    FillChar(V,SizeOf(V),0);
    FillChar(Z,SizeOf(Z),0);
    if (Iteration>1) then
    begin
      if CharInSet(AnalType8,['U','L']) then
      begin
        Get_NewSlope;
        if CharInSet(AnalType,['8']) then
        begin
          temp:=ConcordiaIntercept(5.0e9,NewSlope,NewIntercept);
          temp1:=ConcordiaIntercept(-1.0e9,NewSlope,NewIntercept);
          X_Uint:=Exp(DecayConst[5]*temp)-1;
          X_Lint:=Exp(DecayConst[5]*temp1)-1;
        end;
        if CharInSet(AnalType,['A']) then
        begin
          temp:=TeraWasserburgIntercept(5.0e9,NewSlope,NewIntercept);
          temp1:=TeraWasserburgIntercept(1.0e6,NewSlope,NewIntercept);
          X_Uint:=1.0/(Exp(DecayConst[ord(at238UPb)]*temp)-1);
          X_Lint:=1.0/(Exp(DecayConst[ord(at238UPb)]*temp1)-1);
          {
          MaxX := 0.0;
          for k := 1 to NumberOfPoints do
          begin
            if (RFlg[k] = 'Y') then
            begin
              if (MaxX < Ratio[k,1]) then MaxX := Ratio[k,1];
            end;
          end;
          X_Lint := MaxX;
          }
        end;
        Chordlength:=Abs(X_Uint-X_Lint);
        if (Chordlength=0.0) then Chordlength:=1.0e-6;
        CalcWeight;
      end;
    end;
    for J:=1 to NumberOfPoints do
    begin
      if CharInSet(AnalType8,['U']) and (Iteration>1) then
      begin
        if (AnalType = '8') then Fd:=Abs(X_Uint-Ratio[J,1]);
        if (AnalType = 'A') then Fd:=Abs(X_Uint-Ratio[J,1]);
      end;
      if CharInSet(AnalType8,['L']) and (Iteration>1) then
      begin
        Fd:=Abs(X_Lint-Ratio[J,1]);
      end;
      if CharInSet(AnalType8,['U','L']) and (Iteration>1) then
      begin
        Fd:=Fd/Chordlength;
        if (Fd<>1.0) then Fm:=Fd/(1.0-Fd)
                     else Fm:=1.0e9;
        Ex:=Wt[J,1]*100.0/Ratio[J,1];
        Ey:=Wt[J,2]*100.0/Ratio[J,2];
        E76:=Sqrt(Ex*Ex+Ey*Ey-2.0*R[J]*Ex*Ey);
        Ex:=Sqrt(Ex*Ex+Lud_pp*Lud_pp*Fm*Fm);
        Ey:=Sqrt(Ey*Ey+Lud_pp*Lud_pp*Fm*Fm);
        V76:=E76*E76+((Lud_pp*Fm*1.0)*(Lud_pp*Fm*1.0));
        RR[J]:=(Ex*Ex+Ey*Ey-V76)/(2.0*Ex*Ey);
        Wt[J,1]:=Ex*Ratio[J,1]/100.0;
        Wt[J,2]:=Ey*Ratio[J,2]/100.0;
        Weight[J,1]:=1.0/(Wt[J,1]*Wt[J,1]);
        Weight[J,2]:=1.0/(Wt[J,2]*Wt[J,2]);
        NEquivPtsRegressed:=1.0*NumberOfPointsRegressed;
      end;
      if ((Iteration>1) and (Model=2)) then
      begin
        Weight[J,1]:=1.0;
        Weight[J,2]:=1.0/(TempSlope*TempSlope);
      end;
      Al:=Weight[J,1]*Weight[J,2];                       {Problem shown at next line}
      Z[J]:=Al/(TempSlope*TempSlope*Weight[J,2]+Weight[J,1]
            -2.0*TempSlope*RR[J]*Sqrt(Al));
      if (UpCase(RFlg[J])='Y') then
      begin
        Sum[1]:=Sum[1]+Z[J]*Ratio[J,1];
        Sum[2]:=Sum[2]+Z[J]*Ratio[J,2];
        Sum[3]:=Sum[3]+Z[J];
      end;//if
    end;//for
    Xcentroid:=Sum[1]/Sum[3];
    Ycentroid:=Sum[2]/Sum[3];
    for J:=1 to NumberOfPoints do
    begin
      if ConstrainFlag then
      begin
        U[J]:=Ratio[J,1]-XConstrain;
        V[J]:=Ratio[J,2]-YConstrain;
      end
      else begin
        U[J]:=Ratio[J,1]-Xcentroid;
        V[J]:=Ratio[J,2]-Ycentroid;
      end;
      if (UpCase(RFlg[J])='Y') then
      begin
        Al:=Weight[J,1]*Weight[J,2];
        D1:=U[J]/Weight[J,2]+TempSlope*V[J]/Weight[J,1];
        D2:=RR[J]*V[J]/Sqrt(Al);
        D3:=RR[J]*U[J]/Sqrt(Al);
        Z[J]:=Al/(TempSlope*TempSlope*Weight[J,2]+Weight[J,1]
              -2.0*TempSlope*RR[J]*Sqrt(Al));
        Sum[4]:=Sum[4]+Z[J]*Z[J]*V[J]*(D1-D2);
        Sum[5]:=Sum[5]+Z[J]*Z[J]*U[J]*(D1-TempSlope*D3);
      end;//if
    end;//for
    Slope:=Sum[4]/Sum[5];
    Difference:=Abs(Slope-TempSlope);
    if Iteration>MaxIteration then begin
      Reg_done:=true;
      Converg_done:=false;
    end;
    if Difference<SlopeTolerance then begin
      Reg_done:=true;
      Converg_done:=true;
    end;
    if ConstrainFlag then
    begin
      Intercept:=YConstrain-Slope*XConstrain;
    end
    else begin
      Intercept:=Ycentroid-Slope*Xcentroid;
    end;
  until Reg_done;
  FillChar(Sum,SizeOf(Sum),0);
  FillChar(Residual,SizeOf(Residual),0);
  SST:=0.0;
  SSR:=0.0;
  SSD:=0.0;
  SSLF:=0.0;
  SSPE:=0.0;
  for J:=1 to NumberOfPoints do begin
      Al:=Weight[J,1]*Weight[J,2];
      if Al=0.0 then Al:=1.0E-9;
      C:=RR[J]*Sqrt(Al);
      T1:=Z[J]*(Intercept+Slope*Ratio[J,1]-Ratio[J,2]);
      Residual[J,1]:=T1*(C-Slope*Weight[J,2])/Al;
      Residual[J,2]:=T1*(Weight[J,1]-Slope*C)/Al;
      if (UpCase(RFlg[J])='Y') then begin
        Sum[1]:=Sum[1]+Z[J]*(Ratio[J,2]-Slope*Ratio[J,1]-Intercept)
                           *(Ratio[J,2]-Slope*Ratio[J,1]-Intercept);
        Sum[2]:=Sum[2]+Z[J]*Ratio[J,1]*Ratio[J,1];
        Sum[3]:=Sum[3]+Z[J];
        tx:=Ratio[J,1]+Residual[J,1];
        Sum[4]:=Sum[4]+Z[J]*tx;
        Sum[5]:=Sum[5]+Z[J]*tx*tx;
        SST:=SST+Z[J]*U[J]*U[J];
      end;
      if (ErrTyp[J]<>'4') then begin
        case (ErrTyp[J]) of
          '1' : begin
                  if Ratio[J,1]<>0.0 then
                    Residual[J,1]:=Residual[J,1]*100.0/Ratio[J,1];
                  if Ratio[J,2]<>0.0 then
                    Residual[J,2]:=Residual[J,2]*100.0/Ratio[J,2];
                end;
          '2' : if Ratio[J,1]<>0.0 then
                  Residual[J,1]:=Residual[J,1]*100.0/Ratio[J,1];
          '3' : if Ratio[J,2]<>0.0 then
                  Residual[J,2]:=Residual[J,2]*100.0/Ratio[J,2];
        end;//case
      end;
  end;//for
  if NumberOfPointsRegressed>2 then Msum:=Sum[1]/(NumberOfPointsRegressed-2)
                               else Msum:=0.0;
  if (Sum[3]/(Sum[5]*Sum[3]-Sum[4]*Sum[4]))<>0.0 then begin
    SlopeError:=Sqrt(Sum[3]/(Sum[5]*Sum[3]-Sum[4]*Sum[4]));
    InterceptError:=Sqrt(Sum[5]/(Sum[5]*Sum[3]-Sum[4]*Sum[4]));
  end
  else begin
    SlopeError:=0.0;
    InterceptError:=0.0;
  end;
  sbRegressionResult.Panels[3].Text := ' ';
end;{procedure Regress}

procedure TfmRegressionResult.RegressModel ( var Lud_pp : double);
var
  ModelOption   : char;
  Msum_Str      : string;
  CritF_Str     : string;
  temp, temp1,
  temp2,
  tempm, tempse,
  tempie        : double;
  RegCnt        : integer;
  J             : integer;
begin
  CalcTMult(1.0*N_Rep);
  Model:=1;
  CalcWeight;
  Regress(Lud_pp);
  if Msum>Msumcutoff then
  begin
    try
      GetErrorchronOptionForm := TfmGetErrorchronOption.Create(Self);
      ModelOption:='?';
      if CharInSet(AnalType,['1','2','4'..'7','9','B','C','D','E','F','G']) then
      //if (Analtype in ['1','2','4'..'7','9','B','C','D','E','F','G']) then
        GetErrorchronOptionForm.rbbVarInit.Visible := true
      else
        GetErrorchronOptionForm.rbbVarInit.Visible := false;
      if CharInSet(AnalType,['8','A']) then
      //if (Analtype in ['8','A']) then
      begin
        GetErrorchronOptionForm.rbbUprItcpt.Visible := true;
        GetErrorchronOptionForm.rbbLwrItcpt.Visible := true;
      end else
      begin
        GetErrorchronOptionForm.rbbUprItcpt.Visible := false;
        GetErrorchronOptionForm.rbbLwrItcpt.Visible := false;
      end;
      Str(Msum:7:3,Msum_Str);
      Str(MsumCutoff:5:2,CritF_Str);
      GetErrorchronOptionForm.eMSWD.Text := Msum_Str;
      GetErrorchronOptionForm.eCritF.Text := CritF_Str;
      GetErrorchronOptionForm.ShowModal;
      case Model of
        1 : begin  //augment by sqrt(MSWD)
         CalcTMult(1.0*N_Rep);
         if Msum>MsumCutOff then begin
            SlopeError:=SlopeError*Sqrt(Msum/MsumCutOff);
            InterceptError:=InterceptError*Sqrt(Msum/MsumCutOff);
          end;
        end;
        2 : begin  //no assumptions for scatter
           if (AnalType8<>'N') then AnalType8:='N';
           CalcTMult(1.0*(NumberOfPointsRegressed-2));
           tempm:=Msum;
           tempse:=SlopeError;
           tempie:=InterceptError;
           Lud_pp:=1.0;
           CalcWeight;
           Regress(Lud_pp);
           if ((Msum < 1.0) and (Msum > 0.0)) then Msum:=1.0/Msum;
           if Msum > 1.0 then
           begin
             SlopeError:=SlopeError/Sqrt(Msum);
             InterceptError:=InterceptError/Sqrt(Msum);
           end;
           Msum:=1.0;
        end;
        3 : begin   //assume variable initial ratios
           if (AnalType8<>'N') then AnalType8:='N';
           CalcTMult(1.0*(NumberOfPointsRegressed-2));
           RegCnt:=0;
           temp2:=InterceptError;
           CalcWeight;
           repeat
             RegCnt:=RegCnt+1;
             temp1:=temp2*Sqrt(Msum/MsumCutOff);
             temp2:=temp1;
             temp1:=temp1*temp1;
             for J:=1 to NumberofPoints do
             begin
               Weight[J,1]:=Wt[J,1];
               Weight[J,1]:=1.0/(Weight[J,1]*Weight[J,1]);
               temp:=Wt[J,2]*Wt[J,2];
               Weight[J,2]:=1.0/(temp+temp1);
               RR[J]:=R[J]*Sqrt(temp/(temp+temp1));
             end;
             Regress(Lud_pp);
           until (((Msum < (MsumCutOff+0.005)) and (Msum > (MsumCutoff-0.005))) or (RegCnt >15));
           if ((Msum < 1.0) and (Msum > 0.0)) then Msum:=1.0/Msum;
           if Msum>1.0 then
           begin
             SlopeError:=SlopeError*Sqrt(Msum/MsumCutoff);
             InterceptError:=InterceptError*Sqrt(Msum/MsumCutOff);
           end;
           Msum:=MsumCutOff;
        end;
        4 : begin  //Ludwig discordance weighting
           Lud_pp := 0.0001;
           CalcTMult(1.0*(NumberOfPointsRegressed-2));
           RegCnt:=0;
           SlopeTolerance:=SlopeTolerance*500.0;
           if (AnalType8 <> 'N') then
           begin
             Lud_pp:=0.2;
           end;
           CalcWeight;
           //ShowMessage('1 '+IntToStr(RegCnt)+'  '+FormatFloat('###0.000',Lud_pp)+'  '+FormatFloat('######0.0000',Msum)+'  '+FormatFloat('######0.0000',MsumCutOff));
           Regress(Lud_pp);
           repeat
             RegCnt:=RegCnt+1;
             Lud_pp:=Lud_pp*Sqrt(Msum/MsumCutOff)+0.0001;
             CalcWeight;
             //ShowMessage('2 '+IntToStr(RegCnt)+'  '+FormatFloat('###0.000',Lud_pp)+'  '+FormatFloat('######0.0000',Msum)+'  '+FormatFloat('######0.0000',MsumCutOff));
             Regress(Lud_pp);
           until ((Abs(Msum-MsumCutoff) < 0.01) or (RegCnt > 20));
           SlopeTolerance:=SlopeTolerance/50.0;
           repeat
             RegCnt:=RegCnt+1;
             Lud_pp:=Lud_pp*Sqrt(Msum/MsumCutOff)+0.0001;
             CalcWeight;
             //ShowMessage('3 '+IntToStr(RegCnt)+'  '+FormatFloat('###0.000',Lud_pp)+'  '+FormatFloat('######0.0000',Msum)+'  '+FormatFloat('######0.0000',MsumCutOff));
             Regress(Lud_pp);
           until ((Abs(Msum-MsumCutoff) < 0.01) or (RegCnt > 40));
           SlopeTolerance:=SlopeTolerance/10.0;
           repeat
             RegCnt:=RegCnt+1;
             Lud_pp:=Lud_pp*Sqrt(Msum/MsumCutOff)+0.0001;
             CalcWeight;
             //ShowMessage('4 '+IntToStr(RegCnt)+'  '+FormatFloat('###0.000',Lud_pp)+'  '+FormatFloat('######0.0000',Msum)+'  '+FormatFloat('######0.0000',MsumCutOff));
             Regress(Lud_pp);
           until ((Abs(Msum-MsumCutoff) < 0.01) or (RegCnt > 70));
           //ShowMessage(FormatFloat('###0.0000',Lud_pp));
        end;
        5 : begin  //assume separate analytical and geological errors
           CalcTMult(1.0*N_Rep);
           {
           SlopeError:=SlopeError;
           InterceptError:=InterceptError;
           }
        end;
        6 : begin //do not augment errors
           CalcTMult(1.0*N_Rep);
           SlopeError:=SlopeError;
           InterceptError:=InterceptError;
        end;
      end;
    finally
      GetErrorchronOptionForm.Free;
    end;
  end;
  if CharInSet(AnalType8,['U','L']) and (Model in [4]) then
  //if ((AnalType8 in ['U','L']) and (Model in [4])) then
  begin
    NEquivPtsRegressed:=1.0;
    temp:=-9.0e27;
    for J:=1 to NumberofPoints do
    begin
      if (RFlg[J] = 'Y') then
      begin
        if (Z[J] > temp) then temp:=Z[J];
      end;
    end;
    temp1:=0.0;
    for J:=1 to NumberofPoints do
    begin
      if (RFlg[J] = 'Y') then
      begin
        if ((Z[J] > temp1) and (Z[J] < temp)) then temp1:=Z[J];
      end;
    end;
    for J:=1 to NumberofPoints do
    begin
      if (RFlg[J] = 'Y') then
      begin
        if (Z[J] < temp) then
           NEquivPtsRegressed:=NEquivPtsRegressed + Z[J]/temp1;
      end;
    end;
    CalcTMult(1.0*NEquivPtsRegressed-2.0);
  end;
end;//RegressModel

procedure TfmRegressionResult.CalcAgeError;
var
  t1, t2, t3       : double;
  //AgeErrorPlusIncl, AgeErrorMinusIncl : double;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  //ShowMessage(IntToStr(IAnalTyp));
  iAnalTyp := Get_IAnal_from_AnalType(AnalType);
  //ShowMessage(IntToStr(IAnalTyp));
    case AnalType of
      '0' : begin //general
            end;
      '1','2','7' : begin //Rb-Sr, Sm-Nd, Lu-Hf
              if Slope>-1.0 then Age := (ln(1.0+Slope))/DecayConst[iAnalTyp]
                            else Age := 0.0;
              AgeError := (ln(1.0+T_Mult*SlopeError))/DecayConst[iAnalTyp];
              if (Model <> 5) then
              begin
                AgeError := ln(1.0+T_Mult*SlopeError)/DecayConst[iAnalTyp];
                if (DecayConstUncertainty[iAnalTyp] > 0.0) then
                begin
                  AgeErrorPlusIncl := (ln(1.0 + T_Mult*SlopeError))/(DecayConst[iAnalTyp]-T_Mult*DecayConst[iAnalTyp]*DecayConstUncertainty[iAnalTyp]/100.0);
                  AgeErrorMinusIncl := (ln(1.0 + T_Mult*SlopeError))/(DecayConst[iAnalTyp]+T_Mult*DecayConst[iAnalTyp]*DecayConstUncertainty[iAnalTyp]/100.0);
                end;
                InitRatioError:=InterceptError*T_Mult;
                t1:=Intercept;
                t2:=CHUR[iAnalTyp,2]-CHUR[iAnalTyp,1]*
                    (Exp(DecayConst[iAnalTyp]*Age)-1.0);
                Epsilon1:=10000.0*(t1/t2-1.0);
                EpError1:=T_Mult*10000.0*InterceptError/Intercept;
              end;
              if Model=5 then
              begin
                t1:=SlopeError*Sqrt(Msum/MsumCutOff-1.0);
                t2:=t1*TMultiplier(NumberOfPointsRegressed-2);
                t2:=t2*t2;
                t1:=SlopeError*TMultiplier(N_Rep);
                t2:=Sqrt(t2+t1*t1);
                AgeError:=Ln(1.0+t2)/DecayConst[iAnalTyp];
                t1:=InterceptError*Sqrt(Msum/MsumCutOff-1.0);
                t2:=t1*TMultiplier(NumberOfPointsRegressed-2);
                t2:=t2*t2;
                t1:=InterceptError*TMultiplier(N_Rep);
                t2:=Sqrt(t2+t1*t1);
                InitRatioError:=t2;
                t1:=Intercept;
                t2:=CHUR[iAnalTyp,2]-CHUR[iAnalTyp,1]*(Exp(DecayConst[iAnalTyp]*Age)-1.0);
                Epsilon1:=10000.0*(t1/t2-1.0);
                EpError1:=10000.0*InitRatioError/Intercept;
              end;
              Age:=Age/1.0e6;
              AgeError:=AgeError/1.0e6;
              AgeErrorPlusIncl:=AgeErrorPlusIncl/1.0e6;
              AgeErrorMinusIncl:=AgeErrorMinusIncl/1.0e6;
            end;
      '3' : begin //Pb-Pb
              IncludeDCUncertainty := false;
              AgePlusAgeMinus := 'neither';
              if Slope>0.0 then Age:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)
                           else Age:=0.0;
              UpperAgeError:=(PbPbAge(Slope+T_Mult*SlopeError,IncludeDCUncertainty,AgePlusAgeMinus)-Age);
              LowerAgeError:=(Age-PbPbAge(Slope-T_Mult*SlopeError,IncludeDCUncertainty,AgePlusAgeMinus));
              IncludeDCUncertainty := true;
              AgePlusAgeMinus := UncertaintyPlus;
              UpperAgeErrorIncl:=(PbPbAge(Slope+T_Mult*SlopeError,IncludeDCUncertainty,AgePlusAgeMinus)-Age);
              IncludeDCUncertainty := true;
              AgePlusAgeMinus := UncertaintyMinus;
              LowerAgeErrorIncl:=(Age-PbPbAge(Slope-T_Mult*SlopeError,IncludeDCUncertainty,AgePlusAgeMinus));
              if ((Age>MuV[1,1]) and (mu_choice > 0)) then
              begin
                mu_choice:=0;
                MessageDlg('Date too old for model. Changing to single stage model',
                    mtWarning,[mbOK],0);
              end;
              MuErrors;
              Age:=Age*0.000001;
              UpperAgeError:=UpperAgeError*0.000001;
              LowerAgeError:=LowerAgeError*0.000001;
              UpperAgeErrorIncl:=UpperAgeError*0.000001;
              LowerAgeErrorIncl:=LowerAgeError*0.000001;
            end;
      '4'..'6','9','G' : //U-Pb, Th-Pb, Lu-Hf, La-Ce, La-Ba
            begin
              if Slope>-1.0 then Age:=Ln(1.0+Slope)/DecayConst[iAnalTyp]
                            else Age:=0.0;
              AgeError := (ln(1.0+T_Mult*SlopeError))/DecayConst[iAnalTyp];
              if (DecayConstUncertainty[iAnalTyp] > 0.0) then
              begin
                AgeErrorPlusIncl := (ln(1.0 + T_Mult*SlopeError))/(DecayConst[iAnalTyp]-T_Mult*DecayConst[iAnalTyp]*DecayConstUncertainty[iAnalTyp]/100.0);
                AgeErrorMinusIncl := (ln(1.0 + T_Mult*SlopeError))/(DecayConst[iAnalTyp]+T_Mult*DecayConst[iAnalTyp]*DecayConstUncertainty[iAnalTyp]/100.0);
              end;
              Age:=Age/1.0e6;
              AgeError:=AgeError/1.0e6;
              AgeErrorPlusIncl:=AgeErrorPlusIncl/1.0e6;
              AgeErrorMinusIncl:=AgeErrorMinusIncl/1.0e6;
              InitRatioError:=InterceptError*T_Mult;
            end;
      '8' : begin //conventional Concordia
               NewConcordiaErrors;
            end;
      'A' : begin //Tera-Wasserburg
              NewTeraWasserburgErrors;
            end;
      'B' : begin //K-Ar
              if Slope>-1.0 then Age:=Ln(1.0+Slope*(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])
                    /DecayConst[ord(atKAr)])/(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])
                            else Age:=0.0;
              AgeError:=Ln(1.0+T_Mult*SlopeError*(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])
                    /DecayConst[ord(atKAr)])/(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)]);
              Age:=Age*0.000001;
              AgeError:=AgeError*0.000001;
              InitRatioError:=InterceptError*T_Mult;
            end;
      'C' : begin     //Ar-Ar
              ArArJ := Ratio[1,3];
              ArArJ1sig := ErrorWt[1,3];
              //ShowMessage('ArArJ1sig = '+FormatFloat('#####0.0000000000',ArArJ1sig));
              // if value for Ar-Ar J value uncertainty is too large to be actual
              // value, then assume it is percentage and convert to actual;
              if (ArArJ1sig > 0.0001) then ArArJ1sig := ArArJ1sig*ArArJ/100.0;
              //ShowMessage('ArArJ = '+FormatFloat('#####0.0000000000',ArArJ));
              //ShowMessage('ArArJ1sig = '+FormatFloat('#####0.0000000000',ArArJ1sig));
              //ShowMessage('DecayConstTot = '+FormatFloat('#####0.0000000000',1e6*(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])));
              if (ArArJ > 0.0) then
              begin
                if Slope>-1.0 then Age:=Ar40Ar39Age(Slope,ArArJ,ArArJ1sig)
                              else Age:=0.0;
                //if Slope>-1.0 then Age:=Ln(1.0+Slope*ArArJ)/(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])
                //              else Age:=0.0;
                //ShowMessage('Slope = '+FormatFloat('#####0.0000000000',Slope));
                //ShowMessage('Age = '+FormatFloat('#####0.0000000000',Age/1.0e6));
                AgeError:= Ar40Ar39Age(Slope+SlopeError*T_Mult,ArArJ,ArArJ1sig)-Age;
                //AgeError:=Ln(1.0+T_Mult*SlopeError*ArArJ)/(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)]);
                //ShowMessage('SlopeError = '+FormatFloat('#####0.0000000000',SlopeError));
                //ShowMessage('AgeError = '+FormatFloat('#####0.0000000000',AgeError/1.0e6));
                UprUprAgeErrorIncl := -1.0e6;
                if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
                begin
                  UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(Slope+SlopeError*T_Mult,ArArJ,ArArJ1sig,T_Mult)-Age;
                end;
                Age:=Age*0.000001;
                AgeError:=AgeError*0.000001;
                InitRatioError:=InterceptError*T_Mult;
                UprUprAgeErrorIncl := UprUprAgeErrorIncl/1.0e6;
              end else
              begin
                MessageDlg('J constant must be defined in the third variable of the first sample',mtInformation,[mbOK],0);
              end;
            end;
      'D' : begin //Ar inverse
              ArArJ := Ratio[1,3];
              ArArJ1sig := ErrorWt[1,3];
              // if value for Ar-Ar J value uncertainty is too large to be actual
              // value, then assume it is percentage and convert to actual;
              if (ArArJ1sig > 0.0001) then ArArJ1sig := ArArJ1sig*ArArJ/100.0;
              if (Slope < 0.0) then
              begin
                XItcpt := (0.0-Intercept)/Slope;
              end;
              if (ArArJ > 0.0) then
              begin
                //if Slope<1.0 then Age:=Ln(1.0+ArArJ/XItcpt)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)])
                //             else Age:=0.0;
                if Slope>-1.0 then Age:=Ar40Ar39Age(1.0/XItcpt,ArArJ,ArArJ1sig)
                              else Age:=0.0;
                UprUprAgeError := CalcArInverseError(-1,0.0,Slope,SlopeError*T_Mult,
                                       Intercept,InterceptError*T_Mult);
                if UprUprAgeError>0.0 then
                begin
                  UprUprAgeError:=Ar40Ar39Age(1.0/UprUprAgeError,ArArJ,ArArJ1sig)-Age;
                  //UprUprAgeError:=Ln(1.0+ArArJ/UprUprAgeError)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)])-Age;
                end else
                begin
                  UprUprAgeError:=0.0;
                end;
                UprLwrAgeError := CalcArInverseError(1,20.0,Slope,SlopeError*T_Mult,
                                       Intercept,InterceptError*T_Mult);
                if UprLwrAgeError>0.0 then
                begin
                  UprLwrAgeError:=Age-Ar40Ar39Age(1.0/UprLwrAgeError,ArArJ,ArArJ1sig);
                  //UprLwrAgeError:=Age-Ln(1.0+ArArJ/UprLwrAgeError)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)]);
                end else
                begin
                  UprLwrAgeError:=0.0;
                end;
                if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
                begin
                  UprUprAgeErrorIncl := CalcArInverseError(-1,0.0,Slope,SlopeError*T_Mult,
                                         Intercept,InterceptError*T_Mult);
                  //if UprUprAgeErrorIncl>0.0 then UprUprAgeErrorIncl:=Ln(1.0+ArArJ/UprUprAgeErrorIncl)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)])-Age
                  //                      else UprUprAgeErrorIncl:=0.0;
                  if UprUprAgeErrorIncl>0.0 then
                  begin
                    UprUprAgeErrorIncl:=Ar40Ar39AgeWithDCErr(1.0/UprUprAgeErrorIncl,ArArJ,ArArJ1sig,T_Mult)-Age;
                    //UprUprAgeError:=Ln(1.0+ArArJ/UprUprAgeError)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)])-Age;
                  end else
                  begin
                    UprUprAgeErrorIncl:=0.0;
                  end;
                  UprLwrAgeErrorIncl := CalcArInverseError(1,20.0,Slope,SlopeError*T_Mult,
                                         Intercept,InterceptError*T_Mult);
                  //if UprLwrAgeErrorIncl>0.0 then UprLwrAgeErrorIncl:=Age-Ln(1.0+ArArJ/UprLwrAgeErrorIncl)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)])
                  //                      else UprLwrAgeErrorIncl:=0.0;
                  if UprLwrAgeError>0.0 then
                  begin
                    UprLwrAgeErrorIncl:=Age-Ar40Ar39AgeWithDCErr(1.0/UprLwrAgeErrorIncl,ArArJ,ArArJ1sig,T_Mult);
                    //UprLwrAgeError:=Age-Ln(1.0+ArArJ/UprLwrAgeError)/(DecayConst[ord(atKCa)]+DecayConst[ord(atKAr)]);
                  end else
                  begin
                    UprLwrAgeErrorIncl:=0.0;
                  end;
                end;
                Age:=Age/1.0e6;
                UprUprAgeError := UprUprAgeError/1.0e6;
                UprLwrAgeError := UprLwrAgeError/1.0e6;
                InitRatioError:=InterceptError*T_Mult;
                UprUprAgeErrorIncl := UprUprAgeErrorIncl/1.0e6;
                UprLwrAgeErrorIncl := UprLwrAgeErrorIncl/1.0e6;
              end;
            end;
      'E' : begin //K-Ca
              MessageDlg('Not yet implemented',mtInformation,[mbOK],0);
              {
              if Slope>-1.0 then Age:=Ln(1.0+Slope)/DecayConst[iAnalTyp]
                            else Age:=0.0;
              AgeError:=Ln(1.0+T_Mult*SlopeError)/DecayConst[iAnalTyp];
              Age:=Age*0.000001;
              AgeError:=AgeError*0.000001;
              InitRatioError:=InterceptError*T_Mult;
              }
            end;
      'F' : begin //Re-Os
              if Slope>-1.0 then Age:=Ln(1.0+Slope)/DecayConst[iAnalTyp]
                            else Age:=0.0;
              AgeError := (ln(1.0+T_Mult*SlopeError))/DecayConst[iAnalTyp];
              if (Model <> 5) then
              begin
                if (DecayConstUncertainty[iAnalTyp] > 0.0) then
                begin
                  AgeErrorPlusIncl := (ln(1.0 + T_Mult*SlopeError))/(DecayConst[iAnalTyp]-T_Mult*DecayConst[iAnalTyp]*DecayConstUncertainty[iAnalTyp]/100.0);
                  AgeErrorMinusIncl := (ln(1.0 + T_Mult*SlopeError))/(DecayConst[iAnalTyp]+T_Mult*DecayConst[iAnalTyp]*DecayConstUncertainty[iAnalTyp]/100.0);
                end;
                InitRatioError:=InterceptError*T_Mult;
                t1:=Intercept;
                t2:=CHUR[iAnalTyp,2]-CHUR[iAnalTyp,1]*
                    (Exp(DecayConst[iAnalTyp]*Age)-1.0);
                Epsilon1:=100.0*(t1/t2-1.0);
                EpError1:=T_Mult*100.0*InterceptError/Intercept;
              end;
              if (Model = 5) then
              begin
                t1:=SlopeError*Sqrt(Msum/MsumCutOff-1.0);
                t2:=t1*TMultiplier(NumberOfPointsRegressed-2);
                t2:=t2*t2;
                t1:=SlopeError*TMultiplier(N_Rep);
                t2:=Sqrt(t2+t1*t1);
                AgeError:=Ln(1.0+t2)/DecayConst[iAnalTyp];
                t1:=InterceptError*Sqrt(Msum/MsumCutOff-1.0);
                t2:=t1*TMultiplier(NumberOfPointsRegressed-2);
                t2:=t2*t2;
                t1:=InterceptError*TMultiplier(N_Rep);
                t2:=Sqrt(t2+t1*t1);
                InitRatioError:=t2;
                t1:=Intercept;
                t2:=CHUR[iAnalTyp,2]-CHUR[iAnalTyp,1]*(Exp(DecayConst[iAnalTyp]*Age)-1.0);
                Epsilon1:=100.0*(t1/t2-1.0);
                EpError1:=100.0*InitRatioError/Intercept;
              end;
              Age:=Age/1.0e6;
              AgeError:=AgeError/1.0e6;
              AgeErrorPlusIncl:=AgeErrorPlusIncl/1.0e6;
              AgeErrorMinusIncl:=AgeErrorMinusIncl/1.0e6;
            end;
    end;//case
end;//CalcAgeError

procedure TfmRegressionResult.HideResultLabels;
begin
  PanelDate.Visible := false;
  lDateStr.Visible := false;
  eDate.Visible := false;
  lDatePlusMinus.Visible := false;
  lDateErrPlusOnly.Visible := false;
  eDateErr.Visible := false;
  lDateErrMinusOnly.Visible := false;
  eDateErrMinus.Visible := false;
  lDate95Percent.Visible := false;
  lDateAdjusted.Visible := false;
  eDateAdjusted.Visible := false;
  lDatePlusErrAdjusted.Visible := false;
  eDatePlusErrAdjusted.Visible := false;
  lDateMinusErrAdjusted.Visible := false;
  eDateMinusErrAdjusted.Visible := false;
  lRoPlusMinus.Visible := false;
  lRoErrPlusOnly.Visible := false;
  lRoErrMinusOnly.Visible := false;
  eRoErrMinus.Visible := false;
  lEpsilonStr.Visible := false;
  eEpsilon.Visible := false;
  lEpsilonPlusMinus.Visible := false;
  eEpsilonErr.Visible := false;
  lEpsilon95Percent.Visible := false;
  lMuErrPlusOnly.Visible := false;
  eMuErrMinus.Visible := false;
  lMuErrMinusOnly.Visible := false;
  lAdditionalStr.Visible := false;
  //including decay uncertainties
  //lDateStrIncl.Visible := false;
  lDateDCIncl.Visible := false;
  //eDateIncl.Visible := false;
  lDatePlusMinusIncl.Visible := false;
  lDateErrPlusOnlyIncl.Visible := false;
  eDateErrIncl.Visible := false;
  lDateErrMinusOnlyIncl.Visible := false;
  eDateErrMinusIncl.Visible := false;
  lDate95PercentIncl.Visible := false;
  //eDateAdjustedIncl.Visible := false;
  lDatePlusErrAdjustedIncl.Visible := false;
  eDatePlusErrAdjustedIncl.Visible := false;
  lDateMinusErrAdjustedIncl.Visible := false;
  eDateMinusErrAdjustedIncl.Visible := false;
  lDateAdjustedIncl.Visible := false;
  cbTicLabels.Visible := false;
  cbConcordiaUncertainties.Visible := false;
  lLwrDateDCincl.Visible := false;
  lLwrDatePlusErrIncl.Visible := false;
  lLwrDateMinusErrIncl.Visible := false;
  lLwrDatePlusErrAdjustedIncl.Visible := false;
  lLwrDateMinusErrAdjustedIncl.Visible := false;
  eLwrDatePlusErrIncl.Visible := false;
  eLwrDateMinusErrIncl.Visible := false;
  eLwrDatePlusErrAdjustedIncl.Visible := false;
  eLwrDateMinusErrAdjustedIncl.Visible := false;
  lLwrDate95PercentIncl.Visible := false;
  lLwrDateAdjustedIncl.Visible := false;
  eLwrDateAdjustedIncl.Visible := false;
  eLwrDateIncl.Visible := false;
  eDateAdjustedIncl.Visible := false;
  eDateIncl.Visible := false;
end;

procedure TfmRegressionResult.Scrn_Results;
var
  Msum_Str       : string;
  N_Rep_Str      : string;
  N_Pts_Reg_Str  : string;
  FAlphaStr      : string;
  Lud_pp_Str     : string;
  Iteration_Str  : string;
  tempStr        : string; //string[15]
  tmpDiff, tmpPC : double;
  InverseRatio, InverseRatioError : double;
  tAgeExcl, tAgeErrExcl : double;
  tAgeIncl, tAgeErrIncl : double;
  tDCx, tDCxErr : double;
  tDCy, tDCyErr : double;
  //AgeErrorPlusIncl, AgeErrorMinusIncl : double;
begin
  lEpsilonStr.Font.Size := 10;
  if (EllipseMagnif = 1.0) then lEllipseMagnif.Caption := 'Ellipses are 1 sigma';
  if (EllipseMagnif > 1.0) then lEllipseMagnif.Caption := 'Ellipses are 95% confidence';
  lEllipseMagnif.Visible := false;
  Str(MsumCutoff:4:2,Msum_Str);
  Str(N_Rep:3,N_Rep_Str);
  Str((NumberOfPointsRegressed-2):3,N_Pts_Reg_Str);
  ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed),1.0*(N_Rep),Msum,1);
  tempStr := FormatFloat('#0.000',ProbabilityOfFit);
  //Str(ProbabilityOfFit:8:3,tempStr);
  eProbabilityOfFit.Text := tempStr;
  //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
  if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                               else eProbabilityOfFit.Font.Color := clBlue;
  case Model of
    1 : begin
      if Msum>MsumCutOff then
      begin
        Augmented:='  Errors augmented by Sqrt(MSWD/'+Msum_Str+') ';
        lIsochronErrorchron.Caption := ' Beyond anal. uncertainty';
        lIsochronErrorchron.Font.Color := clRed;
        lMSWDForced.Visible := true;
        Str(T_Mult:7:2,tempStr);
        lMSWDForced.Caption :='MSWD forced to the F cut-off.  Students t = '
                              +tempStr;
        ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed),1.0*(N_Rep),Msum,1);
        tempStr := FormatFloat('#0.000',ProbabilityOfFit);
        //Str(ProbabilityOfFit:8:3,tempStr);
        eProbabilityOfFit.Text := tempStr;
        //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
        if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                     else eProbabilityOfFit.Font.Color := clBlue;
      end;
      if Msum<=MsumCutOff then
      begin
        Augmented:='  ';
        lIsochronErrorchron.Caption := ' Within anal. uncertainty';
        lIsochronErrorchron.Font.Color := clBlue;
        lMSWDForced.Visible := true;
        Str(T_Mult:7:2,tempStr);
        lMSWDForced.Caption :='Conf. int. based on # replicates. Students t = '+tempStr;
        ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed),1.0*(N_Rep),Msum,1);
        tempStr := FormatFloat('#0.000',ProbabilityOfFit);
        //Str(ProbabilityOfFit:8:3,tempStr);
        eProbabilityOfFit.Text := tempStr;
        //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
        if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                     else eProbabilityOfFit.Font.Color := clBlue;
      end;
    end;
    2 : begin
      Augmented:='  No assumptions - equal weights, r=0 ';
      lIsochronErrorchron.Caption := ' Beyond anal. uncertainty';
      lIsochronErrorchron.Font.Color := clRed;
      lMSWDForced.Visible := true;
      Str(T_Mult:7:2,tempStr);
      lMSWDForced.Caption :='MSWD forced to the F cut-off.  Students t = '
                              +tempStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed-2),1.0*(NumberOfPointsRegressed-2),Msum,1);
      Str(ProbabilityOfFit:7:3,tempStr);
      eProbabilityOfFit.Text := tempStr;
      //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                   else eProbabilityOfFit.Font.Color := clBlue;
    end;
    3 : begin
      Augmented:='  Assuming variable initial ratio ';
      lIsochronErrorchron.Caption := ' Beyond anal. uncertainty';
      lIsochronErrorchron.Font.Color := clRed;
      lMSWDForced.Visible := true;
      Str(T_Mult:7:2,tempStr);
      lMSWDForced.Caption :='MSWD forced to the F cut-off.  Students t = '
                              +tempStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed-2),1.0*(NumberOfPointsRegressed-2),Msum,1);
      tempStr := FormatFloat('#0.000',ProbabilityOfFit);
      //Str(ProbabilityOfFit:8:3,tempStr);
      eProbabilityOfFit.Text := tempStr;
      //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                   else eProbabilityOfFit.Font.Color := clBlue;
    end;
    4 : begin
      Augmented:='  Assuming multi-episodic scatter ';
      lIsochronErrorchron.Caption := ' Beyond anal. uncertainty';
      lIsochronErrorchron.Font.Color := clRed;
      lMSWDForced.Visible := true;
      Str(T_Mult:8:3,tempStr);
      lMSWDForced.Caption :='MSWD forced to the F cut-off.  Students t = '
                              +tempStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed-2),1.0*(NumberOfPointsRegressed-2),Msum,1);
      tempStr := FormatFloat('#0.000',ProbabilityOfFit);
      //Str(ProbabilityOfFit:8:3,tempStr);
      eProbabilityOfFit.Text := tempStr;
      //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                   else eProbabilityOfFit.Font.Color := clBlue;
    end;
    5 : begin
      Augmented:='  Assuming separate anal. and geol errors ';
      lIsochronErrorchron.Caption := ' Beyond anal. uncertainty';
      lIsochronErrorchron.Font.Color := clRed;
      lMSWDForced.Visible := true;
      Str(T_Mult:7:2,tempStr);
      lMSWDForced.Caption :='MSWD forced to the F cut-off.  Students t = '
                              +tempStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed-2),1.0*(NumberOfPointsRegressed-2),Msum,1);
      tempStr := FormatFloat('#0.000',ProbabilityOfFit);
      //Str(ProbabilityOfFit:8:3,tempStr);
      eProbabilityOfFit.Text := tempStr;
      //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                   else eProbabilityOfFit.Font.Color := clBlue;
    end;
    6 : begin
      Augmented:='  Errors not augmented ';
      lIsochronErrorchron.Caption := ' Beyond anal. uncertainty';
      lIsochronErrorchron.Font.Color := clRed;
      lMSWDForced.Visible := true;
      Str(T_Mult:7:2,tempStr);
      lMSWDForced.Caption :='Conf. int. based on # replicates. Students t = '+tempStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(NumberOfPointsRegressed),1.0*(N_Rep),Msum,1);
      tempStr := FormatFloat('#0.000',ProbabilityOfFit);
      //Str(ProbabilityOfFit:8:3,tempStr);
      eProbabilityOfFit.Text := tempStr;
      //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                   else eProbabilityOfFit.Font.Color := clBlue;
    end;
  end;
  eTitle.Text := Title;
  if Converg_done=true then
  begin
    Str(Iteration:3,Iteration_Str);
    eIterationStr.Caption := 'Regression converged after '+Iteration_Str+' iterations';
  end else
  begin
    Str(Iteration:3,Iteration_Str);
    eIterationStr.Caption := 'Regression had not converged after '+Iteration_Str+' iterations';
  end;
  Str(FAlpha:5:3,FAlphaStr);
  lFCrit.Caption := 'F ('+FAlphaStr+'; '+N_Rep_Str+'; '+N_Pts_Reg_Str+') = '+Msum_Str;
  lCentroidX.Caption := XRatioStr[iAnalTyp];
  Str(XCentroid:12:6,tempStr);
  eCentroidX.Text := tempStr;
  lCentroidY.Caption := YRatioStr[iAnalTyp];
  Str(YCentroid:12:6,tempStr);
  eCentroidY.Text := tempStr;
  if (Slope > 0.001) then Str(Slope:12:7,tempStr)
                     else Str(Slope:12:10,tempStr);
  eSlope.Text := tempStr;
  if (Slope > 0.001) then Str(SlopeError:12:7,tempStr)
                     else Str(SlopeError:12:10,tempStr);
  eSlopeErr.Text := tempStr;
  if (Intercept > 0.001) then Str(Intercept:12:6,tempStr)
                         else Str(Intercept:12:10,tempStr);
  eIntercept.Text := tempStr;
  if (Intercept > 0.001) then Str(InterceptError:12:6,tempStr)
                         else Str(InterceptError:12:10,tempStr);
  eInterceptErr.Text := tempStr;
  if (Msum < 2000.001) then Str(Msum:8:3,tempStr)
                       else Str(Msum:8:0,tempStr);
  eMSWD.Text := tempStr;
  Str(NumberOfPointsRegressed:3,tempStr);
  eNumberOfPointsRegressed.Text := tempStr;
  Str(NumberOfPoints:3,tempStr);
  eNumberOfPoints.Text := tempStr;
  if (AnalType8 in ['U','L']) then
  begin
    if (Model in [4]) then
    begin
      Str(MsumCutoff:8:3,tempStr);
      eMSWD.Text := tempStr;
      ProbabilityOfFit := ProbabilityOfF(1.0*(NEquivPtsRegressed-2),1.0*(NEquivPtsRegressed-2),Msum,1);
      tempStr := FormatFloat('##0.000',ProbabilityOfFit);
      //Str(ProbabilityOfFit:8:3,tempStr);
      eProbabilityOfFit.Text := tempStr;
      //eProbabilityOfFit.Text := FormatFloat('  0.000',ProbabilityOfFit);
      if (ProbabilityOfFit < FAlpha) then eProbabilityOfFit.Font.Color := clRed
                                     else eProbabilityOfFit.Font.Color := clBlue;
      Str(NEquivPtsRegressed:6:2,tempStr);
      eNumberOfPointsRegressed.Text := tempStr;
    end;
  end;
  lAugmented.Caption := Augmented;
  if (AnalType in ['1','2','4'..'7','9','B'..'G']) then
  begin
    if (AnalType in ['C','D']) then
    begin
      if (AnalType in ['C']) then
      begin
        Str((Intercept):10:3,tempStr);
      end;
      if (AnalType in ['D']) then
      begin
        InverseRatio := 1.0 / Intercept;
        Str((InverseRatio):10:3,tempStr);
      end;
    end else
    begin
      if (Intercept >= 10.0) then Str(Intercept:10:6,tempStr);
      if ((Intercept < 10.0) and (Intercept >= 1.0)) then Str(Intercept:10:6,tempStr);
      if (Intercept < 1.0) then Str(Intercept:8:6,tempStr);
    end;
    eRo.Text := tempStr;
    if (AnalType in ['C','D']) then
    begin
      if (AnalType in ['C']) then
      begin
        tmpDiff := InitRatioError;
        Str((tmpDiff):10:3,tempStr);
      end;
      if (AnalType in ['D']) then
      begin
        tmpPC := 100.0*InterceptError/Intercept;
        //tmpDiff := 1.0/Intercept-1.0/(Intercept+InitRatioError);
        tmpDiff := T_Mult*tmpPC*InverseRatio/100.0;
        Str((tmpDiff):10:3,tempStr);
      end;
    end else
    begin
      if (InitRatio >= 10.0) then Str(InitRatioError:10:6,tempStr);
      if ((InitRatio < 10.0) and (InitRatio >= 1.0)) then Str(InitRatioError:10:6,tempStr);
      if (InitRatio < 1.0) then Str(InitRatioError:8:6,tempStr);
    end;
    eRoErr.Text := tempStr;
    if (AnalType in ['1','2','7','9','E','F','G']) then
    begin
      Str(Epsilon1:10:3,tempStr);
      eEpsilon.Text := tempStr;
      Str(EpError1:10:3,tempStr);
      eEpsilonErr.Text := tempStr;
    end;
  end;
  PanelDate.Visible := true;
  case AnalType of
    '0' : begin
            HideResultLabels;
          end;
    '1','2','7','9','E','G' :
          begin
            HideResultLabels;
            PanelDate.Visible := true;
            lDatePlusMinus.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            lDatePlusMinus.Visible := true;
            eDateErr.Visible := true;
            lDate95Percent.Visible := true;
            lRoStr.Caption := 'Initial ratio =';
            lRoPlusMinus.Visible := true;
            lEpsilonStr.Caption := 'Epsilon =';
            lEpsilonStr.Visible := true;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(AgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            eEpsilon.Visible := true;
            lEpsilonPlusMinus.Visible := true;
            eEpsilonErr.Visible := true;
            lEpsilon95Percent.Visible := true;
            if (DecayConstUncertainty[iAnalTyp] > 0.0) then
            begin
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := false;
              lDateErrPlusOnlyIncl.Visible := true;
              eDateErrIncl.Visible := true;
              lDateErrMinusOnlyIncl.Visible := true;
              eDateErrMinusIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              //eDateAdjustedIncl.Visible := false;
              Str(AgeErrorPlusIncl:8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              Str(AgeErrorMinusIncl:8:2,tempStr);
              eDateErrMinusIncl.Text := tempStr;
              //ShowMessage('1');
            end;
          end;
    '3' : begin
            HideResultLabels;
            PanelDate.Visible := true;
            lRoStr.Caption := 'mu =';
            lDateErrPlusOnly.Visible := true;
            lRoErrPlusOnly.Visible := true;
            lDateErrMinusOnly.Visible := true;
            lRoErrMinusOnly.Visible := true;
            eDateErrMinus.Visible := true;
            eRoErrMinus.Visible := true;
            lAdditionalStr.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            lDate95Percent.Visible := true;
            eDateErr.Visible := true;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(UpperAgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            lDateErrPlusOnly.Visible := true;
            Str(LowerAgeError:8:2,tempStr);
            eDateErrMinus.Text := tempStr;
            Str(Mu:6:3,tempStr);
            eRo.Text := tempStr;
            Str(UprMuError:6:3,tempStr);
            eRoErr.Text := tempStr;
            lRoErrPlusOnly.Visible := true;
            Str(LwrMuError:6:3,tempStr);
            eRoErrMinus.Text := tempStr;
            if mu_choice=0 then
              lAdditionalStr.Caption := 'Single stage model';
            if mu_choice=1 then
              lAdditionalStr.Caption := 'Stacey and Kramers two stage model';
            if mu_choice=2 then
              lAdditionalStr.Caption := 'User defined model';
            cbTicLabels.Visible := true;
          end;
    '4','5','6' :
          begin
            HideResultLabels;
            PanelDate.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            lDatePlusMinus.Visible := true;
            eDateErr.Visible := true;
            lDate95Percent.Visible := true;
            lRoStr.Caption := 'Initial ratio =';
            lRoPlusMinus.Visible := true;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(AgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            lEpsilonStr.Caption := 'Epsilon =';
            lEpsilonStr.Visible := false;
            eEpsilon.Visible := false;
            lEpsilonPlusMinus.Visible := false;
            eEpsilonErr.Visible := false;
            lEpsilon95Percent.Visible := false;
            if (DecayConstUncertainty[iAnalTyp] > 0.0) then
            begin
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := false;
              lDateErrPlusOnlyIncl.Visible := true;
              eDateErrIncl.Visible := true;
              lDateErrMinusOnlyIncl.Visible := true;
              eDateErrMinusIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              //eDateAdjustedIncl.Visible := false;
              Str(AgeErrorPlusIncl:8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              Str(AgeErrorMinusIncl:8:2,tempStr);
              eDateErrMinusIncl.Text := tempStr;
              //ShowMessage('1');
            end;
          end;
    '8','A' : begin
            HideResultLabels;
            PanelDate.Visible := true;
            cbConcordiaUncertainties.Checked := true;
            lDateStr.Caption := 'Upper date =';
            lRoStr.Caption := 'Lower date =';
            eDateErr.Visible := true;
            lRoStr.Visible := true;
            //lEpsilonStr.Caption := '(incl. d.c. unc.)';
            //lEpsilonStr.Visible := true;
            lDateDCexcl.Visible := true;
            lDateErrPlusOnly.Visible := true;
            lRoErrPlusOnly.Visible := true;
            lDateErrMinusOnly.Visible := true;
            lRoErrMinusOnly.Visible := true;
            eDateErrMinus.Visible := true;
            eRoErrMinus.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            //including decay uncertainties
            if (DecayConstUncertainty[ord(at238UPb)] > 0.0) then
            begin
              //lDateStrIncl.Visible := true;
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := true;
              eDateErrIncl.Visible := true;
              eDateErrMinusIncl.Visible := true;
              //eDateMinusErrAdjustedIncl.Visible := true;
              lDateErrPlusOnlyIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              //lDateAdjustedIncl.Visible := true;
              eDateErrIncl.Visible := true;
              //eMuErrMinus.Visible := true;
              //lEpsilonStr.Visible := false;
              //eEpsilon.Visible := true;
              //eEpsilonErr.Visible := true;
              //lEpsilon95Percent.Visible := false;
              //eEpsilonErr.Visible := false;
              lDate95Percent.Visible := true;
              eDateErr.Visible := true;
              //eLwrDateAdjustedIncl.Visible := true;
              eLwrDateIncl.Visible := true;
              //eDateAdjustedIncl.Visible := true;
              eDateIncl.Visible := true;
              lLwrDateDCincl.Visible := true;
              lLwrDatePlusErrIncl.Visible := true;
              lLwrDateMinusErrIncl.Visible := true;
              //lLwrDatePlusErrAdjustedIncl.Visible := true;
              //lLwrDateMinusErrAdjustedIncl.Visible := true;
              eLwrDatePlusErrIncl.Visible := true;
              eLwrDateMinusErrIncl.Visible := true;
              //eLwrDatePlusErrAdjustedIncl.Visible := true;
              lLwrDate95PercentIncl.Visible := true;
            end;
            cbTicLabels.Visible := true;
            cbConcordiaUncertainties.Visible := true;
            //upper intercept
            //age with and without decay constant uncertainty are the same
            Str(UprIntercept:8:2,tempStr);
            eDate.Text := tempStr;
            //plus age uncertainty without decay constant uncertainties
            Str(UprUprAgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            //minus age uncertainty without decay constant uncertainties
            Str(UprLwrAgeError:8:2,tempStr);
            eDateErrMinus.Text := tempStr;
            //lower intercept
            Str((LwrIntercept):8:2,tempStr);
            eRo.Text := tempStr;
            eEpsilon.Text := tempStr;
            //plus age uncertainty without decay constant uncertainties
            Str(LwrUprAgeError:8:2,tempStr);
            eRoErr.Text := tempStr;
            Str(LwrLwrAgeError:8:2,tempStr);
            eRoErrMinus.Text := tempStr;
            eEpsilon.Text := '0.0';
            lEpsilonStr.Caption := '(excl. d.c. unc.)';
            lEpsilonStr.Visible := true;
            lEpsilonStr.Font.Size := 8;
            if (DecayConstUncertainty[ord(at238UPb)] > 0.0) then
            begin
              Str((UprIntercept):8:2,tempStr);
              eDateIncl.Text := tempStr;
                //minus age uncertainty with decay constant uncertainties
              //plus age uncertainty with decay constant uncertainties
              Str((UprUprAgeErrorIncl):8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              //ShowMessage('plus uprupr '+FormatFloat('####0.0000',UprUprAgeErrorIncl));
              lDateErrPlusOnly.Visible := true;
              lDateErrPlusOnlyIncl.Visible := true;
              Str((LwrIntercept):8:2,tempStr);
              eLwrDateIncl.Text := tempStr;
              Str((UprLwrAgeErrorIncl):8:2,tempStr);
              eDateErrMinusIncl.Text := tempStr;
              //ShowMessage('minus uprlwr '+FormatFloat('####0.0000',UprLwrAgeErrorIncl));
              //plus age uncertainty with decay constant uncertainties
              lRoErrPlusOnly.Visible := true;
              Str(LwrUprAgeErrorIncl:8:2,tempStr);
              eLwrDatePlusErrIncl.Text := tempStr;
              Str(LwrLwrAgeErrorIncl:8:2,tempStr);
              eLwrDateMinusErrIncl.Text := tempStr;
            end;
            Str(Lud_pp:5:2,Lud_pp_Str);
            if (AnalType8='U') then
            begin
              lAdditionalStr.Visible := true;
              lAdditionalStr.Caption := 'Weighted for upper intercept'+
                '  Discordance factor = '+Lud_pp_Str;
            end;
            if (AnalType8='L') then
            begin
              lAdditionalStr.Visible := true;
              lAdditionalStr.Caption := 'Weighted for lower intercept'+
                '  Discordance factor = '+Lud_pp_Str;
            end;
            // if lower intercept is within uncertainty of origin, adjust
            // age uncertainties assuming slope line passes through origin
            //
            // Not appropriate to do this for a constrained fit
            if ((AnalType = '8')
              and (AdjustForNegativeIntercept)
              and (not ConstrainFlag)) then
            begin
              //ShowMessage('Adjusted regression '+FormatFloat('###0.0000',UprIntercept)+'___'+FormatFloat('###0.0000',UprUprAgeError2));
              eDateAdjusted.Visible := true;
              lDateAdjusted.Visible := true;
              lDatePlusErrAdjusted.Visible := true;
              eDatePlusErrAdjusted.Visible := true;
              lDateMinusErrAdjusted.Visible := true;
              eDateMinusErrAdjusted.Visible := true;
              //lEpsilonStr.Caption := '(incl. d.c. unc.)';
              //lEpsilonStr.Visible := true;
              eEpsilon.Visible := true;
              eEpsilon.Text := '    0.00';
              eEpsilonErr.Visible := true;
              lEpsilon95Percent.Visible := true;
              lEpsilon95Percent.Caption := '(adj. for neg. itcpt.)';
              lMuErrPlusOnly.Visible := true;
              lMuErrMinusOnly.Visible := true;
              Str(UprUprAgeError2:8:2,tempStr);
              eDateAdjusted.Text := tempStr;
              tmpDiff := UprUprAgeError2-UprIntercept;
              Str((UprUprAgeError-tmpDiff):8:2,tempStr);
              eDatePlusErrAdjusted.Text := tempStr;
              Str((UprLwrAgeError+tmpDiff):8:2,tempStr);
              eDateMinusErrAdjusted.Text := tempStr;
              tmpDiff := Abs(LwrIntercept);
              Str((LwrUprAgeError-tmpDiff):8:2,tempStr);
              eEpsilonErr.Text := tempStr;
              Str((LwrLwrAgeError+tmpDiff):8:2,tempStr);
              eMuErrMinus.Text := tempStr;
              eMuErrMinus.Visible := true;

              if (DecayConstUncertainty[ord(at238UPb)] > 0.0) then
              begin
                Str(UprUprAgeError2:8:2,tempStr);
                eDateAdjustedIncl.Text := tempStr;
                tmpDiff := UprUprAgeError2-UprIntercept;
                Str((UprUprAgeErrorIncl-tmpDiff):8:2,tempStr);
                eDatePlusErrAdjustedIncl.Text := tempStr;
                Str((UprLwrAgeErrorIncl+tmpDiff):8:2,tempStr);
                eDateMinusErrAdjustedIncl.Text := tempStr;

                tmpDiff := Abs(LwrIntercept);
                //eDateAdjustedIncl.Visible := true;
                lDateAdjustedIncl.Visible := true;
                eDateAdjustedIncl.Visible := true;
                lDatePlusErrAdjustedIncl.Visible := true;
                eDatePlusErrAdjustedIncl.Visible := true;
                lDateMinusErrAdjustedIncl.Visible := true;
                eDateMinusErrAdjustedIncl.Visible := true;
                //Str((LwrUprAgeErrorIncl+tmpDiff):8:2,tempStr);
                eLwrDateAdjustedIncl.Text := '    0.00';
                Str((LwrUprAgeErrorIncl-tmpDiff):8:2,tempStr);
                eLwrDatePlusErrAdjustedIncl.Text := tempStr;
                Str((LwrLwrAgeErrorIncl+tmpDiff):8:2,tempStr);
                eLwrDateMinusErrAdjustedIncl.Text := tempStr;
                eLwrDateAdjustedIncl.Visible := true;
                lLwrDatePlusErrAdjustedIncl.Visible := true;
                lLwrDateMinusErrAdjustedIncl.Visible := true;
                eLwrDatePlusErrAdjustedIncl.Visible := true;
                eLwrDateMinusErrAdjustedIncl.Visible := true;
                lLwrDateAdjustedIncl.Visible := true;
              end;
            end;
          end;
    'B' :
          begin
            HideResultLabels;
            PanelDate.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            lDatePlusMinus.Visible := true;
            eDateErr.Visible := true;
            lDate95Percent.Visible := true;
            lRoStr.Caption := '40Ar/36Ar =';
            lRoPlusMinus.Visible := true;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(AgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            eEpsilon.Visible := false;
            eEpsilonErr.Visible := false;
            eRo.Visible := true;
            eRoErr.Visible := true;
            {
            if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
            begin
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := true;
              lDatePlusMinusIncl.Visible := true;
              //lDatePlusErrIncl.Visible := true;
              eDateErrIncl.Visible := true;
              //lDateErrMinusOnlyIncl.Visible := true;
              eDateErrMinusIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              Str(UprUprAgeErrorIncl:8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              //Str(UprLwrAgeErrorIncl:8:2,tempStr);
              //eDateErrMinusIncl.Text := tempStr;
            end;
            }
          end;
    'C' :
          begin
            HideResultLabels;
            PanelDate.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            lDatePlusMinus.Visible := true;
            eDateErr.Visible := true;
            lDate95Percent.Visible := true;
            lRoStr.Caption := '40Ar/36Ar =';
            lRoPlusMinus.Visible := true;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(AgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            eEpsilon.Visible := false;
            eEpsilonErr.Visible := false;
            eRo.Visible := true;
            eRoErr.Visible := true;
            if ((DecayConstUncertainty[ord(atKAr)] > 0.0)) then
            begin
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := true;
              lDatePlusMinusIncl.Visible := true;
              //lDatePlusErrIncl.Visible := true;
              eDateErrIncl.Visible := true;
              //lDateErrMinusOnlyIncl.Visible := true;
              //eDateErrMinusIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              Str(UprUprAgeErrorIncl:8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              //Str(UprLwrAgeErrorIncl:8:2,tempStr);
              //eDateErrMinusIncl.Text := tempStr;
            end;
          end;
    'D' :
          begin
            HideResultLabels;
            PanelDate.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            eDateErr.Visible := true;
            lDate95Percent.Visible := true;
            lDateErrPlusOnly.Visible := true;
            lDateErrMinusOnly.Visible := true;
            eDateErrMinus.Visible := true;
            lRoStr.Caption := '40Ar/36Ar =';
            lRoStr.Visible := true;
            eRo.Visible := true;
            lRoPlusMinus.Visible := true;
            eRoErr.Visible := true;
            lEpsilonStr.Caption := 'X intercept = ';
            lEpsilonStr.Visible := true;
            eEpsilon.Visible := true;
            lEpsilonPlusMinus.Visible := false;
            eEpsilonErr.Visible := false;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(UprUprAgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            lDateErrPlusOnly.Visible := true;
            Str(UprLwrAgeError:8:2,tempStr);
            eDateErrMinus.Text := tempStr;
            eEpsilon.Visible := true;
            eEpsilonErr.Visible := false;
            Str(XItcpt:10:6,tempStr);
            eEpsilon.Text := tempStr;
            if (DecayConstUncertainty[ord(atKAr)] > 0.0) then
            begin
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := true;
              lDatePlusMinusIncl.Visible := true;
              //lDatePlusErrIncl.Visible := true;
              eDateErrIncl.Visible := true;
              //lDateErrMinusOnlyIncl.Visible := true;
              //eDateErrMinusIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              Str(UprUprAgeErrorIncl:8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              //Str(UprLwrAgeErrorIncl:8:2,tempStr);
              //eDateErrMinusIncl.Text := tempStr;
            end;
          end;
    'F' :
          begin
            HideResultLabels;
            PanelDate.Visible := true;
            lDateStr.Visible := true;
            eDate.Visible := true;
            lDatePlusMinus.Visible := true;
            eDateErr.Visible := true;
            lDate95Percent.Visible := true;
            lRoStr.Caption := 'Initial ratio =';
            lEpsilonStr.Caption := 'Gamma =';
            lEpsilonStr.Visible := true;
            eMuErrMinus.Visible := false;
            Str(Age:8:2,tempStr);
            eDate.Text := tempStr;
            Str(AgeError:8:2,tempStr);
            eDateErr.Text := tempStr;
            eEpsilon.Visible := true;
            lEpsilonPlusMinus.Visible := true;
            eEpsilonErr.Visible := true;
            lEpsilon95Percent.Visible := true;
            if (DecayConstUncertainty[iAnalTyp] > 0.0) then
            begin
              lDateDCIncl.Visible := true;
              //eDateIncl.Visible := false;
              lDateErrPlusOnlyIncl.Visible := true;
              eDateErrIncl.Visible := true;
              lDateErrMinusOnlyIncl.Visible := true;
              eDateErrMinusIncl.Visible := true;
              lDate95PercentIncl.Visible := true;
              //eDateAdjustedIncl.Visible := false;
              Str(AgeErrorPlusIncl:8:2,tempStr);
              eDateErrIncl.Text := tempStr;
              Str(AgeErrorMinusIncl:8:2,tempStr);
              eDateErrMinusIncl.Text := tempStr;
              //ShowMessage('1');
            end;
          end;
  end;//case
  if ConstrainFlag then
  begin
    lConstrain.Visible := true;
    eXConstrain.Visible := true;
    lConstrainAnd.Visible := true;
    eYConstrain.Visible := true;
    lConstrainNear.Visible := true;
    eConstrainAge.Visible := true;
    lConstrainMa.Visible := true;
    Str(XConstrain:11:4,tempStr);
    eXConstrain.Text := tempStr;
    Str(YConstrain:11:4,tempStr);
    eYConstrain.Text := tempStr;
    if (AnalType in ['8','A']) then
    begin
      lConstrainNear.Visible := true;
      eConstrainAge.Visible := true;
      lConstrainMa.Visible := true;
      Str(AgeConstrain:7:2,tempStr);
      eConstrainAge.Text := tempStr;
    end else
    begin
      lConstrainNear.Visible := false;
      eConstrainAge.Visible := false;
      lConstrainMa.Visible := false;
    end;
  end
  else begin
    lConstrain.Visible := false;
    eXConstrain.Visible := false;
    lConstrainAnd.Visible := false;
    eYConstrain.Visible := false;
    lConstrainNear.Visible := false;
    eConstrainAge.Visible := false;
    lConstrainMa.Visible := false;
  end;
end;//Scrn_Results

procedure TfmRegressionResult.ChartRegClick(Sender: TObject);
var
  i, itmp : integer;
  x, y : double;
  tx, ty : double;
  tmpStr : string;
  Node : TTreeNode;
begin
  //Series1.GetCursorValues(x,y);
  for i := 0 to ChartReg.SeriesCount-1 do
  begin
    if (i in [7,8,9]) then
    begin
      itmp := ChartReg.Series[i].GetCursorValueIndex;
      if (itmp <> -1) then
      begin
        //ShowMessage('Clicked series: '+ChartReg.Series[i].Name+' at point: '+IntToStr(itmp));
        dmGDWtmp.cdsReg.Locate('i',itmp+1,[]);
        tmpStr := dmGDWtmp.cdsRegSample_no.AsString;
        tx := dmGdwtmp.cdsRegXRatio.AsFloat;
        ty := dmGdwtmp.cdsRegYRatio.AsFloat;
        ChartReg.Series[iCurrent].Clear;
        ChartReg.Series[iCurrent].AddXY(tx,ty);
        ChartReg.Enabled := true;
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

procedure TfmRegressionResult.ChartRegDblClick(Sender: TObject);
begin
  ChartRegClick(Sender);
  dmGDWtmp.cdsReg.Edit;
  if (dmGDWtmp.cdsRegRFlag.AsString = 'Y') then dmGDWtmp.cdsRegRFlag.AsString := 'N'
  else dmGDWtmp.cdsRegRFlag.AsString := 'Y';
  dmGDWtmp.cdsReg.Post;
end;

procedure TfmRegressionResult.ChartRegMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  //
end;

procedure TfmRegressionResult.ChartRegZoom(Sender: TObject);
var
  IncrementAmountLeft, IncrementAmountBottom : double;
  NumLabelledTicsLeft, NumLabelledTicsBottom : integer;
  RangeLeft, RangeBottom : double;
  MinX, MaxX,
  MinY, MaxY : double;
  tFactor : double;
  NumTics : integer;
begin
  NumTics := 10;
  MinY := ChartReg.Axes.Left.Minimum;
  MaxY := ChartReg.Axes.Left.Maximum;
  MinX := ChartReg.Axes.Bottom.Minimum;
  MaxX := ChartReg.Axes.Bottom.Maximum;
  IncrementAmountLeft := ChartReg.Axes.Left.Increment;
  IncrementAmountBottom := ChartReg.Axes.Bottom.Increment;
  //ShowMessage('Default IncrementLeft = '+FormatFloat('####0.000000',IncrementAmountLeft));
  //ShowMessage('Default IncrementBottom = '+FormatFloat('####0.000000',IncrementAmountBottom));
  //if (IncrementAmountLeft <= 0.0) then IncrementAmountLeft := 0.1;
  //if (IncrementAmountBottom <= 0.0) then IncrementAmountBottom := 0.1;
  RangeLeft := MaxY-MinY;
  RangeBottom := MaxX-MinX;
  if (IncrementAmountLeft <= 0.0) then IncrementAmountLeft := RangeLeft/(1.0*NumTics);
  if (IncrementAmountBottom <= 0.0) then IncrementAmountBottom := RangeBottom/(1.0*NumTics);
  NumLabelledTicsLeft := Round(RangeLeft/IncrementAmountLeft);
  NumLabelledTicsBottom := Round(RangeBottom/IncrementAmountBottom);
  //ShowMessage('RangeLeft = '+FormatFloat('###0.0000000',RangeLeft)+'__'+Int2Str(NumLabelledTicsLeft));
  IncrementAmountLeft := RangeLeft/(1.0*NumTics);
  IncrementAmountBottom := RangeBottom/(1.0*NumTics);
  //ShowMessage('Minimum = '+FormatFloat('###0.0000000',MinY)+'__'+'Maximum = '+FormatFloat('###0.0000000',MaxY));
  NumLabelledTicsLeft := Round(RangeLeft/IncrementAmountLeft);
  NumLabelledTicsBottom := Round(RangeBottom/IncrementAmountBottom);
  //ShowMessage('IncrementAmountLeft before = '+FormatFloat('###0.0000000',IncrementAmountLeft)+'__'+Int2Str(NumLabelledTicsLeft));
  //ShowMessage('IncrementAmountBottom before = '+FormatFloat('###0.0000000',IncrementAmountBottom)+'__'+Int2Str(NumLabelledTicsBottom));
  MinY := ChartReg.Axes.Left.Minimum;
  MaxY := ChartReg.Axes.Left.Maximum;
  MinX := ChartReg.Axes.Bottom.Minimum;
  MaxX := ChartReg.Axes.Bottom.Maximum;
  RangeLeft := MaxY-MinY;
  RangeBottom := MaxX-MinX;
  if (IncrementAmountLeft < 1.0) then
  begin
    tFactor := (1.0/IncrementAmountLeft)/10.0;
    tFactor := 10.0*Round(tFactor);
    IncrementAmountLeft := 1.0/tFactor;
  end else
  begin
    if (IncrementAmountLeft >= 10.0) then
    begin
      IncrementAmountLeft := Round(RangeLeft/10.0);
    end else
    begin
      IncrementAmountLeft := RangeLeft/10.0;
    end;
  end;
  if (IncrementAmountBottom < 1.0) then
  begin
    tFactor := (1.0/IncrementAmountBottom)/10.0;
    tFactor := 10.0*Round(tFactor);
    IncrementAmountBottom := 1.0/tFactor;
  end else
  begin
    if (IncrementAmountBottom >= 10.0) then
    begin
      IncrementAmountBottom := Round(RangeBottom/10.0);
    end else
    begin
      IncrementAmountBottom := RangeBottom/10.0;
    end;
  end;
  NumLabelledTicsLeft := Round(RangeLeft/IncrementAmountLeft);
  NumLabelledTicsBottom := Round(RangeBottom/IncrementAmountBottom);
  //ShowMessage('IncrementAmountLeft before = '+FormatFloat('###0.0000000',IncrementAmountLeft)+'__'+Int2Str(NumLabelledTicsLeft));
  ///ShowMessage('IncrementAmountBottom before = '+FormatFloat('###0.0000000',IncrementAmountBottom)+'__'+Int2Str(NumLabelledTicsBottom));
  //ShowMessage('IncrementAmountLeft after = '+FormatFloat('###0.0000000',IncrementAmountLeft)+'__'+Int2Str(NumLabelledTicsLeft));
  //ShowMessage('RangeBottom = '+FormatFloat('###0.0000000',RangeBottom)+'__'+Int2Str(NumLabelledTicsBottom));
  //ShowMessage('Minimum = '+FormatFloat('###0.0000000',MinX)+'__'+'Maximum = '+FormatFloat('###0.0000000',MaxX));
  //ShowMessage('IncrementAmountBottom = '+FormatFloat('###0.0000000',IncrementAmountBottom)+'__'+Int2Str(NumLabelledTicsBottom));
  //if (NumLabelledTicsLeft > 10) then
  //begin
    //IncrementAmountLeft := 1.0*Round(RangeLeft/10.0);
    if (IncrementAmountLeft <= 0.0) then IncrementAmountLeft := RangeLeft/(1.0*NumTics);
    //ShowMessage('Set IncrementAmountLeft = '+FormatFloat('###0.0000000',IncrementAmountLeft)+'__'+Int2Str(NumLabelledTicsLeft));
    ChartReg.LeftAxis.Automatic := false;
    ChartReg.LeftAxis.Increment := IncrementAmountLeft;
    ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
  //end;
  //if (NumLabelledTicsBottom > 10) then
  //begin
    //IncrementAmountBottom := 1.0*Round(RangeBottom/10.0);
    if (IncrementAmountBottom <= 0.0) then IncrementAmountBottom := RangeBottom/(1.0*NumTics);
    //ShowMessage('Set IncrementAmountBottom = '+FormatFloat('###0.0000000',IncrementAmountBottom)+'__'+Int2Str(NumLabelledTicsBottom));
    ChartReg.BottomAxis.Automatic := false;
    ChartReg.BottomAxis.Increment := IncrementAmountBottom;
    ChartReg.BottomAxis.SetMinMax(MinX,MaxX);
  //end;
  MinY := ChartReg.Axes.Left.Minimum;
  MaxY := ChartReg.Axes.Left.Maximum;
  MinX := ChartReg.Axes.Bottom.Minimum;
  MaxX := ChartReg.Axes.Bottom.Maximum;
  RangeLeft := MaxY-MinY;
  //ShowMessage('RangeLeft = '+FormatFloat('####0.000000',RangeLeft));
  ChartReg.Axes.Left.AxisValuesFormat := '####0.0##';
  if (RangeLeft <= 0.7) then ChartReg.Axes.Left.AxisValuesFormat := '###0.000';
  if (RangeLeft <= 0.07) then ChartReg.Axes.Left.AxisValuesFormat := '###0.0000';
  if (RangeLeft <= 0.007) then ChartReg.Axes.Left.AxisValuesFormat := '###0.00000';
  if (RangeLeft <= 0.0007) then ChartReg.Axes.Left.AxisValuesFormat := '###0.000000';
  if (RangeLeft <= 0.00007) then ChartReg.Axes.Left.AxisValuesFormat := '###0.000000';
  RangeBottom := MaxX-MinX;
  //ShowMessage('RangeBottom = '+FormatFloat('####0.000000',RangeBottom));
  ChartReg.Axes.Bottom.AxisValuesFormat := '####0.0##';
  if (RangeBottom <= 0.7) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.00';
  if (RangeBottom <= 0.07) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.000';
  if (RangeBottom <= 0.007) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.0000';
  if (RangeBottom <= 0.0007) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.00000';
  if (RangeBottom <= 0.00007) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.000000';
end;

procedure TfmRegressionResult.CheckConstrain;
var
  ConstrainChar : char;
  L             : integer;
begin
   if (ProcessOption='C') then ConstrainFlag:=true
                          else ConstrainFlag:=false;
   if ConstrainFlag then
   begin
     try
       ConstrainForm := TfmConstrain.Create(Self);
       ConstrainForm.ShowModal;
     finally
       ConstrainForm.Free;
     end;
   end;
end;

procedure TfmRegressionResult.Regress_Data;
var
  J : integer;
begin
  Lud_pp := 0.2;
   for J:=1 to NumberOfPoints do
   begin
     case AnalType of
       '0' : begin
          if (Ratio[J,1]<-9990.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<-9990.00001) then RFlg[J]:='N';
          if (Ratio[J,1]<-9990.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<-9990.000001) then PFlg[J]:='N';
       end;
       '1' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.690) then RFlg[J]:='N';
          if (Ratio[J,1]<0.00001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.6001) then PFlg[J]:='N';
       end;
       '2' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.4001) then PFlg[J]:='N';
       end;
       '3' : begin
          if (Ratio[J,1]<8.001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<5.0001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '4' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '5' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '6' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '7' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.150) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.1001) then PFlg[J]:='N';
       end;
       '8' : begin
          if (Ratio[J,1]<0.0001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.001) then RFlg[J]:='N';
          if (Ratio[J,1]<0.00001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.00001) then PFlg[J]:='N';
       end;
       '9','A'..'G' : begin
          if (Ratio[J,1]<0.000001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.000001) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.000001) then PFlg[J]:='N';
       end;
     end;
   end;
   CheckConstrain;
   if (AnalType in ['8','A']) then
   begin
     try
       ConcordiaWtTypeForm := TfmConcordiaWtType.Create(Self);
       ConcordiaWtTypeForm.ShowModal;
     finally
       ConcordiaWtTypeForm.Free;
     end;
   end;
   NumberOfPointsRegressed:=0;
   for J:=1 to NumberOfPoints do begin
      if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
   end;
   NEquivPtsRegressed:=NumberOfPointsRegressed;
   if NumberOfPointsRegressed>1 then begin
     StartAtX:=Ratio[1,1];
     StartAtY:=Ratio[1,2];
     EndAtX:=StartAtX;
     EndAtY:=StartAtY;
     for J:=1 to NumberOfPointsRegressed do
     begin
       if (Ratio[J,1] < StartAtX) then
       begin
         StartAtX:=Ratio[J,1];
         StartAtY:=Ratio[J,2];
       end;
       if (Ratio[J,1] > EndAtX) then
       begin
         EndAtX:=Ratio[J,1];
         EndAtY:=Ratio[J,2];
       end;
     end;
     if (EndAtX-StartAtX)<>0 then
       Slope:=(EndAtY-StartAtY)/(EndAtX-StartAtX)
     else Slope:=0.015;
     if (NumberOfPointsRegressed <= 2) then
     begin
       MsumCutOff:=1;
     end else
     begin
       if (NumberOfPointsRegressed > NumStatisticsValues) then
       begin
         MsumCutoff:=MSUM_Val[NumStatisticsValues];
       end else
       begin
         MsumCutoff:=MSUM_Val[NumberOfPointsRegressed-2];
       end;
     end;
     if (MsumCutoff = 0.0) then MsumCutOff := 1.00;
     if (AnalType8='N') then Lud_pp:=1.0
                        else Lud_pp:=0.2;
     CalcTMult(1.0*N_Rep);
     Model:=1;
     //ShowMessage('1');
     RegressModel(Lud_pp);
     //ShowMessage('2');
     Get_NewSlope;
     //ShowMessage('3');
     CalcAgeError;
     //ShowMessage('4');
     Slope1:=Slope;
     Intercept1:=Intercept;
     if (AnalType in ['3','8','A']) then
     begin
       Slope1:=NewSlope;
       Intercept1:=NewIntercept;
       MuAge21:=Age;
       if (AnalType = '3') then Mu1:=Mu
                           else Mu1:=0.0;
     end;
     Scrn_Results;
   end
   else begin
     MessageDlg('Insufficient data for regression', mtWarning,[mbOk], 0);
   end;
end;{proc Regress_Data}

procedure TfmRegressionResult.bbExporttoXMLClick(Sender: TObject);
begin
  //
  MessageDlg('Not yet implemented',mtInformation,[mbOK],0);
end;

procedure TfmRegressionResult.bbSpreadSheetClick(Sender: TObject);
{
var
  fr: TFlexCelReport;
  frTemplateStr, frFileNameStr : string;
begin
  frTemplateStr := FlexTemplatePath+'pdf_grainpdf.xlsx';
  SaveDialogSprdSheet.InitialDir := ExportPath;
  SaveDialogSprdSheet.FileName := 'FitPDF_results_multiplesample';
  if SaveDialogSprdSheet.Execute then
  begin
    frFileNameStr := SaveDialogSprdSheet.FileName;
    ExportPath := ExtractFilePath(SaveDialogSprdSheet.FileName);
    fr := TFlexCelReport.Create(true);
    try
      fr.AddTable('cdsGrainData',dmPDF.cdsGrainData);
      fr.Run(
         frTemplateStr,frFileNameStr
        //TPath.Combine(TPath.GetDocumentsPath, 'report.template.xlsx'),
        //              TPath.Combine(TPath.GetDocumentsPath, 'result.xlsx')
      );
    finally
      fr.Free;
    end;
  end;
}


var
  i, iRow, iCol    : integer;
  StepSize, t1, t2, temp : double;
  tmpStr   : string;
  StepIncrement : integer;
begin
  try
    GetAxisValuesForm := TfmAxOpt.Create(Self);
    GetAxisValuesForm.XMax := ChartReg.BottomAxis.Maximum;
    GetAxisValuesForm.XMin := ChartReg.BottomAxis.Minimum;
    GetAxisValuesForm.YMax := ChartReg.LeftAxis.Maximum;
    GetAxisValuesForm.YMin := ChartReg.LeftAxis.Minimum;
    if (cbErrorEllipses.Checked) then
    begin
      GetAxisValuesForm.cbAugmentEllipses.Enabled := true;
    end else
    begin
      GetAxisValuesForm.cbAugmentEllipses.Enabled := false;
    end;
    GetAxisValuesForm.ShowModal;
    if (GetAxisValuesForm.ModalResult = mrOK) then
    begin
      SaveDialogSprdSheet.InitialDir := TTPath;
      SaveDialogSprdSheet.FileName := ProjectName+'_Regression';
      if SaveDialogSprdSheet.Execute then
      begin
        Drive3 := ExtractFileDir(SaveDialogSprdSheet.FileName);
        TTPath := ExtractFilePath(SaveDialogSprdSheet.FileName);
        //try
          //SprdSheetForm := TfmSheet.Create(Self);
        try
          SprdSheet := TXlsFile.Create(true);
          SprdSheet.NewFile(1);
          //SprdSheetForm.SprdSheet.NumSheets := 1;
          //SprdSheetForm.SprdSheet.Sheet := 1;
          //SprdSheetForm.SprdSheet.SheetName[1]:= 'Regression data';
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
          tmpStr := 'Precision';
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
          tmpStr := 'X reg line';
          SprdSheet.SetCellValue(iRow,iCol,tmpStr);
          iCol := 20;
          tmpStr := 'Y reg line';
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
            end;
          end;
          //Regression line
          StepIncrement := 20; //originally 20
          StepSize := (GetAxisValuesForm.XMax-GetAxisValuesForm.XMin)/StepIncrement;
          j := 2;
          for i := 0 to StepIncrement do
          begin
            iCol := 19;
            iRow := j;
            //tmpStr := FloatToStr(StepSize*i);
            SprdSheet.SetCellValue(iRow,iCol,StepSize*i);
            iCol := 20;
            //tmpStr := FloatToStr(Slope*StepSize*i+Intercept);
            SprdSheet.SetCellValue(iRow,iCol,Slope*StepSize*i+Intercept);
            j := j+1;
          end;
          //Lower error envelope
          StartAtX := GetAxisValuesForm.XMin;
          EndAtX := GetAxisValuesForm.XMax;
          t1 := 5;
          TicsAtX := (EndAtX-StartAtX)/t1;
          i := 2;
          t1 := StartAtX;
          temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
               +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
          if (temp > 0.0) then
          begin
            t2:=NewIntercept+NewSlope*t1-Sqrt(temp);
            iRow := i;
            iCol := 21;
            //tmpStr := FloatToStr(t1);
            SprdSheet.SetCellValue(iRow,iCol,t1);
            iCol := 22;
            //tmpStr := FloatToStr(t2);
            SprdSheet.SetCellValue(iRow,iCol,t2);
            i := i + 1;
          end;
          repeat
            temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
                 +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
            if (temp > 0.0) then
            begin
              t2:=NewIntercept+NewSlope*t1-Sqrt(temp);
              iRow := i;
              iCol := 21;
              //tmpStr := FloatToStr(t1);
              SprdSheet.SetCellValue(iRow,iCol,t1);
              iCol := 22;
              //tmpStr := FloatToStr(t2);
              SprdSheet.SetCellValue(iRow,iCol,t2);
              i := i + 1;
            end;
            t1:=t1+TicsAtX/(1.0*StepIncrement);
          until (t1 > EndAtX);
          //Upper error envelope
          i := 2;
          t1 := StartAtX;
          temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
               +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
          if (temp > 0.0) then
          begin
            t2:=NewIntercept+NewSlope*t1+Sqrt(temp);
            iRow := i;
            iCol := 23;
            //tmpStr := FloatToStr(t1);
            SprdSheet.SetCellValue(iRow,iCol,t1);
            iCol := 24;
            //tmpStr := FloatToStr(t2);
            SprdSheet.SetCellValue(iRow,iCol,t2);
            i := i + 1;
          end;
          repeat
            temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
                 +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
            if (temp > 0.0) then
            begin
              t2:=NewIntercept+NewSlope*t1+Sqrt(temp);
              iRow := i;
              iCol := 23;
              //tmpStr := FloatToStr(t1);
              SprdSheet.SetCellValue(iRow,iCol,t1);
              iCol := 24;
              //tmpStr := FloatToStr(t2);
              SprdSheet.SetCellValue(iRow,iCol,t2);
              i := i + 1;
            end;
            t1:=t1+TicsAtX/(1.0*StepIncrement);
          until (t1 > EndAtX);
          //Error ellipses
          if (cbErrorEllipses.Checked) then
          begin
            iRow := 1;
            iCol := 25;
            tmpStr := 'X ellipse incl.';
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            iCol := 26;
            tmpStr := 'Y ellipse incl.';
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            iCol := 28;
            tmpStr := 'X ellipse excl.';
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            iCol := 29;
            tmpStr := 'Y ellipse excl.';
            SprdSheet.SetCellValue(iRow,iCol,tmpStr);
            ChooseEllipse := 'Y';
            RowNumber := 2;
            for i := 1 to NumberOfPoints do
            begin
              if (PFlg[i] = 'Y') then SprdSheetEllipse(SprdSheet,i);
              RowNumber := RowNumber + 1;
            end;
          end;
          //SprdSheetForm.SprdSheet.Sheet := 1;
          iRow := 1;
          iCol := 1;
          SprdSheet.Save(SaveDialogSprdSheet.FileName);
        finally
          FreeAndNil(SprdSheet);
        end;
        //SprdSheetForm.ShowModal;
      //finally
        //SprdSheetForm.Free;
      //end;
      end;
    end;
  finally
    GetAxisValuesForm.Free;
  end;
end;

procedure TfmRegressionResult.FormShow(Sender: TObject);
var
  i      : integer;
  tmpStr : string; //string[1]
  Node   : TTreeNode;
begin
  //TSystemTheme.ApplyStyle(ChartReg);
  TicksEvery := 100.0;
  eTicksEvery.Text := FormatFloat('###0.0',TicksEvery);
  MaxAgeConcordia := 4500.0;
  eMaxAgeConcordia.Text := FormatFloat('###0.0',MaxAgeConcordia);
  cbLegend.Checked := false;
  iAnalTyp := Get_Ianal_from_AnalType(AnalType);
  if (AnalType = '0') then
  begin
    PanelDate.Visible := false;
  end else
  begin
    PanelDate.Visible := true;
  end;
  if (AnalType in ['3','8','A']) then
  begin
    if (AnalType in ['8','A']) then
    begin
      lMaxAgeConcordia.Visible := true;
      eMaxAgeConcordia.Visible := true;
    end;
    lTicksEvery.Visible := true;
    eTicksEvery.Visible := true;
  end else
  begin
    lMaxAgeConcordia.Visible := false;
    eMaxAgeConcordia.Visible := false;
    lTicksEvery.Visible := false;
    eTicksEvery.Visible := false;
  end;
  Regress_Data;
  with dmGdwtmp.cdsReg do
  begin
    Active := false;
  end;
  FillRegTable(Sender);
  SetAllChartData;
  ChartReg.Title.Caption := Title;
  iAnalTyp := Get_Ianal_from_AnalType(AnalType);
  ChartReg.BottomAxis.Title.TextFormat:=ttfHtml;
  ChartReg.LeftAxis.Title.TextFormat:=ttfHtml;
  ChartReg.BottomAxis.Title.Caption := GraphXRatioStr[iAnalTyp];
  ChartReg.LeftAxis.Title.Caption := GraphYRatioStr[iAnalTyp];
  if (NumberOfPointsRegressed < 2) then
  begin
    {
    ShowMessage('# points regressed = '+IntToStr(NumberOfPointsRegressed));
    fmRegressionResult.bbCloseClick(Sender);
    }
  end;
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

procedure TfmRegressionResult.FillRegTable(Sender: TObject);
var
  i      : integer;
  tmpStr : string;  //string[1]
begin
  with dmGdwtmp.cdsReg do
  begin
    Active := true;
    if (RecordCount > 0) then
    begin
      EmptyDataset;
    end;
    {
    EmptyTable;
    Active := true;
    }
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
         cdsRegYDev.AsFloat := Residual[i,2];
         if (PFlg[i] = 'Y') then cdsRegPFlag.AsString := 'Y'
                            else cdsRegPFlag.AsString := 'N';
         cdsRegProject.AsString := ProjectName;
         cdsRegXRatio.AsFloat := Ratio[i,1];
         cdsRegYRatio.AsFloat := Ratio[i,2];
         cdsRegZRatio.AsFloat := Ratio[i,3];
         cdsRegi.AsInteger := i;

         cdsReg.Next;
     end;
     cdsReg.First;
     iRec := 1;
  end;
  if ((Residual[iRec,1] > (2.5 * ErrorWt[iRec,1])) or
      (Residual[iRec,2] > (2.5 * ErrorWt[iRec,2]))) then
  begin
    lResidual.Visible := true;
  end else
  begin
    lResidual.Visible := false;
  end;
end;

procedure TfmRegressionResult.SetAllChartData;
const
  ApproxNumberOfRowsPerEllipse = 30;

var
  iCode : integer;
  i, j   : integer;
  MaxAge, StepSize : double;
  tmpStr : string;  //string[20]
  tmpAge : double;
  tmpX, tmpY, t1, t2, temp : double;
  tmpXp, tmpYp, tmpXm, tmpYm : double;
  tMinX, tMinY, tMaxX, tMaxY : double;
  OldAgeFactor8, OldAgeFactor5 : double;
  Range : double;
  TMult : double;
  IncrementAmount : double;
  NumLabelledTics : integer;
begin
  { Column       Variable
       1            X pt incl
       2            Y pt incl
       3            X pt excl
       4            Y pt excl
       5            X reg line
       6            Y reg line
       7            X ell incl
       8            X ell incl
       9            X ell excl
      10            X ell excl
      11            X current
      12            X current
      13            X lwr error envelope
      14            Y lwr error envelope
      15            X upr error envelope
      16            Y upr error envelope
      17            X concordia or X Pb model curve
      18            Y concordia or Y Pb model curve
      19            X concordia plus
      20            Y concordia plus
      21            X concordia minus
      22            Y concordia minus
  }
  ChartReg.Enabled := false;
  MaxX := -1.0e9;
  MinX :=  1.0e9;
  MaxY := -1.0e9;
  MinY :=  1.0e9;
  Val(eTicksEvery.Text,TicksEvery,iCode);
  if (iCode <>0) then TicksEvery := 200.0;
  eTicksEvery.Text := FormatFloat('###0.0',TicksEvery);
  Val(eMaxAgeConcordia.Text,MaxAgeConcordia,iCode);
  if (iCode <>0) then MaxAgeConcordia := 4500.0;
  eMaxAgeConcordia.Text := FormatFloat('###0.0',MaxAgeConcordia);
  //Points
  ChartReg.Series[iEllipsesExcluded].Clear;
  ChartReg.Series[iEllipsesIncluded].Clear;
  ChartReg.Series[iEnvelopeLower].Clear;
  ChartReg.Series[iEnvelopeUpper].Clear;
  ChartReg.Series[iCurveTic].Clear;
  ChartReg.Series[iCurveLine].Clear;
  ChartReg.Series[iRegressionLine].Clear;
  ChartReg.Series[iDataExcluded].Clear;
  ChartReg.Series[iDataIncluded].Clear;
  ChartReg.Series[iErrorExcluded].Clear;
  ChartReg.Series[iErrorIncluded].Clear;
  ChartReg.Series[iEllipseConcordia].Clear;
  ChartReg.Series[iDataConcordia].Clear;
  ChartReg.Series[iCurrent].Clear;
  ChartReg.Series[iCurveLinePlus].Clear;
  ChartReg.Series[iCurveLineMinus].Clear;
  ChartReg.Series[iRegressionLine].XValues.Order := loNone;
  ChartReg.Series[iRegressionLine].YValues.Order := loNone;
  ChartReg.Series[iDataIncluded].XValues.Order := loNone;
  ChartReg.Series[iDataIncluded].YValues.Order := loNone;
  ChartReg.Series[iDataExcluded].XValues.Order := loNone;
  ChartReg.Series[iDataExcluded].YValues.Order := loNone;
  ChartReg.Series[iCurveLine].XValues.Order := loNone;
  ChartReg.Series[iCurveLine].YValues.Order := loNone;
  ChartReg.Series[iCurveTic].XValues.Order := loNone;
  ChartReg.Series[iCurveTic].YValues.Order := loNone;
  ChartReg.Series[iEnvelopeUpper].XValues.Order := loNone;
  ChartReg.Series[iEnvelopeUpper].YValues.Order := loNone;
  ChartReg.Series[iEnvelopeLower].XValues.Order := loNone;;
  ChartReg.Series[iEnvelopeLower].YValues.Order := loNone;;
  ChartReg.Series[iEllipsesIncluded].XValues.Order := loNone;
  ChartReg.Series[iEllipsesIncluded].YValues.Order := loNone;
  ChartReg.Series[iEllipsesExcluded].XValues.Order := loNone;
  ChartReg.Series[iEllipsesExcluded].YValues.Order := loNone;
  ChartReg.Series[iCurveLinePlus].XValues.Order := loNone;
  ChartReg.Series[iCurveLinePlus].YValues.Order := loNone;
  ChartReg.Series[iCurveLineMinus].XValues.Order := loNone;
  ChartReg.Series[iCurveLineMinus].YValues.Order := loNone;
  for i := 1 to NumberOfPoints do
  begin
    if (i = 1) then
    begin
      ChartReg.Series[iCurrent].Clear;
      ChartReg.Series[iCurrent].AddXY(Ratio[i,1],Ratio[i,2]);
    end;
    if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
    begin
      ChartReg.Series[iDataIncluded].AddXY(Ratio[i,1],Ratio[i,2]);
    end;
    if ((RFlg[i] = 'N') and (PFlg[i] = 'Y')) then
    begin
      ChartReg.Series[iDataExcluded].AddXY(Ratio[i,1],Ratio[i,2]);
    end;
    if (PFlg[i] = 'Y') then
    begin
      if (MaxX < Ratio[i,1]) then MaxX := Ratio[i,1];
      if (MinX > Ratio[i,1]) then MinX := Ratio[i,1];
      if (MaxY < Ratio[i,2]) then MaxY := Ratio[i,2];
      if (MinY > Ratio[i,2]) then MinY := Ratio[i,2];
    end;
  end;
  //Current sample from tmpRes.DB file
  ChartReg.Series[iCurrent].XValue[0] := dmGdwtmp.cdsRegXRatio.AsFloat;
  ChartReg.Series[iCurrent].YValue[0] := dmGdwtmp.cdsRegYRatio.AsFloat;
  //ShowMessage('after data points MaxX = '+FormatFloat('########0.00000',MaxX));

  TMult := TMultiplier(N_Rep);
  //Concordia curve
  if (AnalType in ['8']) then
  begin
    tmpAge := 0.0;
    tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
    tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
    ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
    tmpXp := exp((DecayConst[ord(at235UPb)]-(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
    tmpYp := exp((DecayConst[ord(at238UPb)]+(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
    ChartReg.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
    tmpXm := exp((DecayConst[ord(at235UPb)]+(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
    tmpYm := exp((DecayConst[ord(at238UPb)]-(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
    ChartReg.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
    j := 1;
    repeat
      tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
      tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
      ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
      tmpXp := exp((DecayConst[ord(at235UPb)]-(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      tmpYp := exp((DecayConst[ord(at238UPb)]+(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      ChartReg.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
      tmpXm := exp((DecayConst[ord(at235UPb)]+(DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      tmpYm := exp((DecayConst[ord(at238UPb)]-(DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0))*tmpAge*1.0e6)-1.0;
      ChartReg.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
      tmpAge := tmpAge + TicksEvery/5.0;
      j := j+1;
    //until (tmpAge > (UprIntercept+50.0));
    until (tmpAge > (MaxAgeConcordia+50.0));
  end;

  //Concordia curve tics
  if (AnalType in ['8']) then
  begin
    tmpAge := 0.0;
    tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
    tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
    //ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
    j := 1;
    repeat
      tmpX := exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0;
      tmpY := exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0;
      ChartReg.Series[iCurveTic].AddXY(tmpX,tmpY,FormatFloat('###0',tmpAge));
      tmpAge := tmpAge + TicksEvery;
      j := j+1;
    //until (tmpAge > (UprIntercept+50.0));
    until (tmpAge > (MaxAgeConcordia+50.0));
  end;

  //Tera-Wasserburg curve
  if (AnalType in ['A']) then
  begin
    if (LwrIntercept < 1.0) then
      tmpX := 12.0
    else
      tmpY := 1.0/(exp(DecayConst[ord(at238UPb)]*LwrIntercept*1.0e6)-1.0);
    if (UprIntercept <=1.0) then
    begin
      tmpAge := 4500.0;
    end else
    begin
      tmpAge := UprIntercept;
    end;
      tmpX := 1.0/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
      tmpY := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0);
      tmpY := tmpY/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
      ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
      tmpXp := 1.0/(exp((DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYp := (1.0/U238U235)*(exp((DecayConst[ord(at235UPb)]+DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYp := tmpYp/(exp((DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      ChartReg.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
      tmpXm := 1.0/(exp((DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYm := (1.0/U238U235)*(exp((DecayConst[ord(at235UPb)]-DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYm := tmpYm/(exp((DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      ChartReg.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
    //tmpAge := 0.00000001;
    //tmpAge := 2800.0;
    j := 1;
    repeat
      tmpX := 1.0/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
      tmpY := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0);
      tmpY := tmpY/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
      ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
      tmpXm := 1.0/(exp((DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYm := (1.0/U238U235)*(exp((DecayConst[ord(at235UPb)]-DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYm := tmpYm/(exp((DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      ChartReg.Series[iCurveLineMinus].AddXY(tmpXm,tmpYm);
      tmpXp := 1.0/(exp((DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYp := (1.0/U238U235)*(exp((DecayConst[ord(at235UPb)]+DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      tmpYp := tmpYp/(exp((DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]*TMult/100.0)*tmpAge*1.0e6)-1.0);
      ChartReg.Series[iCurveLinePlus].AddXY(tmpXp,tmpYp);
      tmpAge := tmpAge - TicksEvery/5.0;
      j := j+1;
    //until (tmpAge > (MaxAge+0.1*MaxAge));
    until (tmpAge < 1.0);
    //until (tmpAge > (4400.0));  //temporary
  end;

  //Tera-Wasserburg curve tics
  if (AnalType in ['A']) then
  begin
    //if (LwrIntercept < 1.0) then
    //  tmpX := 12.0
    //else
    //  tmpY := 1.0/(exp(DecayConst[ord(at238UPb)]*LwrIntercept*1.0e6)-1.0);
    //if (UprIntercept <=1.0) then
    //begin
      tmpX := (1/U238U235)*(exp(DecayConst[ord(at235UPb)]*4500.0*1.0e6)-1.0);
      tmpY := tmpX/(exp(DecayConst[ord(at238UPb)]*4500.00*1.0e6)-1.0);
      MaxAge := 4500.0;
      //ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
    //end else
    //begin
    //  tmpX := (1/U238U235)*(exp(DecayConst[ord(at235UPb)]*UprIntercept*1.0e6)-1.0);
    //  tmpY := tmpX/(exp(DecayConst[ord(at238UPb)]*UprIntercept*1.0e6)-1.0);
    //  MaxAge := UprIntercept;
      //ChartReg.Series[iCurveLine].AddXY(tmpX,tmpY);
    //end;
    tmpAge := MaxAge;
    //tmpAge := 0.00000001;
    j := 1;
    repeat
      tmpX := 1.0/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
      tmpY := (1.0/U238U235)*(exp(DecayConst[ord(at235UPb)]*tmpAge*1.0e6)-1.0);
      tmpY := tmpY/(exp(DecayConst[ord(at238UPb)]*tmpAge*1.0e6)-1.0);
      //ChartReg.Series[iCurveTic].AddXY(tmpX,tmpY,FormatFloat('###0',tmpAge));
      //ChartReg.Series[iCurveTic].AddXY(tmpX,tmpY);
      ChartReg.Series[iCurveTic].AddXY(tmpX,tmpY,FormatFloat('###0',tmpAge));
      tmpAge := tmpAge - TicksEvery;
      j := j+1;
    //until (tmpAge > (MaxAge+0.1*MaxAge));
    until (tmpAge < 1.0);
  end;

  //Pb model curve
  if (AnalType in ['3']) then
  begin
    OldAgeFactor8:=Exp(DecayConst[4]*MuV[mu_choice,1]);
    OldAgeFactor5:=Exp(DecayConst[5]*MuV[mu_choice,1]);
    tmpAge:=0.0;
    t1:=MuV[mu_choice,2]+MuV[mu_choice,5]*(OldAgeFactor8-Exp(DecayConst[4]*tmpAge*1.0e6));
    t2:=MuV[mu_choice,3]+MuV[mu_choice,5]/U238U235*(OldAgeFactor5-Exp(DecayConst[5]*tmpAge*1.0e6));
    j := 1;
    repeat
      t1:=MuV[mu_choice,2]+MuV[mu_choice,5]*(OldAgeFactor8-Exp(DecayConst[4]*tmpAge*1.0e6));
      t2:=MuV[mu_choice,3]+MuV[mu_choice,5]/U238U235*(OldAgeFactor5-Exp(DecayConst[5]*tmpAge*1.0e6));
      ChartReg.Series[iCurveLine].AddXY(t1,t2);
      tmpAge := tmpAge + TicksEvery/5.0;
      j := j+1;
    until (tmpAge > 4560.0);
  end;

  //Pb model curve tics
  if (AnalType in ['3']) then
  begin
    OldAgeFactor8:=Exp(DecayConst[4]*MuV[mu_choice,1]);
    OldAgeFactor5:=Exp(DecayConst[5]*MuV[mu_choice,1]);
    tmpAge:=0.0;
    t1:=MuV[mu_choice,2]+MuV[mu_choice,5]*(OldAgeFactor8-Exp(DecayConst[4]*tmpAge*1.0e6));
    t2:=MuV[mu_choice,3]+MuV[mu_choice,5]/U238U235*(OldAgeFactor5-Exp(DecayConst[5]*tmpAge*1.0e6));
    j := 1;
    repeat
      t1:=MuV[mu_choice,2]+MuV[mu_choice,5]*(OldAgeFactor8-Exp(DecayConst[4]*tmpAge*1.0e6));
      t2:=MuV[mu_choice,3]+MuV[mu_choice,5]/U238U235*(OldAgeFactor5-Exp(DecayConst[5]*tmpAge*1.0e6));
      ChartReg.Series[iCurveTic].AddXY(t1,t2);
      tmpAge := tmpAge + TicksEvery;
      j := j+1;
    until (tmpAge > 4560.0);
  end;

  //Minimum and Maximum values for chart
  if (MaxX <= MinX) then MaxX := MinX + 0.1*MinX;
  if (MaxY <= MinY) then MaxY := MinY + 0.1*MinY;
  //ShowMessage('start min-max for chart MaxX = '+FormatFloat('########0.00000',MaxX));
  if (AnalType in ['8']) then
  begin
    if ((LwrIntercept - 10.0) < 0.0) then
    begin
      tMinX := 0.0;
      tMinY := 0.0;
    end else
    begin
      tMinX := exp(DecayConst[ord(at235UPb)]*(LwrIntercept-10.0)*1.0e6)-1.0;
      tMinY := exp(DecayConst[ord(at238UPb)]*(LwrIntercept-10.0)*1.0e6)-1.0;
    end;
    if (MinX > tMinX) then MinX := tMinX;
    if (MaxX < tMaxX) then MaxX := tMaxX;
    if (MinY > tMinY) then MinY := tMinY;
    if (MaxY < tMaxY) then MaxY := tMaxY;
  end;
  if (AnalType in ['A']) then
  begin
    if ((LwrIntercept - 10.0) < 0.0) then
    begin
      tMaxX := 120.0;
      tMinY := 0.0;
    end else
    begin
      tMaxX := exp(DecayConst[ord(at235UPb)]*(LwrIntercept-10.0)*1.0e6)-1.0;
      tMinY := exp(DecayConst[ord(at238UPb)]*(LwrIntercept-10.0)*1.0e6)-1.0;
    end;
    if (MinX > tMinX) then MinX := tMinX;
    if (MaxX < tMaxX) then MaxX := tMaxX;
    if (MinY > tMinY) then MinY := tMinY;
    if (MaxY < tMaxY) then MaxY := tMaxY;
  end;
  //ShowMessage('start min-max for chart 2 MaxX = '+FormatFloat('########0.00000',MaxX));
  if (AnalType in ['3']) then
  begin
    OldAgeFactor8:=Exp(DecayConst[4]*MuV[mu_choice,1]);
    OldAgeFactor5:=Exp(DecayConst[5]*MuV[mu_choice,1]);
    tmpAge:=Age + 0.1*Age;
    //ShowMessage('Age = '+FormatFloat('###0.000',Age + 0.1*Age));
    t1:=MuV[mu_choice,2]+MuV[mu_choice,5]*(OldAgeFactor8-Exp(DecayConst[4]*tmpAge*1.0e6));
    t2:=MuV[mu_choice,3]+MuV[mu_choice,5]/U238U235*(OldAgeFactor5-Exp(DecayConst[5]*tmpAge*1.0e6));
    if (MinX > t1) then MinX := t1;
    if (MinY > t2) then MinY := t2;
    //ShowMessage('MinX = '+FormatFloat('###0.000',MinX));
    //ShowMessage('MinY = '+FormatFloat('###0.000',MinY));
  end;
  if (AnalType in ['8']) then
  begin
    tMaxX := exp(DecayConst[ord(at235UPb)]*(UprIntercept+50.0)*1.0e6)-1.0;
    tMaxY := exp(DecayConst[ord(at238UPb)]*(UprIntercept+50.0)*1.0e6)-1.0;
    if (MaxX < tMaxX) then MaxX := tMaxX;
    if (MaxY < tMaxY) then MaxY := tMaxY;
  end;
  if (AnalType in ['A']) then
  begin
    tMinX := exp(DecayConst[ord(at235UPb)]*(UprIntercept+50.0)*1.0e6)-1.0;
    tMaxY := exp(DecayConst[ord(at238UPb)]*(UprIntercept+50.0)*1.0e6)-1.0;
    if (MinX > tMaxX) then MinX := tMaxX;
    if (MaxY < tMaxY) then MaxY := tMaxY;
  end;
  //ShowMessage('start min-max for chart 3 MaxX = '+FormatFloat('########0.00000',MaxX));
  if (AnalType in ['1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G']) then
  begin
    tMinX := ChartReg.BottomAxis.Minimum;
    tMinY := ChartReg.LeftAxis.Minimum;
    if (MinX < 0.0) then MinX := 0.0;
    if (MinY < 0.0) then MinY := 0.0;
    if (tMinX < 0.0) then MinX := 0.0;
    if (tMinY < 0.0) then MinY := 0.0;
    //ShowMessage('tMinX = '+FormatFloat('########0.00000',tMinX));
    //ShowMessage('tMinY = '+FormatFloat('########0.00000',tMinY));
    //ShowMessage('MinX = '+FormatFloat('########0.00000',MinX));
    //ShowMessage('MinY = '+FormatFloat('########0.00000',MinY));
    //ShowMessage('MaxX = '+FormatFloat('########0.00000',MaxX));
    //ShowMessage('MaxY = '+FormatFloat('########0.00000',MaxY));
  end;
  MaxX := MaxX + 0.1*(MaxX-MinX);
  MinX := MinX - 0.1*(MaxX-MinX);
  MaxY := MaxY + 0.1*(MaxY-MinY);
  MinY := MinY - 0.1*(MaxY-MinY);
  StepSize := (MaxX)/20;
  ChartReg.BottomAxis.SetMinMax(MinX,MaxX);
  ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
  if (AnalType in ['1','2','4','5','6','7','9','B','C','D','E','F','G']) then
    ChartReg.BottomAxis.SetMinMax(0.0,MaxX);
  if (AnalType in ['3','8','A']) then
    ChartReg.BottomAxis.SetMinMax(MinX,MaxX);
  if (AnalType in ['3','8','A']) then
    ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
  ChartReg.Enabled := true;

  //Regression line
  j := 1;
  for i := 0 to 20 do
  begin
    t1 := StepSize*i;
    t2 := Slope*StepSize*i+Intercept;
    ChartReg.Series[iRegressionLine].AddXY(t1,t2);
    j := j+1;
  end;
  iRec := 1;
  ChartReg.Legend.Visible := cbLegend.Checked;

  IncrementAmount := ChartReg.Axes.Left.Increment;
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
    ChartReg.LeftAxis.Automatic := false;
    ChartReg.LeftAxis.Increment := IncrementAmount;
    ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
  end;
  Range := MaxY-MinY;
  ChartReg.Axes.Left.AxisValuesFormat := '####0.0##';
  if (Range <= 2.0) then ChartReg.Axes.Left.AxisValuesFormat := '###0.00';
  if (Range <= 0.70) then ChartReg.Axes.Left.AxisValuesFormat := '###0.000';
  if (Range <= 0.07) then ChartReg.Axes.Left.AxisValuesFormat := '###0.0000';
  if (Range <= 0.007) then ChartReg.Axes.Left.AxisValuesFormat := '###0.00000';
  if (Range <= 0.0007) then ChartReg.Axes.Left.AxisValuesFormat := '###0.000000';
  Range := MaxX-MinX;
  ChartReg.Axes.Bottom.AxisValuesFormat := '####0.0##';
  if (Range <= 0.7) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.00';
  if (Range <= 0.07) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.000';
  if (Range <= 0.007) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.0000';
  if (Range <= 0.0007) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.00000';
  if (Range <= 0.00007) then ChartReg.Axes.Bottom.AxisValuesFormat := '###0.000000';

  ChartReg.Foot.Caption := GeodateVersionStr;
  //if (EllipseMagnif = 1.0) then ChartReg.SubFoot.Caption := '1 sigma uncertainties';
  //if (EllipseMagnif > 1.0) then ChartReg.SubFoot.Caption := '95% conf. uncertainties';
  ChartReg.SubFoot.Visible := false;
  ChartReg.BottomAxis.SetMinMax(MinX,MaxX);
  ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
  if (AnalType in ['1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G']) then
  begin
    tMinX := ChartReg.BottomAxis.Minimum;
    tMinY := ChartReg.LeftAxis.Minimum;
    if (MinX < 0.0) then MinX := 0.0;
    if (MinY < 0.0) then MinY := 0.0;
    if (tMinX < 0.0) then MinX := 0.0;
    if (tMinY < 0.0) then MinY := 0.0;
    //ShowMessage('tMinX = '+FormatFloat('########0.00000',tMinX));
    //ShowMessage('tMinY = '+FormatFloat('########0.00000',tMinY));
    //ShowMessage('MinX = '+FormatFloat('########0.00000',MinX));
    //ShowMessage('MinY = '+FormatFloat('########0.00000',MinY));
  end;
  if (AnalType in ['1','2','4','5','6','7','9','B','C','D','E','F','G']) then
    ChartReg.BottomAxis.SetMinMax(0.0,MaxX);
  if (AnalType in ['3','8','A']) then
  begin
    ChartReg.BottomAxis.SetMinMax(MinX,MaxX);
    ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
  end;
end;


procedure TfmRegressionResult.dbnRegClick(Sender: TObject;
  Button: TNavigateBtn);
var
  tx, ty : double;
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
  tx := dmGdwtmp.cdsRegXRatio.AsFloat;
  ty := dmGdwtmp.cdsRegYRatio.AsFloat;
  ChartReg.Series[iCurrent].Clear;
  ChartReg.Series[iCurrent].AddXY(tx,ty);
  ChartReg.Enabled := true;
  if ((Residual[iRec,1] > (2.5 * ErrorWt[iRec,1])) or
      (Residual[iRec,2] > (2.5 * ErrorWt[iRec,2]))) then
  begin
    lResidual.Visible := true;
  end else
  begin
    lResidual.Visible := false;
  end;
end;

procedure TfmRegressionResult.VtChartRegClick(Sender: TObject);
begin
  try
    {do nothing}
    ShowMessage('Right click, not left click');
  except
    Exit;
  end;
end;

procedure TfmRegressionResult.VtChartRegPointSelected(Sender: TObject;
  var Series, DataPoint, MouseFlags, Cancel: Smallint);
begin
  try
    {do nothing}
  except
    ShowMessage('Right click, not left click');
  end;
end;

procedure TfmRegressionResult.cbRegressionLineClick(Sender: TObject);
begin
  if (cbRegressionLine.Checked) then
  begin
    ChartReg.Series[iRegressionLine].Visible := true;
  end
  else begin
    ChartReg.Series[iRegressionLine].Visible := false;
  end;
end;

procedure TfmRegressionResult.cbTicLabelsClick(Sender: TObject);
begin
  if (cbTicLabels.Checked) then
  begin
    ChartReg.Series[iCurveTic].Marks.Visible := true;
  end
  else begin
    ChartReg.Series[iCurveTic].Marks.Visible := false;
  end;
end;

procedure TfmRegressionResult.cbConcordiaUncertaintiesClick(Sender: TObject);
begin
  if (cbConcordiaUncertainties.Checked) then
  begin
    ChartReg.Series[iCurveLineMinus].Visible := true;
    ChartReg.Series[iCurveLinePlus].Visible := true;
  end
  else begin
    ChartReg.Series[iCurveLineMinus].Visible := false;
    ChartReg.Series[iCurveLinePlus].Visible := false;
  end;
end;

procedure TfmRegressionResult.cbCurrentSampleClick(Sender: TObject);
begin
  if (cbCurrentSample.Checked) then
  begin
    ChartReg.Series[iCurrent].Visible := true;
  end
  else begin
    ChartReg.Series[iCurrent].Visible := false;
  end;
end;

procedure TfmRegressionResult.cbErrorEllipsesClick(Sender: TObject);
var
  i : integer;
begin
  ChartReg.Series[iEllipsesIncluded].Clear;
  ChartReg.Series[iEllipsesExcluded].Clear;
  if (cbErrorEllipses.Checked) then
  begin
    lEllipseMagnif.Visible := true;
    ChooseEllipse := 'Y';
    if (Model = 4) then ChooseEllipse := 'A';
    ChartReg.Series[iEllipsesIncluded].Visible := true;
    ChartReg.Series[iEllipsesExcluded].Visible := true;
    for i := 1 to NumberOfPoints do
    begin
      if (PFlg[i] = 'Y') then DrawEllipse(i);
    end;
    //ChartReg.BottomAxis.SetMinMax(MinX,MaxX);
    //ChartReg.LeftAxis.SetMinMax(MinY,MaxY);
    if (EllipseMagnif = 1.0) then ChartReg.SubFoot.Caption := '1 sigma uncertainties';
    if (EllipseMagnif > 1.0) then ChartReg.SubFoot.Caption := '95% conf. uncertainties';
    ChartReg.SubFoot.Visible := true;
  end
  else begin
    lEllipseMagnif.Visible := false;
    ChooseEllipse := 'N';
    ChartReg.Series[iEllipsesIncluded].Visible := false;
    ChartReg.Series[iEllipsesExcluded].Visible := false;
    ChartReg.SubFoot.Visible := false;
  end;
end;

procedure TfmRegressionResult.cbErrorEnvelopeClick(Sender: TObject);
var
  i : integer;
  t1, t2, temp : double;
  TicsIncrement : double;
begin
  TicsIncrement := 40.0;
  ChartReg.Series[iEnvelopeLower].Clear;
  ChartReg.Series[iEnvelopeUpper].Clear;
  if (cbErrorEnvelope.Checked) then
  begin
    PlotErrorEnvelope := 'Y';
    //Lower error envelope
    ChartReg.Series[iEnvelopeLower].Visible := false;
    StartAtX := ChartReg.BottomAxis.Minimum;
    EndAtX := ChartReg.BottomAxis.Maximum;
    t1 := 5.0;
    TicsAtX := (EndAtX-StartAtX)/t1;
    i := 1;
    t1 := StartAtX;
    temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
         +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
    if (temp > 0.0) then
    begin
      t2:=NewIntercept+NewSlope*t1-Sqrt(temp);
      ChartReg.Series[iEnvelopeLower].AddXY(t1,t2);
      i := i + 1;
    end;
    repeat
      temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
           +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
      if (temp > 0.0) then
      begin
        t2:=NewIntercept+NewSlope*t1-Sqrt(temp);
        ChartReg.Series[iEnvelopeLower].AddXY(t1,t2);
        i := i + 1;
      end;
      t1:=t1+TicsAtX/TicsIncrement;
    until (t1 > EndAtX);
    //Upper error envelope
    ChartReg.Series[iEnvelopeUpper].Visible := false;
    i := 1;
    t1 := StartAtX;
    temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
         +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
    if (temp > 0.0) then
    begin
      t2:=NewIntercept+NewSlope*t1+Sqrt(temp);
      ChartReg.Series[iEnvelopeUpper].AddXY(t1,t2);
      i := i + 1;
    end;
    repeat
      temp:=T_Mult*NewInterceptError*T_Mult*NewInterceptError
           +(T_Mult*NewSlopeError*T_Mult*NewSlopeError*t1*(t1-2.0*Xcentroid));
      if (temp > 0.0) then
      begin
        t2:=NewIntercept+NewSlope*t1+Sqrt(temp);
        ChartReg.Series[iEnvelopeUpper].AddXY(t1,t2);
        i := i + 1;
      end;
      t1:=t1+TicsAtX/TicsIncrement;
    until (t1 > EndAtX);
    ChartReg.Series[iEnvelopeLower].Visible := true;
    ChartReg.Series[iEnvelopeUpper].Visible := true;
  end
  else begin
    PlotErrorEnvelope := 'N';
    ChartReg.Series[iEnvelopeLower].Visible := false;
    ChartReg.Series[iEnvelopeUpper].Visible := false;
  end;
end;

procedure TfmRegressionResult.cbLegendClick(Sender: TObject);
begin
  if (cbLegend.Checked) then
  begin
    ChartReg.Legend.Visible := true;
  end
  else begin
    ChartReg.Legend.Visible := false;
  end;
end;

procedure TfmRegressionResult.FormKeyDown(Sender: TObject; var Key: Word;
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
      dmGdwtmp.cdsReg.First;
      iRec := 1;
    end;
    vk_PgUp, vk_UpArrow : begin
      dmGdwtmp.cdsReg.Prior;
      iRec := iRec - 1;
      if (iRec < 1) then iRec := 1;
    end;
    vk_PgDn, vk_DownArrow : begin
      dmGdwtmp.cdsReg.Next;
      iRec := iRec + 1;
      if (iRec > dmGdwtmp.cdsReg.RecordCount)
        then iRec := dmGdwtmp.cdsReg.RecordCount;
    end;
    vk_End : begin
      dmGdwtmp.cdsReg.Last;
      iRec := dmGdwtmp.cdsReg.RecordCount;
    end;
  end;
end;


procedure TfmRegressionResult.sbReRegressClick(Sender: TObject);
begin
  AdjustForNegativeIntercept := false;
  cbErrorEllipses.Checked := false;
  cbErrorEnvelope.Checked := false;
  UpdateRFlg(Sender);
  bbUpdateClick(Sender);
  Regress_Data;
  with dmGdwtmp.cdsReg do
  begin
    Active := false;
  end;
  FillRegTable(Sender);
  SetAllChartData;
end;


procedure TfmRegressionResult.bbUpdateClick(Sender: TObject);
var
  i : integer;
  tx, ty : double;
begin
  UpdateRFlg(Sender);
  ChartReg.Series[iDataIncluded].Clear;
  ChartReg.Series[iDataExcluded].Clear;
  with dmGdwtmp.cdsReg do
  begin
    First;
    i := 1;
    repeat
      if (dmGdwtmp.cdsRegRFlag.AsString = 'Y') then
      begin
        tx := dmGdwtmp.cdsRegXRatio.AsFloat;
        ty := dmGdwtmp.cdsRegYRatio.AsFloat;
        ChartReg.Series[iDataIncluded].AddXY(tx,ty);
      end else
      begin
        if (dmGdwtmp.cdsRegPFlag.AsString = 'Y') then
        begin
          tx := dmGdwtmp.cdsRegXRatio.AsFloat;
          ty := dmGdwtmp.cdsRegYRatio.AsFloat;
          ChartReg.Series[iDataExcluded].AddXY(tx,ty);
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

procedure TfmRegressionResult.DrawEllipse ( i : integer);
const
  St           : double = 0.1;
  StepIncrement = 200;  //originally 10
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

procedure CalcLWt( J : integer);
begin
    case ErrTyp[J] of
      '1' : begin
              LWt[1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Lwt[2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '2' : begin
              Lwt[1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Lwt[2]:=ErrorWt[J,2];
            end;
      '3' : begin
              Lwt[1]:=ErrorWt[J,1];
              Lwt[2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '4' : begin
              Lwt[1]:=ErrorWt[J,1];
              Lwt[2]:=ErrorWt[J,2];
            end;
    end;//case
    if (Lwt[1]<=0.0) then Lwt[1]:=1.0e-5;
    if (Lwt[2]<=0.0) then Lwt[2]:=1.0e-5;
end;//procedure CalcLW

begin
  if (RFlg[i] = 'Y') then ChartReg.Series[iEllipsesIncluded].AddNullXY(0.0,0.0);
  if (RFlg[i] = 'N') then ChartReg.Series[iEllipsesExcluded].AddNullXY(0.0,0.0);
  if (AnalType8 = 'N') then
  begin
    if (EllipseMagnif > 1.0)
      then T_Mult_ell:=TMultiplier(1.0*N_Rep)
      else T_Mult_ell := 1.0;
    CalcLWt(i);
    XE:=LWt[1]*T_Mult_ell;
    YE:=LWt[2]*T_Mult_ell;
  end;
  if ((AnalType8 = 'U') or (AnalType8 = 'L')) then
  begin
    if (ChooseEllipse in ['Y','O']) then   //show original ellipses
    begin
      if (EllipseMagnif > 1.0)
        then T_Mult_ell:=TMultiplier(1.0*N_Rep)
        else T_Mult_ell := 1.0;
      CalcLWt(i);
      XE:=LWt[1]*T_Mult_ell;
      YE:=LWt[2]*T_Mult_ell;
    end;
    if (ChooseEllipse in ['A']) then        //show augmented ellipses
    begin
      if (EllipseMagnif > 1.0)
        then T_Mult_ell:=TMultiplier(1.0*N_Rep)
        else T_Mult_ell := 1.0;
      XE:=Wt[i,1]*T_Mult_ell;
      YE:=Wt[i,2]*T_Mult_ell;
      //ShowMessage(IntToStr(i)+'   '+FormatFloat('###0.0000',XE)+'   '+FormatFloat('###0.0000',YE));
    end;
  end;
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
               else A := 1.0;  //length of major axi
  if (B > 0.0) then B:=Sqrt(B)
               else B := 1.0;  //length of minor axis
  SinAngle:=Sin(Angle);
  CosAngle:=Cos(Angle);
  K:=1.0;
  J:=0;
  X:=K*A;
  St := X/(1.0*StepIncrement);
  if (St <= 0.0) then St := 1.0;
  repeat
    begin
      J:=J+1;
      Z:=1.0-(X/A)*(X/A);
      if (Z>=0.0) then Y:=K*B*Sqrt(Z)
                  else Y:=0.0;
      XP:=Ratio[i,1]+X*CosAngle-Y*SinAngle;
      YP:=Ratio[i,2]+X*SinAngle+Y*CosAngle;
      if (XP > MaxX) then MaxX := XP;
      if (XP < MinX) then MinX := XP;
      if (YP > MaxY) then MaxY := YP;
      if (YP < MinY) then MinY := YP;
      if (J=1) then
      begin
        if (RFlg[i] = 'Y') then ChartReg.Series[iEllipsesIncluded].AddXY(XP,YP);
        if (RFlg[i] = 'N') then ChartReg.Series[iEllipsesExcluded].AddXY(XP,YP);
      end;
      if (J=1) then
      begin
      	XPP:=XP;
      	YPP:=YP;
      end;
      if (RFlg[i] = 'Y') then ChartReg.Series[iEllipsesIncluded].AddXY(XP,YP);
      if (RFlg[i] = 'N') then ChartReg.Series[iEllipsesExcluded].AddXY(XP,YP);
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
      XP:=Ratio[i,1]+X*CosAngle-Y*SinAngle;
      YP:=Ratio[i,2]+X*SinAngle+Y*CosAngle;
      if (XP > MaxX) then MaxX := XP;
      if (XP < MinX) then MinX := XP;
      if (YP > MaxY) then MaxY := YP;
      if (YP < MinY) then MinY := YP;
      if (RFlg[i] = 'Y') then ChartReg.Series[iEllipsesIncluded].AddXY(XP,YP);
      if (RFlg[i] = 'N') then ChartReg.Series[iEllipsesExcluded].AddXY(XP,YP);
    end;
    if (J < 5) then X:=X-K*St
               else X:=X-K*St*2.0;
  until (X > (-(K*A)));

  J:=J+1;
  X := -K*A;
  Z:=1.0-(X/A)*(X/A);
  if (Z>=0.0) then Y:=K*B*Sqrt(Z)
              else Y:=0.0;
  XP:=Ratio[i,1]+X*CosAngle-Y*SinAngle;
  YP:=Ratio[i,2]+X*SinAngle+Y*CosAngle;
  if (XP > MaxX) then MaxX := XP;
  if (XP < MinX) then MinX := XP;
  if (YP > MaxY) then MaxY := YP;
  if (YP < MinY) then MinY := YP;
  if (RFlg[i] = 'Y') then ChartReg.Series[iEllipsesIncluded].AddXY(XP,YP);
  if (RFlg[i] = 'N') then ChartReg.Series[iEllipsesExcluded].AddXY(XP,YP);
end;

procedure TfmRegressionResult.SprdSheetEllipse (Xls : TExcelFile; i : integer);
const
  St           : double = 0.1;
  StepIncrement = 200; //originally 15
  Minus100 : double = -100.0;
var
  MaxX, MinX, MaxY, MinY : double;
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
  iRow, iCol : integer;

procedure CalcLWt( J : integer);
begin
    case ErrTyp[J] of
      '1' : begin
              LWt[1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Lwt[2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '2' : begin
              Lwt[1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Lwt[2]:=ErrorWt[J,2];
            end;
      '3' : begin
              Lwt[1]:=ErrorWt[J,1];
              Lwt[2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '4' : begin
              Lwt[1]:=ErrorWt[J,1];
              Lwt[2]:=ErrorWt[J,2];
            end;
    end;{case}
    if (Lwt[1]<=0.0) then Lwt[1]:=1.0e-5;
    if (Lwt[2]<=0.0) then Lwt[2]:=1.0e-5;
end;{procedure CalcLWt}

begin
  MaxX := GetAxisValuesForm.XMax;
  MinX := GetAxisValuesForm.XMin;
  MaxY := GetAxisValuesForm.YMax;
  MinY := GetAxisValuesForm.YMin;
  if (AnalType8 = 'N') then
  begin
    if (EllipseMagnif > 1.0)
      then T_Mult_ell:=TMultiplier(1.0*N_Rep)
      else T_Mult_ell := 1.0;
    CalcLWt(i);
    XE:=LWt[1]*T_Mult_ell;
    YE:=LWt[2]*T_Mult_ell;
  end;
  if ((AnalType8 = 'U') or (AnalType8 = 'L')) then
  begin
    if CharInSet(ChooseEllipse,['Y','O']) then
    begin
      if (EllipseMagnif > 1.0)
        then T_Mult_ell:=TMultiplier(1.0*N_Rep)
        else T_Mult_ell := 1.0;
      CalcLWt(i);
      XE:=LWt[1]*T_Mult_ell;
      YE:=LWt[2]*T_Mult_ell;
    end;
    if CharInSet(ChooseEllipse,['A']) then
    begin
      if (EllipseMagnif > 1.0)
        then T_Mult_ell:=T_Mult
        else T_Mult_ell := 1.0;
      XE:=Wt[i,1]*T_Mult_ell;
      YE:=Wt[i,2]*T_Mult_ell;
    end;
  end;
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
  St := X/(1.0*StepIncrement);
  if (St <= 0.0) then St := 1.0;
  repeat
    begin
      J:=J+1;
      Z:=1.0-(X/A)*(X/A);
      if (Z>=0.0) then Y:=K*B*Sqrt(Z)
                  else Y:=0.0;
      XP:=Ratio[i,1]+X*CosAngle-Y*SinAngle;
      YP:=Ratio[i,2]+X*SinAngle+Y*CosAngle;
      if (J=1) then
      begin
        iRow := RowNumber;
        iCol := 27;
        tmpStr := SmpNo[i];
         Xls.SetCellValue(iRow,iCol,tmpStr);
        if (RFlg[i] = 'Y') then iCol := 25
                           else iCol := 28;
        //tmpStr := FloatToStr(XP);
        Xls.SetCellValue(iRow,iCol,XP);
        if (RFlg[i] = 'Y') then iCol := 26
                           else iCol := 29;
        //tmpStr := FloatToStr(YP);
        Xls.SetCellValue(iRow,iCol,YP);
        RowNumber := RowNumber + 1;
      end;
      if (J=1) then
      begin
    	XPP:=XP;
    	YPP:=YP;
      end;
        iRow := RowNumber;
        iCol := 27;
        tmpStr := SmpNo[i];
        Xls.SetCellValue(iRow,iCol,tmpStr);
        if (RFlg[i] = 'Y') then iCol := 25
                           else iCol := 28;
        //tmpStr := FloatToStr(XP);
        Xls.SetCellValue(iRow,iCol,XP);
        if (RFlg[i] = 'Y') then iCol := 26
                           else iCol := 29;
        //tmpStr := FloatToStr(YP);
        Xls.SetCellValue(iRow,iCol,YP);
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
      XP:=Ratio[i,1]+X*CosAngle-Y*SinAngle;
      YP:=Ratio[i,2]+X*SinAngle+Y*CosAngle;
      iRow := RowNumber;
      iCol := 27;
      tmpStr := SmpNo[i];
      Xls.SetCellValue(iRow,iCol,tmpStr);
      if (RFlg[i] = 'Y') then iCol := 25
                         else iCol := 28;
      //tmpStr := FloatToStr(XP);
      Xls.SetCellValue(iRow,iCol,XP);
      if (RFlg[i] = 'Y') then iCol := 26
                         else iCol := 29;
      //tmpStr := FloatToStr(YP);
      Xls.SetCellValue(iRow,iCol,YP);
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
  XP:=Ratio[i,1]+X*CosAngle-Y*SinAngle;
  YP:=Ratio[i,2]+X*SinAngle+Y*CosAngle;
  iRow := RowNumber;
  iCol := 27;
  tmpStr := SmpNo[i];
  Xls.SetCellValue(iRow,iCol,tmpStr);
  if (RFlg[i] = 'Y') then iCol := 25
                     else iCol := 28;
  //tmpStr := FloatToStr(XP);
  Xls.SetCellValue(iRow,iCol,XP);
  if (RFlg[i] = 'Y') then iCol := 26
                     else iCol := 29;
  //tmpStr := FloatToStr(YP);
  Xls.SetCellValue(iRow,iCol,YP);
  RowNumber := RowNumber + 1;

  iRow := RowNumber;
  iCol := 27;
  tmpStr := SmpNo[i];
  Xls.SetCellValue(iRow,iCol,tmpStr);
  if (RFlg[i] = 'Y') then iCol := 25
                     else iCol := 28;
  //tmpStr := FloatToStr(XP);
  Xls.SetCellValue(iRow,iCol,XP);
  if (RFlg[i] = 'Y') then iCol := 26
                     else iCol := 29;
  //tmpStr := FloatToStr(YP);
  Xls.SetCellValue(iRow,iCol,YP);
  RowNumber := RowNumber + 1;
  iRow := RowNumber;
  if (RFlg[i] = 'Y') then iCol := 25
                     else iCol := 28;
  //tmpStr := FloatToStr(-100.0);
  Xls.SetCellValue(iRow,iCol,Minus100);
  if (RFlg[i] = 'Y') then iCol := 26
                     else iCol := 29;
  //tmpStr := FloatToStr(-100.0);
  Xls.SetCellValue(iRow,iCol,Minus100);
  RowNumber := RowNumber + 1;
end;

procedure TfmRegressionResult.TreeView1Click(Sender: TObject);
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
  ChartReg.Series[iCurrent].Clear;
  ChartReg.Series[iCurrent].AddXY(tx,ty);
  ChartReg.Enabled := true;
end;

procedure TfmRegressionResult.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  {
  GraphColour[1,1] := VtChartReg.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Red;
  GraphColour[1,2] := VtChartReg.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Blue;
  GraphColour[1,3] := VtChartReg.Plot.SeriesCollection.Item[1].DataPoints.Item[1].Marker.Pen.VtColor.Green;
  GraphColour[2,1] := VtChartReg.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Red;
  GraphColour[2,2] := VtChartReg.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Blue;
  GraphColour[2,3] := VtChartReg.Plot.SeriesCollection.Item[3].DataPoints.Item[1].Marker.Pen.VtColor.Green;
  GraphColour[3,1] := VtChartReg.Plot.SeriesCollection.Item[7].Pen.VtColor.Red;
  GraphColour[3,2] := VtChartReg.Plot.SeriesCollection.Item[7].Pen.VtColor.Blue;
  GraphColour[3,3] := VtChartReg.Plot.SeriesCollection.Item[7].Pen.VtColor.Green;
  GraphColour[4,1] := VtChartReg.Plot.SeriesCollection.Item[9].Pen.VtColor.Red;
  GraphColour[4,2] := VtChartReg.Plot.SeriesCollection.Item[9].Pen.VtColor.Blue;
  GraphColour[4,3] := VtChartReg.Plot.SeriesCollection.Item[9].Pen.VtColor.Green;
  GraphColour[5,1] := VtChartReg.Plot.SeriesCollection.Item[5].Pen.VtColor.Red;
  GraphColour[5,2] := VtChartReg.Plot.SeriesCollection.Item[5].Pen.VtColor.Blue;
  GraphColour[5,3] := VtChartReg.Plot.SeriesCollection.Item[5].Pen.VtColor.Green;
  GraphColour[6,1] := VtChartReg.Plot.SeriesCollection.Item[13].Pen.VtColor.Red;
  GraphColour[6,2] := VtChartReg.Plot.SeriesCollection.Item[13].Pen.VtColor.Blue;
  GraphColour[6,3] := VtChartReg.Plot.SeriesCollection.Item[13].Pen.VtColor.Green;
  GraphColour[7,1] := VtChartReg.Plot.SeriesCollection.Item[17].Pen.VtColor.Red;
  GraphColour[7,2] := VtChartReg.Plot.SeriesCollection.Item[17].Pen.VtColor.Blue;
  GraphColour[7,3] := VtChartReg.Plot.SeriesCollection.Item[17].Pen.VtColor.Green;
  }
end;

end.
