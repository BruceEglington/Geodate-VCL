unit gd_reslt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DBCtrls, Grids, DBGrids, Buttons, Db, DBTables,
  ComCtrls;

type
  TfmResults = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    dblSuite: TDBLookupComboBox;
    dblLithology: TDBLookupComboBox;
    lSuite: TLabel;
    lLithology: TLabel;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    Label1: TLabel;
    sbInsertSuite: TSpeedButton;
    sbInsertLithology: TSpeedButton;
    Label2: TLabel;
    sbInsertTechnique: TSpeedButton;
    dblTechnique: TDBLookupComboBox;
    Label3: TLabel;
    dblCountry: TDBLookupComboBox;
    sbLookup: TSpeedButton;
    Label4: TLabel;
    dblInterpretation: TDBLookupComboBox;
    InsertInterpretation: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Label5: TLabel;
    ePolygonNo: TEdit;
    eComment: TMemo;
    Label6: TLabel;
    dblEquipment: TDBLookupComboBox;
    sbInsertEquipment: TSpeedButton;
    Panel3: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Panel4: TPanel;
    Label7: TLabel;
    dblMaterial: TDBLookupComboBox;
    SpeedButton1: TSpeedButton;
    procedure bbOKClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbLookupClick(Sender: TObject);
    procedure dblCountryExit(Sender: TObject);
    procedure dblSuiteExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    GDWSender : string;
  end;

var
  fmResults: TfmResults;

implementation

{$R *.DFM}

uses
  gdw_varb, dmDV, gd_MSWD, Gd_lkup;

var
  LookupForm    : TfmLookUp;

procedure TfmResults.bbOKClick(Sender: TObject);
var
  i      : integer;
  tmpDiff : double;
begin
  if (dblCountry.Text = '') then
  begin
    MessageDlg('Specify a value for country',mtWarning,[mbOK],0);
    Exit;
  end;
  if (dblSuite.Text = '') then
  begin
    MessageDlg('Specify a value for unit',mtWarning,[mbOK],0);
    Exit;
  end;
  if (dblLithology.Text = '') then
  begin
    MessageDlg('Specify a value for lithology',mtWarning,[mbOK],0);
    Exit;
  end;
  if (dblTechnique.Text = '') then
  begin
    MessageDlg('Specify a value for technique',mtWarning,[mbOK],0);
    Exit;
  end;
  if (dblInterpretation.Text = '') then
  begin
    MessageDlg('Specify a value for interpretation',mtWarning,[mbOK],0);
    Exit;
  end;

  if (GDWSender = 'Regress') then
  begin
    with dmDVResults do
    begin
      IsoResults.Append;
      IsoResultsCountryAbr.AsString := dblCountry.KeyValue;
      IsoResultsUnitName.AsString := dblSuite.KeyValue;
      IsoResultsLithology.AsString := dblLithology.KeyValue;
      IsoResultsTechAbr.AsString := dblTechnique.KeyValue;
      IsoResultsInterpAbr.AsString := dblInterpretation.KeyValue;
      IsoResultsIsotopeSystem.AsString := ProcessAbr[IAnalTyp];
      if (dblEquipment.Text <> '') then
        IsoResultsApproachAbr.AsString := dblEquipment.KeyValue;
        {
      IsoResultsPolygonNo.AsString := ePolygonNo.Text;
      }
      IsoResultsDateString.AsString := DateString;
      {
      IsoResultsXCentroid.AsFloat := XCentroid;
      IsoResultsYCentroid.AsFloat := YCentroid;
      }
      case AnalType of
        '1','2','4','5','6','7','9','B','E','F','G' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[IAnalTyp];
        end;
        '3','8','A','H' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[4];
          IsoResultsDecayConst2.AsFloat := DecayConst[5];
        end;
        'C','D' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[11];
          IsoResultsDecayConst2.AsFloat := DecayConst[12];
        end;
      end;
      IsoResultsSigmaPercentDC1.AsFloat := 0.0;
      if (AnalType in ['1','2','4','5','6','7','9','B','C','D','E','F','G']) then
      begin
        IsoResultsAge.AsFloat := Age;
        IsoResultsAgePError.AsFloat := AgeError;
        IsoResultsAgeMError.AsFloat := AgeError;
        {
        IsoResultsInitRatio.AsFloat := Intercept;
        IsoResultsInitRatioError.AsFloat := InitRatioError;
        if (AnalType in ['1','2','7','9','E','F','G']) then
        begin
          IsoResultsEpsilon.AsFloat := Epsilon1;
          IsoResultsEpsilonError.AsFloat := EpError1;
        end;
        }
      end;
      if (AnalType in ['3']) then
      begin
        IsoResultsAge.AsFloat := Age;
        IsoResultsAgePError.AsFloat := UpperAgeError;
        IsoResultsAgeMError.AsFloat := LowerAgeError;
        {
        IsoResultsMu.AsFloat := Mu;
        IsoResultsMuPError.AsFloat := UprMuError;
        IsoResultsMuMError.AsFloat := LwrMuError;
        case mu_choice of
          0 : IsoResultsMumodel.AsString := 'Single stage';
          1 : IsoResultsMumodel.AsString := 'S&K 2 stage';
          2 : IsoResultsMumodel.AsString := 'User defined';
        end;
        }
      end;
      if (AnalType in ['8','A']) then
      begin
        case AnalType8 of
          'L' : begin
            IsoResultsAge.AsFloat := LwrIntercept;
            IsoResultsAgePError.AsFloat := LwrUprAgeError;
            IsoResultsAgeMError.AsFloat := LwrLwrAgeError;
            {
            IsoResultsLwrIntercept.AsFloat := UprIntercept;
            IsoResultsLwrPError.AsFloat := UprUprAgeError;
            IsoResultsLwrMError.AsFloat := UprLwrAgeError;
            IsoResultsWeighting.AsString := 'Weighted for lower intercept';
            IsoResultsLudwigp.AsFloat := Lud_pp;
            }
          end;
          'N' : begin
            IsoResultsAge.AsFloat := UprIntercept;
            IsoResultsAgePError.AsFloat := UprUprAgeError;
            IsoResultsAgeMError.AsFloat := UprLwrAgeError;
            {
            IsoResultsLwrIntercept.AsFloat := LwrIntercept;
            IsoResultsLwrPError.AsFloat := LwrUprAgeError;
            IsoResultsLwrMError.AsFloat := LwrLwrAgeError;
            IsoResultsWeighting.AsString := 'Normal weighting';
            IsoResultsLudwigp.AsString := '';
            }
            if ((AnalType = '8')
              and(UprIntercept <> UprUprAgeError2)
              and (not ConstrainFlag)) then
            begin
              IsoResultsAge.AsFloat := UprUprAgeError2;
              tmpDiff := UprUprAgeError2-UprIntercept;
              IsoResultsAgePError.AsFloat := UprUprAgeError-tmpDiff;
              IsoResultsAgeMError.AsFloat := UprLwrAgeError+tmpDiff;
            end;
          end;
          'U' : begin
            IsoResultsAge.AsFloat := UprIntercept;
            IsoResultsAgePError.AsFloat := UprUprAgeError;
            IsoResultsAgeMError.AsFloat := UprLwrAgeError;
            {
            IsoResultsLwrIntercept.AsFloat := LwrIntercept;
            IsoResultsLwrPError.AsFloat := LwrUprAgeError;
            IsoResultsLwrMError.AsFloat := LwrLwrAgeError;
            IsoResultsWeighting.AsString := 'Weighted for upper intercept';
            IsoResultsLudwigp.AsFloat := Lud_pp;
            }
            if ((AnalType = '8')
              and(UprIntercept <> UprUprAgeError2)
              and (not ConstrainFlag)) then
            begin
              IsoResultsAge.AsFloat := UprUprAgeError2;
              tmpDiff := UprUprAgeError2-UprIntercept;
              IsoResultsAgePError.AsFloat := UprUprAgeError-tmpDiff;
              IsoResultsAgeMError.AsFloat := UprLwrAgeError+tmpDiff;
            end;
          end;
        end;
      end;
      if (AnalType in ['H']) then
      begin
        IsoResultsAge.AsFloat := Age;
        IsoResultsAgePError.AsFloat := AgeError;
        IsoResultsAgeMError.AsFloat := AgeError;
      end;
      {
      IsoResultsMSWD.AsFloat := Msum;
      IsoResultsProbOfFit.AsFloat := ProbabilityOfFit;
      }
      IsoResultsPrefLevel.AsInteger := 1;
      {
      case Model of
        1 : begin
          if Msum>MsumCutOff then
          begin
            IsoResultsAugmentation.AsString := 'Errors augmented by Sqrt(MSWD/F)';
            IsoResultsPrefLevel.AsInteger := 1;
          end;
          if Msum<=MsumCutOff then
          begin
            IsoResultsAugmentation.AsString := '';
            IsoResultsPrefLevel.AsInteger := 2;
          end;
        end;
        2 : IsoResultsAugmentation.AsString := 'No assumptions - equal weights, r=0';
        3 : IsoResultsAugmentation.AsString := 'Assuming variable initial ratio';
        4 : IsoResultsAugmentation.AsString := 'Assuming multi-episodic scatter';
        5 : IsoResultsAugmentation.AsString := 'Assuming separate anal. and geol errors';
        6 : IsoResultsAugmentation.AsString := 'Errors not augmented';
      end;
      }
      {
      IsoResultsNReg.AsFloat := NumberOfPointsRegressed;
      if (AnalType8 in ['U','L']) then
      begin
        if (Model in [4]) then
         IsoResultsNReg.AsFloat := NEquivPtsRegressed;
      end;
      IsoResultsNSamples.AsFloat := NumberOfPoints;
      IsoResultsAlpha.AsFloat := Falpha;
      IsoResultsNReplicates.AsFloat := N_Rep;
      IsoResultsDegFreedom.AsFloat := NumberOfPointsRegressed - 2.0;
      IsoResultsFCutoff.AsFloat := MsumCutOff;
      if (ConstrainFlag) then
      begin
        IsoResultsConstraints.AsString := 'Constrained';
        IsoResultsXConstrain.AsFloat := XConstrain;
        IsoResultsYConstrain.AsFloat := YConstrain;
        IsoResultsAgeConstrain.AsFloat := AgeConstrain;
      end
      else begin
        IsoResultsConstraints.AsString := 'Unconstrained';
      end;
      }
      {
      IsoResultsProgName.AsString := ProgramName+' '+ProgVersion;
      }
      IsoResultsRegisteredUser.AsString := RegisteredUser;
      IsoResultsComment.AsString := eComment.Text;
      IsoResults.Post;
      (*
      for i := 1 to NumberOfPoints do
      begin
        SmpReg.Append;
        SmpRegRegressionNumber.AsInteger := IsoResultsRegressionNumber.AsInteger;
        SmpRegSampleNo.AsString := SmpNo[i];
        SmpRegRegressed.AsString := RFlg[i];
        if ((Latitude[i] <> 0.0) and (Longitude[i] <> 0.0)) then
        begin
          SmpRegLatitude.AsFloat := Latitude[i];
          SmpRegLongitude.AsFloat := Longitude[i];
        end;
        {
        if ((Elevation[i] <> 0.0)) then
        begin
          SmpRegElevation.AsFloat := Elevation[i];
        end;
        }
        SmpReg.Next;
      end;
      *)
      IsoResults.Next;
    end;
  end;

  if (GDWSender = 'WtAv') then
  begin
    with dmDVResults do
    begin
      IsoResults.Append;
      IsoResultsCountryAbr.AsString := dblCountry.KeyValue;
      IsoResultsUnitName.AsString := dblSuite.KeyValue;
      IsoResultsLithology.AsString := dblLithology.KeyValue;
      IsoResultsTechAbr.AsString := dblTechnique.KeyValue;
      IsoResultsInterpAbr.AsString := dblInterpretation.KeyValue;
      IsoResultsIsotopeSystem.AsString := ProcessAbr[IAnalTyp];
      if (dblEquipment.Text <> '') then
        IsoResultsApproachAbr.AsString := dblEquipment.KeyValue;
      IsoResultsPolygonNo.AsString := ePolygonNo.Text;
      IsoResultsDateString.AsString := DateString;
      IsoResultsXCentroid.AsString := '';
      IsoResultsYCentroid.AsString := '';
      case AnalType of
        '1','2','4','5','6','7','9','B','E','F','G' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[IAnalTyp];
        end;
        '3','8','A','H' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[4];
          IsoResultsDecayConst2.AsFloat := DecayConst[5];
        end;
        'C','D' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[11];
          IsoResultsDecayConst2.AsFloat := DecayConst[12];
        end;
      end;
      IsoResultsSigmaPercentDC1.AsFloat := 0.0;
      if (AnalType in ['1','2','4','5','6','7','9','B','C','D','E','F','G']) then
      begin
        IsoResultsAge.AsFloat := Age;
        IsoResultsAgePError.AsFloat := AgeError;
        IsoResultsAgeMError.AsFloat := AgeError;
          IsoResultsInitRatio.AsFloat := Intercept;
          IsoResultsInitRatioError.AsFloat := InitRatioError;
          if (AnalType in ['1','2','7','9','E','F','G']) then
          begin
            IsoResultsEpsilon.AsFloat := Epsilon1;
            IsoResultsEpsilonError.AsFloat := EpError1;
          end;
      end;
      if (AnalType in ['3']) then
      begin
        IsoResultsAge.AsFloat := Age;
        IsoResultsAgePError.AsFloat := UpperAgeError;
        IsoResultsAgeMError.AsFloat := LowerAgeError;
        IsoResultsMu.AsFloat := Mu;
        IsoResultsMuPError.AsFloat := UprMuError;
        IsoResultsMuMError.AsFloat := LwrMuError;
        case mu_choice of
          0 : IsoResultsMumodel.AsString := 'Single stage';
          1 : IsoResultsMumodel.AsString := 'S&K 2 stage';
          2 : IsoResultsMumodel.AsString := 'User defined';
        end;
      end;
      if (AnalType in ['8','A']) then
      begin
        case AnalType8 of
          'N' : begin
            IsoResultsAge.AsFloat := UprIntercept;
            IsoResultsAgePError.AsFloat := UprUprAgeError;
            IsoResultsAgeMError.AsFloat := UprLwrAgeError;
            IsoResultsWeighting.AsString := 'Normal weighting';
            IsoResultsLudwigp.AsString := '';
          end;
          'U' : begin
            IsoResultsAge.AsFloat := UprIntercept;
            IsoResultsAgePError.AsFloat := UprUprAgeError;
            IsoResultsAgeMError.AsFloat := UprLwrAgeError;
            IsoResultsWeighting.AsString := 'Weighted for upper intercept';
            if (Lud_pp > 0.0) then IsoResultsLudwigp.AsFloat := Lud_pp;
          end;
        end;
        IsoResultsLwrIntercept.AsString := '';
        IsoResultsLwrPError.AsString := '';
        IsoResultsLwrMError.AsString := '';
      end;
      if (AnalType in ['H']) then
      begin
        IsoResultsAge.AsFloat := Age;
        IsoResultsAgePError.AsFloat := AgeError;
        IsoResultsAgeMError.AsFloat := AgeError;
      end;
      if (Msum > 0.0) then IsoResultsMSWD.AsFloat := Msum;
      IsoResultsProbOfFit.AsFloat := ProbabilityOfFit;
      IsoResultsPrefLevel.AsInteger := 1;
      case Model of
        1 : begin
          if Msum>MsumCutOff then
          begin
            IsoResultsAugmentation.AsString := 'Errors augmented by Sqrt(MSWD/F)';
            IsoResultsPrefLevel.AsInteger := 1;
          end;
          if Msum<=MsumCutOff then
          begin
            IsoResultsAugmentation.AsString := '';
            IsoResultsPrefLevel.AsInteger := 2;
          end;
        end;
        2 : IsoResultsAugmentation.AsString := 'No assumptions - equal weights, r=0';
        3 : IsoResultsAugmentation.AsString := 'Assuming variable initial ratio';
        4 : IsoResultsAugmentation.AsString := 'Assuming multi-episodic scatter';
        5 : IsoResultsAugmentation.AsString := 'Assuming separate anal. and geol errors';
        6 : IsoResultsAugmentation.AsString := 'Errors not augmented';
      end;
      if (NumberOfPointsRegressed > 0) then IsoResultsNReg.AsFloat := NumberOfPointsRegressed;
      if (AnalType8 in ['U','L']) then
      begin
        if (Model in [4]) then
         if (NEquivPtsRegressed > 0.0) then IsoResultsNReg.AsFloat := NEquivPtsRegressed;
      end;
      if (NumberOfPoints > 0) then IsoResultsNSamples.AsFloat := NumberOfPoints;
      if (Falpha > 0.0) then IsoResultsAlpha.AsFloat := Falpha;
      if (N_Rep > 0) then IsoResultsNReplicates.AsFloat := N_Rep;
      if (NumberOfPointsRegressed > 1) then IsoResultsDegFreedom.AsFloat := NumberOfPointsRegressed - 2.0;
      if (MsumCutOff > 0.0) then IsoResultsFCutoff.AsFloat := MsumCutOff;
      IsoResultsConstraints.AsString := 'Constrained';
      if (ConstrainFlag) then
      begin
        IsoResultsXConstrain.AsFloat := 0.0;
        IsoResultsYConstrain.AsFloat := 0.0;
        IsoResultsAgeConstrain.AsFloat := 0.0;
      end;
      IsoResultsProgName.AsString := ProgramName+' '+ProgVersion;
      IsoResultsRegisteredUser.AsString := RegisteredUser;
      IsoResultsComment.AsString := eComment.Text;
      IsoResults.Post;
      for i := 1 to NumberOfPoints do
      begin
        SmpReg.Append;
        SmpRegRegressionNumber.AsInteger := IsoResultsRegressionNumber.AsInteger;
        SmpRegSampleNo.AsString := SmpNo[i];
        SmpRegRegressed.AsString := RFlg[i];
        if ((Latitude[i] <> 0.0) and (Longitude[i] <> 0.0)) then
        begin
          SmpRegLatitude.AsFloat := Latitude[i];
          SmpRegLongitude.AsFloat := Longitude[i];
        end;
        SmpReg.Next;
      end;
      IsoResults.Next;
    end;
  end;

  if (GDWSender = 'Ccda') then
  begin
    with dmDVResults do
    begin
      IsoResults.Append;
      IsoResultsCountryAbr.AsString := dblCountry.KeyValue;
      IsoResultsSuitName.AsString := dblSuite.KeyValue;
      IsoResultsLitholName.AsString := dblLithology.KeyValue;
      IsoResultsTechAbstr.AsString := dblTechnique.KeyValue;
      IsoResultsInterpAbr.AsString := dblInterpretation.KeyValue;
      IsoResultsIsotopeSystem.AsString := ProcessAbr[IAnalTyp];
      IsoResultsPolygonNo.AsString := ePolygonNo.Text;
      if (dblEquipment.Text <> '') then
        IsoResultsEquipment.AsString := dblEquipment.KeyValue;
      IsoResultsDateString.AsString := DateString;
      IsoResultsXCentroid.AsFloat := XCentroid;
      IsoResultsYCentroid.AsFloat := YCentroid;
      case AnalType of
        '8','A' : begin
          IsoResultsDecayConst1.AsFloat := DecayConst[4];
          IsoResultsDecayConst2.AsFloat := DecayConst[5];
          IsoResultsSigmaPercentDC1.AsFloat := 0.0;
          IsoResultsSigmaPercentDC2.AsString := '';
        end;
      end;
      if (AnalType in ['8','A']) then
      begin
            IsoResultsAge.AsFloat := DateWO/1.0e6;
            IsoResultsAgePError.AsFloat := T_Mult*Sqrt(VarDateWO)/1.0e6;
            IsoResultsAgeMError.AsFloat := T_Mult*Sqrt(VarDateWO)/1.0e6;
            IsoResultsWeighting.AsString := 'Normal weighting';
            IsoResultsLudwigp.AsString := '';
        IsoResultsLwrIntercept.AsString := '';
        IsoResultsLwrPError.AsString := '';
        IsoResultsLwrMError.AsString := '';
      end;
      if (ProbabilityOfFitWO >= Falpha) then
      begin
        IsoResultsPrefLevel.AsInteger := 2;
      end else
      begin
        IsoResultsPrefLevel.AsInteger := 1;
      end;
      if (MSWDwo > 0.0) then IsoResultsMSWD.AsFloat := MSWDwo;
      IsoResultsProbOfFit.AsFloat := ProbabilityOfFitWO;
      IsoResultsAugmentation.AsString := 'Excluding decay constant uncertainties';
      if (NumberOfPointsRegressed > 0) then IsoResultsNReg.AsFloat := NumberOfPointsRegressed;
      if (NumberOfPoints > 0) then IsoResultsNSamples.AsFloat := NumberOfPoints;
      if (Falpha > 0.0) then IsoResultsAlpha.AsFloat := Falpha;
      if (N_Rep > 0) then IsoResultsNReplicates.AsFloat := N_Rep;
      if (NumberOfPointsRegressed > 1) then IsoResultsDegFreedom.AsFloat := 2*NumberOfPointsRegressed - 1.0;
      if (MsumCutOff > 0.0) then IsoResultsFCutoff.AsFloat := MsumCutOff;
      IsoResultsConstraints.AsString := '';
      IsoResultsXConstrain.AsString := '';
      IsoResultsYConstrain.AsString := '';
      IsoResultsAgeConstrain.AsString := '';
      IsoResultsProgName.AsString := ProgramName+' '+ProgVersion;
      IsoResultsRegisteredUser.AsString := RegisteredUser;
      IsoResultsComment.AsString := eComment.Text;
      IsoResults.Post;
      for i := 1 to NumberOfPoints do
      begin
        try
          SmpReg.Append;
          SmpRegRegressionNumber.AsInteger := IsoResultsRegressionNumber.AsInteger;
          SmpRegSampleNo.AsString := SmpNo[i];
          SmpRegRegressed.AsString := RFlg[i];
          if ((Latitude[i] <> 0.0) and (Longitude[i] <> 0.0)) then
          begin
            SmpRegLatitude.AsFloat := Latitude[i];
            SmpRegLongitude.AsFloat := Longitude[i];
          end;
          SmpReg.Post;
        except
        end;
      end;
      IsoResults.Next;
      {now add result with decay constant uncertainties}
      IsoResults.Append;
      IsoResultsCountryAbr.AsString := dblCountry.KeyValue;
      IsoResultsSuitName.AsString := dblSuite.KeyValue;
      IsoResultsLitholName.AsString := dblLithology.KeyValue;
      IsoResultsTechAbstr.AsString := dblTechnique.KeyValue;
      IsoResultsInterpAbr.AsString := dblInterpretation.KeyValue;
      if (dblEquipment.Text <> '') then
        IsoResultsEquipment.AsString := dblEquipment.KeyValue;
      IsoResultsIsotopeSystem.AsString := ProcessAbr[IAnalTyp];
      IsoResultsPolygonNo.AsString := ePolygonNo.Text;
      IsoResultsDateString.AsString := DateString;
      IsoResultsXCentroid.AsFloat := XCentroid;
      IsoResultsYCentroid.AsFloat := YCentroid;
          IsoResultsDecayConst1.AsFloat := DecayConst[4];
          IsoResultsDecayConst2.AsFloat := DecayConst[5];
          IsoResultsSigmaPercentDC1.AsFloat := DecayConstUncertainty[4];
          IsoResultsSigmaPercentDC2.AsFloat := DecayConstUncertainty[5];
      if (AnalType in ['8','A']) then
      begin
            IsoResultsAge.AsFloat := DateW/1.0e6;
            IsoResultsAgePError.AsFloat := T_Mult*Sqrt(VarDateW)/1.0e6;
            IsoResultsAgeMError.AsFloat := T_Mult*Sqrt(VarDateW)/1.0e6;
            IsoResultsWeighting.AsString := 'Normal weighting';
            IsoResultsLudwigp.AsString := '';
        IsoResultsLwrIntercept.AsString := '';
        IsoResultsLwrPError.AsString := '';
        IsoResultsLwrMError.AsString := '';
      end;
      if (ProbabilityOfFitW >= Falpha) then
      begin
        IsoResultsPrefLevel.AsInteger := 2;
      end else
      begin
        IsoResultsPrefLevel.AsInteger := 1;
      end;
      if (MSWDw > 0.0) then IsoResultsMSWD.AsFloat := MSWDw;
      IsoResultsProbOfFit.AsFloat := ProbabilityOfFitW;
      IsoResultsAugmentation.AsString := 'Including decay constant uncertainties';
      if (NumberOfPointsRegressed > 0) then IsoResultsNReg.AsFloat := NumberOfPointsRegressed;
      if (NumberOfPoints > 0) then IsoResultsNSamples.AsFloat := NumberOfPoints;
      if (Falpha > 0.0) then IsoResultsAlpha.AsFloat := Falpha;
      if (N_Rep > 0) then IsoResultsNReplicates.AsFloat := N_Rep;
      if (NumberOfPointsRegressed > 1) then IsoResultsDegFreedom.AsFloat := 2*NumberOfPointsRegressed - 1.0;
      if (MsumCutOff > 0.0) then IsoResultsFCutoff.AsFloat := MsumCutOff;
      IsoResultsConstraints.AsString := '';
      IsoResultsXConstrain.AsString := '';
      IsoResultsYConstrain.AsString := '';
      IsoResultsAgeConstrain.AsString := '';
      IsoResultsProgName.AsString := ProgramName+' '+ProgVersion;
      IsoResultsRegisteredUser.AsString := RegisteredUser;
      IsoResultsComment.AsString := eComment.Text;
      IsoResults.Post;
      IsoResults.Next;
    end;
  end;

  Close;
end;

procedure TfmResults.bbCancelClick(Sender: TObject);
begin
  try
    Close;
  except
  end;
end;

procedure TfmResults.FormCreate(Sender: TObject);
begin
  dmDVResults.IsoResults.Open;
  dmDVResults.Country.Open;
  dmDVResults.Suite.Open;
  dmDVResults.SuiteList.Open;
  dmDVResults.Lithology.Open;
  dmDVResults.Technique.Open;
  dmDVResults.SmpReg.Open;
  dmDVResults.Interpretation.Open;
  dmDVResults.Equipment.Open;
  dblCountry.KeyValue := LastCountry;
end;

procedure TfmResults.FormDestroy(Sender: TObject);
begin
  dmDVResults.IsoResults.Close;
  dmDVResults.Suite.Close;
  dmDVResults.SuiteList.Close;
  dmDVResults.Country.Close;
  dmDVResults.Lithology.Close;
  dmDVResults.Technique.Close;
  dmDVResults.SmpReg.Close;
  dmDVResults.Interpretation.Close;
  dmDVResults.Equipment.Close;
end;

procedure TfmResults.sbLookupClick(Sender: TObject);
begin
  try
    LookupForm := TfmLookUp.Create(Self);
    LookupForm.ShowModal;
  finally
    LookupForm.Free;
    dmDVResults.Country.Open;
    dmDVResults.CountryList.Open;
    dmDVResults.Suite.Open;
    dmDVResults.Lithology.Open;
    dmDVResults.Technique.Open;
    dmDVResults.Interpretation.Open;
    dmDVResults.Equipment.Open;
    dmDVResults.
  end;
end;

procedure TfmResults.dblCountryExit(Sender: TObject);
begin
  LastCountry := dblCountry.KeyValue;
end;

procedure TfmResults.dblSuiteExit(Sender: TObject);
begin
  with dmDVResults.IsoResults do
    Locate('SuitName', dblSuite.KeyValue,[loCaseInsensitive,loPartialKey]);
end;

end.
