unit GDW4_DfmCheck_Unit;

interface

implementation

uses
  Gd_3mn,
  Gd_about,
  GDErnOpt,
  Gd_cwt,
  Gd_cnstr,
  Gd_drv,
  Gd_edit,
  Gd_MSWD,
  dmGdMSWD,
  dmGdtmpDB,
  Gd_Evap1,
  Gd_Plat2,
  Gd_MdlAt,
  Gd_prec,
  Gd_New,
  gd_HstVl,
  Gd_sht,
  Gd_shtim,
  Gd_AxVl,
  gdw_reg1,
  Gd_Ccda,
  Gd_WtAv,
  GD_param,
  RegUser3,
  Gd_PostFm,
  ImageCollection_dm,
  SysUtils;

procedure TestDfmFormConsistency;
begin
{ GDW1_MainForm: TGDW1_MainForm }
  with TGDW1_MainForm(nil) do { Gd_3mn.pas }
  begin
    pStatusBar.ClassName; { pStatusBar: TPanel; }
    StatusLine.ClassName; { StatusLine: TPanel; }
    pButtons.ClassName; { pButtons: TPanel; }
    lRegisteredUser.ClassName; { lRegisteredUser: TLabel; }
    lProjectName.ClassName; { lProjectName: TLabel; }
    lItemsHaveChanged.ClassName; { lItemsHaveChanged: TLabel; }
    bSaveGDWFile.ClassName; { bSaveGDWFile: TButton; }
    bOpen.ClassName; { bOpen: TButton; }
    bExit.ClassName; { bExit: TButton; }
    bImportSpreadSheet.ClassName; { bImportSpreadSheet: TButton; }
    bEdit.ClassName; { bEdit: TButton; }
    bRegressUnconstrained.ClassName; { bRegressUnconstrained: TButton; }
    bRegressConstrained.ClassName; { bRegressConstrained: TButton; }
    MainMenuGDW1.ClassName; { MainMenuGDW1: TMainMenu; }
    File1.ClassName; { File1: TMenuItem; }
    FileOpenItem.ClassName; { FileOpenItem: TMenuItem; }
    FileAddItem.ClassName; { FileAddItem: TMenuItem; }
    FileSaveItem.ClassName; { FileSaveItem: TMenuItem; }
    FileSaveAsItem.ClassName; { FileSaveAsItem: TMenuItem; }
    N1.ClassName; { N1: TMenuItem; }
    FileImportSpreadSheet.ClassName; { FileImportSpreadSheet: TMenuItem; }
    FileExportSpreadsheet.ClassName; { FileExportSpreadsheet: TMenuItem; }
    N4.ClassName; { N4: TMenuItem; }
    FileOpenLegacy.ClassName; { FileOpenLegacy: TMenuItem; }
    N11.ClassName; { N11: TMenuItem; }
    FileExitItem.ClassName; { FileExitItem: TMenuItem; }
    Edit1.ClassName; { Edit1: TMenuItem; }
    EditEdit.ClassName; { EditEdit: TMenuItem; }
    N7.ClassName; { N7: TMenuItem; }
    EditConvert.ClassName; { EditConvert: TMenuItem; }
    EditConvertConcordia2TeraWasserburg.ClassName; { EditConvertConcordia2TeraWasserburg: TMenuItem; }
    EditConvertTeraWasserburg2Concordia.ClassName; { EditConvertTeraWasserburg2Concordia: TMenuItem; }
    EditConvertIsochron2InverseIsochron.ClassName; { EditConvertIsochron2InverseIsochron: TMenuItem; }
    EditConvertInverseIsochron2Isochron.ClassName; { EditConvertInverseIsochron2Isochron: TMenuItem; }
    Regress1.ClassName; { Regress1: TMenuItem; }
    RegressUnconstrained.ClassName; { RegressUnconstrained: TMenuItem; }
    RegressConstrained.ClassName; { RegressConstrained: TMenuItem; }
    Models.ClassName; { Models: TMenuItem; }
    ModelsTRatio.ClassName; { ModelsTRatio: TMenuItem; }
    ModelsTCHUR.ClassName; { ModelsTCHUR: TMenuItem; }
    ModelsTDM.ClassName; { ModelsTDM: TMenuItem; }
    ModelsT2DM.ClassName; { ModelsT2DM: TMenuItem; }
    ModelsTRD.ClassName; { ModelsTRD: TMenuItem; }
    ModelsT2DMfromAgeEpsilonValues.ClassName; { ModelsT2DMfromAgeEpsilonValues: TMenuItem; }
    N8.ClassName; { N8: TMenuItem; }
    ModelsRDate.ClassName; { ModelsRDate: TMenuItem; }
    ModelsEpsilonDate.ClassName; { ModelsEpsilonDate: TMenuItem; }
    Models238U204PbDateSame.ClassName; { Models238U204PbDateSame: TMenuItem; }
    ModelsRPostFormation.ClassName; { ModelsRPostFormation: TMenuItem; }
    N3.ClassName; { N3: TMenuItem; }
    ModelsT2DMDate.ClassName; { ModelsT2DMDate: TMenuItem; }
    N9.ClassName; { N9: TMenuItem; }
    ModelsRIndividual.ClassName; { ModelsRIndividual: TMenuItem; }
    ModelsEpsilonIndividual.ClassName; { ModelsEpsilonIndividual: TMenuItem; }
    Models238U204Pb.ClassName; { Models238U204Pb: TMenuItem; }
    N10.ClassName; { N10: TMenuItem; }
    Models207Pb206Pb.ClassName; { Models207Pb206Pb: TMenuItem; }
    Models206Pb238U.ClassName; { Models206Pb238U: TMenuItem; }
    Averages.ClassName; { Averages: TMenuItem; }
    AveragesX.ClassName; { AveragesX: TMenuItem; }
    AveragesY.ClassName; { AveragesY: TMenuItem; }
    AveragesZ.ClassName; { AveragesZ: TMenuItem; }
    N5.ClassName; { N5: TMenuItem; }
    AverageConcordia.ClassName; { AverageConcordia: TMenuItem; }
    N12.ClassName; { N12: TMenuItem; }
    AverageArArPlateau.ClassName; { AverageArArPlateau: TMenuItem; }
    Options1.ClassName; { Options1: TMenuItem; }
    OptionsAlphaLevel.ClassName; { OptionsAlphaLevel: TMenuItem; }
    ConcordiaWt1.ClassName; { ConcordiaWt1: TMenuItem; }
    OptionsNormal.ClassName; { OptionsNormal: TMenuItem; }
    OptionsDiscordance.ClassName; { OptionsDiscordance: TMenuItem; }
    OptionsDirectories.ClassName; { OptionsDirectories: TMenuItem; }
    OptionsEllipseMagnification.ClassName; { OptionsEllipseMagnification: TMenuItem; }
    OptionsEllipseMagnification1.ClassName; { OptionsEllipseMagnification1: TMenuItem; }
    OptionsEllipseMagnification2.ClassName; { OptionsEllipseMagnification2: TMenuItem; }
    OptionsMSWD.ClassName; { OptionsMSWD: TMenuItem; }
    OptionsPbModels.ClassName; { OptionsPbModels: TMenuItem; }
    Singlestage.ClassName; { Singlestage: TMenuItem; }
    SK2stage.ClassName; { SK2stage: TMenuItem; }
    Userdefined.ClassName; { Userdefined: TMenuItem; }
    OptionsDefaultErrorType.ClassName; { OptionsDefaultErrorType: TMenuItem; }
    OptionsDefaultErrorTypePercent.ClassName; { OptionsDefaultErrorTypePercent: TMenuItem; }
    OptionsDefaultErrorTypeActual.ClassName; { OptionsDefaultErrorTypeActual: TMenuItem; }
    Styles1.ClassName; { Styles1: TMenuItem; }
    Window1.ClassName; { Window1: TMenuItem; }
    WindowNewItem.ClassName; { WindowNewItem: TMenuItem; }
    WindowNewSampleSelectionItem.ClassName; { WindowNewSampleSelectionItem: TMenuItem; }
    WindowNewRegressionResultsItem.ClassName; { WindowNewRegressionResultsItem: TMenuItem; }
    WindowNewRegressionGraphItem.ClassName; { WindowNewRegressionGraphItem: TMenuItem; }
    WindowNewEpsilonItem.ClassName; { WindowNewEpsilonItem: TMenuItem; }
    SrNd1.ClassName; { SrNd1: TMenuItem; }
    Graph1.ClassName; { Graph1: TMenuItem; }
    WindowNewInitialRatiosItem.ClassName; { WindowNewInitialRatiosItem: TMenuItem; }
    Data1.ClassName; { Data1: TMenuItem; }
    Graph2.ClassName; { Graph2: TMenuItem; }
    N6.ClassName; { N6: TMenuItem; }
    WindowCascadeItem.ClassName; { WindowCascadeItem: TMenuItem; }
    WindowTileItem.ClassName; { WindowTileItem: TMenuItem; }
    WindowArrangeItem.ClassName; { WindowArrangeItem: TMenuItem; }
    Help1.ClassName; { Help1: TMenuItem; }
    HelpAboutItem.ClassName; { HelpAboutItem: TMenuItem; }
    Registeruser1.ClassName; { Registeruser1: TMenuItem; }
    HelpBugReport.ClassName; { HelpBugReport: TMenuItem; }
    Test1.ClassName; { Test1: TMenuItem; }
    OpenDialogGEODATE.ClassName; { OpenDialogGEODATE: TOpenDialog; }
    SaveDialogGEODATE.ClassName; { SaveDialogGEODATE: TSaveDialog; }
    OpenDialogLegacyGEODATE.ClassName; { OpenDialogLegacyGEODATE: TOpenDialog; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ AboutBox: TAboutBox }
  with TAboutBox(nil) do { Gd_about.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    ProgramIcon.ClassName; { ProgramIcon: TImage; }
    ProductName.ClassName; { ProductName: TLabel; }
    lVersion.ClassName; { lVersion: TLabel; }
    Copyright.ClassName; { Copyright: TLabel; }
    Comments.ClassName; { Comments: TLabel; }
    lRegisteredUser.ClassName; { lRegisteredUser: TLabel; }
    OKButton.ClassName; { OKButton: TBitBtn; }
    Panel2.ClassName; { Panel2: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmGetErrorchronOption: TfmGetErrorchronOption }
  with TfmGetErrorchronOption(nil) do { GDErnOpt.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    rbbAugSqrt.ClassName; { rbbAugSqrt: TRadioButton; }
    rbbNoAssum.ClassName; { rbbNoAssum: TRadioButton; }
    rbbVarInit.ClassName; { rbbVarInit: TRadioButton; }
    rbbSepErr.ClassName; { rbbSepErr: TRadioButton; }
    rbbNoAug.ClassName; { rbbNoAug: TRadioButton; }
    Panel4.ClassName; { Panel4: TPanel; }
    Label3.ClassName; { Label3: TLabel; }
    rbbUprItcpt.ClassName; { rbbUprItcpt: TRadioButton; }
    rbbLwrItcpt.ClassName; { rbbLwrItcpt: TRadioButton; }
    Panel3.ClassName; { Panel3: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    eMSWD.ClassName; { eMSWD: TEdit; }
    eCritF.ClassName; { eCritF: TEdit; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    bbHelp.ClassName; { bbHelp: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmConcordiaWtType: TfmConcordiaWtType }
  with TfmConcordiaWtType(nil) do { Gd_cwt.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    rbConcordiaNormal.ClassName; { rbConcordiaNormal: TRadioButton; }
    rbConcordiaUpper.ClassName; { rbConcordiaUpper: TRadioButton; }
    rbConcordiaLower.ClassName; { rbConcordiaLower: TRadioButton; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmConstrain: TfmConstrain }
  with TfmConstrain(nil) do { Gd_cnstr.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    lAge.ClassName; { lAge: TLabel; }
    lXStr.ClassName; { lXStr: TLabel; }
    lYStr.ClassName; { lYStr: TLabel; }
    lMa.ClassName; { lMa: TLabel; }
    eAgeStr.ClassName; { eAgeStr: TEdit; }
    eXStr.ClassName; { eXStr: TEdit; }
    eYStr.ClassName; { eYStr: TEdit; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmOptDir: TfmOptDir }
  with TfmOptDir(nil) do { Gd_drv.pas }
  begin
    lDirSys.ClassName; { lDirSys: TLabel; }
    lDirData.ClassName; { lDirData: TLabel; }
    lDirTemp.ClassName; { lDirTemp: TLabel; }
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    eDrive1.ClassName; { eDrive1: TEdit; }
    eDrive2.ClassName; { eDrive2: TEdit; }
    eDrive3.ClassName; { eDrive3: TEdit; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    eFTable.ClassName; { eFTable: TEdit; }
    eFlexTemplatePath.ClassName; { eFlexTemplatePath: TEdit; }
    eExportPath.ClassName; { eExportPath: TEdit; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmEdit: TfmEdit }
  with TfmEdit(nil) do { Gd_edit.pas }
  begin
    pTop.ClassName; { pTop: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    dbnEdit.ClassName; { dbnEdit: TDBNavigator; }
    eRec.ClassName; { eRec: TEdit; }
    eRecCount.ClassName; { eRecCount: TEdit; }
    StatusBar1.ClassName; { StatusBar1: TStatusBar; }
    pButtons.ClassName; { pButtons: TPanel; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    bbClearMissing.ClassName; { bbClearMissing: TBitBtn; }
    bbClose.ClassName; { bbClose: TBitBtn; }
    bbCorrelation.ClassName; { bbCorrelation: TBitBtn; }
    bbErrorsPC.ClassName; { bbErrorsPC: TBitBtn; }
    bbEvap.ClassName; { bbEvap: TBitBtn; }
    bbFlags.ClassName; { bbFlags: TBitBtn; }
    bbPrecisions.ClassName; { bbPrecisions: TBitBtn; }
    bbRecalculate.ClassName; { bbRecalculate: TBitBtn; }
    bbErrorsA.ClassName; { bbErrorsA: TBitBtn; }
    Panel1.ClassName; { Panel1: TPanel; }
    pTreeView.ClassName; { pTreeView: TPanel; }
    TreeView1.ClassName; { TreeView1: TTreeView; }
    Panel2.ClassName; { Panel2: TPanel; }
    pTitle.ClassName; { pTitle: TPanel; }
    lTitle.ClassName; { lTitle: TLabel; }
    lNRep.ClassName; { lNRep: TLabel; }
    eTitle.ClassName; { eTitle: TEdit; }
    eNRep.ClassName; { eNRep: TEdit; }
    pEdit.ClassName; { pEdit: TPanel; }
    lXXStr.ClassName; { lXXStr: TLabel; }
    lYYStr.ClassName; { lYYStr: TLabel; }
    lXStr.ClassName; { lXStr: TLabel; }
    lYStr.ClassName; { lYStr: TLabel; }
    lZStr.ClassName; { lZStr: TLabel; }
    lValue.ClassName; { lValue: TLabel; }
    lPrecision.ClassName; { lPrecision: TLabel; }
    lErr.ClassName; { lErr: TLabel; }
    lR.ClassName; { lR: TLabel; }
    lSample.ClassName; { lSample: TLabel; }
    lWStr.ClassName; { lWStr: TLabel; }
    lAgeStr.ClassName; { lAgeStr: TLabel; }
    lRhoExtra.ClassName; { lRhoExtra: TLabel; }
    lZZStr.ClassName; { lZZStr: TLabel; }
    dbeXX.ClassName; { dbeXX: TDBEdit; }
    dbeYY.ClassName; { dbeYY: TDBEdit; }
    dbeXRatio.ClassName; { dbeXRatio: TDBEdit; }
    dbeXPrec.ClassName; { dbeXPrec: TDBEdit; }
    dbeXErr.ClassName; { dbeXErr: TDBEdit; }
    dbeSample_No.ClassName; { dbeSample_No: TDBEdit; }
    dbcbXErrType.ClassName; { dbcbXErrType: TDBComboBox; }
    dbeYRatio.ClassName; { dbeYRatio: TDBEdit; }
    dbeYPrec.ClassName; { dbeYPrec: TDBEdit; }
    dbeYErr.ClassName; { dbeYErr: TDBEdit; }
    dbcbYErrType.ClassName; { dbcbYErrType: TDBComboBox; }
    dbeZRatio.ClassName; { dbeZRatio: TDBEdit; }
    dbeZPrec.ClassName; { dbeZPrec: TDBEdit; }
    dbeR.ClassName; { dbeR: TDBEdit; }
    dbcbRegInclude.ClassName; { dbcbRegInclude: TDBCheckBox; }
    dbcbPlotInclude.ClassName; { dbcbPlotInclude: TDBCheckBox; }
    dbeZErr.ClassName; { dbeZErr: TDBEdit; }
    dbcbZErrType.ClassName; { dbcbZErrType: TDBComboBox; }
    dbeWRatio.ClassName; { dbeWRatio: TDBEdit; }
    dbeWPrec.ClassName; { dbeWPrec: TDBEdit; }
    dbeWErr.ClassName; { dbeWErr: TDBEdit; }
    dbcbWErrType.ClassName; { dbcbWErrType: TDBComboBox; }
    dbeAgeValue.ClassName; { dbeAgeValue: TDBEdit; }
    dbeAge95pcValue.ClassName; { dbeAge95pcValue: TDBEdit; }
    dbeR86.ClassName; { dbeR86: TDBEdit; }
    dbeZZ.ClassName; { dbeZZ: TDBEdit; }
    pLatLon.ClassName; { pLatLon: TPanel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    dbeLatitude.ClassName; { dbeLatitude: TDBEdit; }
    dbeLongitude.ClassName; { dbeLongitude: TDBEdit; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmMSWD: TfmMSWD }
  with TfmMSWD(nil) do { Gd_MSWD.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    ExitBtn.ClassName; { ExitBtn: TSpeedButton; }
    bbSave.ClassName; { bbSave: TBitBtn; }
    dbgMSWD.ClassName; { dbgMSWD: TDBGrid; }
    DBNavigator1.ClassName; { DBNavigator1: TDBNavigator; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ dmMSWD: TdmMSWD }
  with TdmMSWD(nil) do { dmGdMSWD.pas }
  begin
    dsF.ClassName; { dsF: TDataSource; }
    cdsF.ClassName; { cdsF: TClientDataSet; }
    cdsFstType.ClassName; { cdsFstType: TWideStringField; }
    cdsFstAlpha.ClassName; { cdsFstAlpha: TFloatField; }
    cdsFstNRep.ClassName; { cdsFstNRep: TFloatField; }
    cdsFstNSmp.ClassName; { cdsFstNSmp: TFloatField; }
    cdsFstFvalue.ClassName; { cdsFstFvalue: TFloatField; }
  end;

{ dmGdwtmp: TdmGdwtmp }
  with TdmGdwtmp(nil) do { dmGdtmpDB.pas }
  begin
    dsGdwEdit.ClassName; { dsGdwEdit: TDataSource; }
    dsGdwReg.ClassName; { dsGdwReg: TDataSource; }
    dsGdwNew.ClassName; { dsGdwNew: TDataSource; }
    cdsEdit.ClassName; { cdsEdit: TClientDataSet; }
    cdsEditProject.ClassName; { cdsEditProject: TStringField; }
    cdsEditSample_No.ClassName; { cdsEditSample_No: TStringField; }
    cdsEditXX.ClassName; { cdsEditXX: TFloatField; }
    cdsEditYY.ClassName; { cdsEditYY: TFloatField; }
    cdsEditZZ.ClassName; { cdsEditZZ: TFloatField; }
    cdsEditXRatio.ClassName; { cdsEditXRatio: TFloatField; }
    cdsEditXPrec.ClassName; { cdsEditXPrec: TFloatField; }
    cdsEditXWt.ClassName; { cdsEditXWt: TFloatField; }
    cdsEditXWtType.ClassName; { cdsEditXWtType: TStringField; }
    cdsEditYRatio.ClassName; { cdsEditYRatio: TFloatField; }
    cdsEditYPrec.ClassName; { cdsEditYPrec: TFloatField; }
    cdsEditYWt.ClassName; { cdsEditYWt: TFloatField; }
    cdsEditYWtType.ClassName; { cdsEditYWtType: TStringField; }
    cdsEditZRatio.ClassName; { cdsEditZRatio: TFloatField; }
    cdsEditZPrec.ClassName; { cdsEditZPrec: TFloatField; }
    cdsEditZWt.ClassName; { cdsEditZWt: TFloatField; }
    cdsEditZWtType.ClassName; { cdsEditZWtType: TStringField; }
    cdsEditWRatio.ClassName; { cdsEditWRatio: TFloatField; }
    cdsEditWPrec.ClassName; { cdsEditWPrec: TFloatField; }
    cdsEditWWt.ClassName; { cdsEditWWt: TFloatField; }
    cdsEditWWtType.ClassName; { cdsEditWWtType: TStringField; }
    cdsEditR.ClassName; { cdsEditR: TFloatField; }
    cdsEditRhoExtra.ClassName; { cdsEditRhoExtra: TFloatField; }
    cdsEditRFlag.ClassName; { cdsEditRFlag: TStringField; }
    cdsEditPFlag.ClassName; { cdsEditPFlag: TStringField; }
    cdsEditAgeValue.ClassName; { cdsEditAgeValue: TFloatField; }
    cdsEditAge95pcValue.ClassName; { cdsEditAge95pcValue: TFloatField; }
    cdsEditLatitude.ClassName; { cdsEditLatitude: TFloatField; }
    cdsEditLongitude.ClassName; { cdsEditLongitude: TFloatField; }
    cdsEditElevation.ClassName; { cdsEditElevation: TFloatField; }
    cdsEditIsotopeSystem.ClassName; { cdsEditIsotopeSystem: TStringField; }
    cdsEditIsotopeProcess.ClassName; { cdsEditIsotopeProcess: TStringField; }
    cdsEditAnTyp.ClassName; { cdsEditAnTyp: TStringField; }
    cdsNew.ClassName; { cdsNew: TClientDataSet; }
    cdsNewSample_No.ClassName; { cdsNewSample_No: TStringField; }
    cdsReg.ClassName; { cdsReg: TClientDataSet; }
    cdsRegSample_No.ClassName; { cdsRegSample_No: TStringField; }
    cdsRegRFlag.ClassName; { cdsRegRFlag: TStringField; }
    cdsRegXWt.ClassName; { cdsRegXWt: TFloatField; }
    cdsRegXWtType.ClassName; { cdsRegXWtType: TStringField; }
    cdsRegYWt.ClassName; { cdsRegYWt: TFloatField; }
    cdsRegYWtType.ClassName; { cdsRegYWtType: TStringField; }
    cdsRegXDev.ClassName; { cdsRegXDev: TFloatField; }
    cdsRegYDev.ClassName; { cdsRegYDev: TFloatField; }
    cdsRegPFlag.ClassName; { cdsRegPFlag: TStringField; }
    cdsRegProject.ClassName; { cdsRegProject: TStringField; }
    cdsRegXElem.ClassName; { cdsRegXElem: TFloatField; }
    cdsRegYElem.ClassName; { cdsRegYElem: TFloatField; }
    cdsRegXRatio.ClassName; { cdsRegXRatio: TFloatField; }
    cdsRegXPrec.ClassName; { cdsRegXPrec: TFloatField; }
    cdsRegYRatio.ClassName; { cdsRegYRatio: TFloatField; }
    cdsRegYPrec.ClassName; { cdsRegYPrec: TFloatField; }
    cdsRegZRatio.ClassName; { cdsRegZRatio: TFloatField; }
    cdsRegZPrec.ClassName; { cdsRegZPrec: TFloatField; }
    cdsRegR.ClassName; { cdsRegR: TFloatField; }
    cdsRegi.ClassName; { cdsRegi: TIntegerField; }
    FDMemTableData.ClassName; { FDMemTableData: TFDMemTable; }
    FDMemTableDataProject.ClassName; { FDMemTableDataProject: TStringField; }
    FDMemTableDataSaNo.ClassName; { FDMemTableDataSaNo: TStringField; }
    FDMemTableDataXElemConc.ClassName; { FDMemTableDataXElemConc: TFloatField; }
    FDMemTableDataYElemConc.ClassName; { FDMemTableDataYElemConc: TFloatField; }
    FDMemTableDataZElemConc.ClassName; { FDMemTableDataZElemConc: TFloatField; }
    FDMemTableDataX.ClassName; { FDMemTableDataX: TFloatField; }
    FDMemTableDatapX.ClassName; { FDMemTableDatapX: TFloatField; }
    FDMemTableDatasX.ClassName; { FDMemTableDatasX: TFloatField; }
    FDMemTableDataeX.ClassName; { FDMemTableDataeX: TStringField; }
    FDMemTableDataY.ClassName; { FDMemTableDataY: TFloatField; }
    FDMemTableDatapY.ClassName; { FDMemTableDatapY: TFloatField; }
    FDMemTableDatasY.ClassName; { FDMemTableDatasY: TFloatField; }
    FDMemTableDataeY.ClassName; { FDMemTableDataeY: TStringField; }
    FDMemTableDataZ.ClassName; { FDMemTableDataZ: TFloatField; }
    FDMemTableDatapZ.ClassName; { FDMemTableDatapZ: TFloatField; }
    FDMemTableDatasZ.ClassName; { FDMemTableDatasZ: TFloatField; }
    FDMemTableDataeZ.ClassName; { FDMemTableDataeZ: TStringField; }
    FDMemTableDataW.ClassName; { FDMemTableDataW: TFloatField; }
    FDMemTableDatapW.ClassName; { FDMemTableDatapW: TFloatField; }
    FDMemTableDatasW.ClassName; { FDMemTableDatasW: TFloatField; }
    FDMemTableDataeW.ClassName; { FDMemTableDataeW: TStringField; }
    FDMemTableDatarho.ClassName; { FDMemTableDatarho: TFloatField; }
    FDMemTableDatarho2.ClassName; { FDMemTableDatarho2: TFloatField; }
    FDMemTableDataRFlag.ClassName; { FDMemTableDataRFlag: TStringField; }
    FDMemTableDataPFlag.ClassName; { FDMemTableDataPFlag: TStringField; }
    FDMemTableDataAge.ClassName; { FDMemTableDataAge: TFloatField; }
    FDMemTableDatasAge.ClassName; { FDMemTableDatasAge: TFloatField; }
    FDMemTableDataLatitude.ClassName; { FDMemTableDataLatitude: TFloatField; }
    FDMemTableDataLongitude.ClassName; { FDMemTableDataLongitude: TFloatField; }
    FDMemTableDataElevation.ClassName; { FDMemTableDataElevation: TFloatField; }
    FDMemTableDataIsotopeSystem.ClassName; { FDMemTableDataIsotopeSystem: TStringField; }
    FDMemTableDataIsotopeProcess.ClassName; { FDMemTableDataIsotopeProcess: TStringField; }
    FDMemTableDataAnTyp.ClassName; { FDMemTableDataAnTyp: TStringField; }
    FDMemTableResults.ClassName; { FDMemTableResults: TFDMemTable; }
    FDMemTableResultsRecordID.ClassName; { FDMemTableResultsRecordID: TLargeintField; }
    FDMemTableResultsDateTimeCreated.ClassName; { FDMemTableResultsDateTimeCreated: TDateTimeField; }
    FDMemTableResultsProject.ClassName; { FDMemTableResultsProject: TStringField; }
    FDMemTableResultsIsotopeSystemID.ClassName; { FDMemTableResultsIsotopeSystemID: TStringField; }
    FDMemTableResultsAgeX.ClassName; { FDMemTableResultsAgeX: TFloatField; }
    FDMemTableResultsAgeY.ClassName; { FDMemTableResultsAgeY: TFloatField; }
    FDMemTableResultsAgeZ.ClassName; { FDMemTableResultsAgeZ: TFloatField; }
    FDMemTableResultssAgeXPlus.ClassName; { FDMemTableResultssAgeXPlus: TFloatField; }
    FDMemTableResultssAgeXMinus.ClassName; { FDMemTableResultssAgeXMinus: TFloatField; }
    FDMemTableResultssAgeYPlus.ClassName; { FDMemTableResultssAgeYPlus: TFloatField; }
    FDMemTableResultssAgeYMinus.ClassName; { FDMemTableResultssAgeYMinus: TFloatField; }
    FDMemTableResultssAgeZPlus.ClassName; { FDMemTableResultssAgeZPlus: TFloatField; }
    FDMemTableResultssAgeZMinus.ClassName; { FDMemTableResultssAgeZMinus: TFloatField; }
    FDMemTableResultsDecayConst1.ClassName; { FDMemTableResultsDecayConst1: TFloatField; }
    FDMemTableResultsDecayConst2.ClassName; { FDMemTableResultsDecayConst2: TFloatField; }
    FDMemTableResultssDecayConst1.ClassName; { FDMemTableResultssDecayConst1: TFloatField; }
    FDMemTableResultssDecayConst2.ClassName; { FDMemTableResultssDecayConst2: TFloatField; }
    FDMemTableResultsIsotopeConstant.ClassName; { FDMemTableResultsIsotopeConstant: TFloatField; }
    FDMemTableResultsMSWDequivalence.ClassName; { FDMemTableResultsMSWDequivalence: TFloatField; }
    FDMemTableResultsnReplicates.ClassName; { FDMemTableResultsnReplicates: TFloatField; }
    FDMemTableResultsnSamples.ClassName; { FDMemTableResultsnSamples: TFloatField; }
    FDMemTableResultsnSamplesRegressed.ClassName; { FDMemTableResultsnSamplesRegressed: TFloatField; }
    FDMemTableResultsProbOfFitequivalence.ClassName; { FDMemTableResultsProbOfFitequivalence: TFloatField; }
    FDMemTableResultsMSWDconcordance.ClassName; { FDMemTableResultsMSWDconcordance: TFloatField; }
    FDMemTableResultsProbOfFitconcordance.ClassName; { FDMemTableResultsProbOfFitconcordance: TFloatField; }
    FDMemTableResultsInitialRatio.ClassName; { FDMemTableResultsInitialRatio: TFloatField; }
    FDMemTableResultssInitialRatio.ClassName; { FDMemTableResultssInitialRatio: TFloatField; }
    FDMemTableResultsEpsilonGamma.ClassName; { FDMemTableResultsEpsilonGamma: TFloatField; }
    FDMemTableResultssEpsilonGamma.ClassName; { FDMemTableResultssEpsilonGamma: TFloatField; }
    FDMemTableResultsAugmentation.ClassName; { FDMemTableResultsAugmentation: TStringField; }
    FDMemTableResultsMSWDforced.ClassName; { FDMemTableResultsMSWDforced: TStringField; }
    FDMemTableResultsAdditional.ClassName; { FDMemTableResultsAdditional: TStringField; }
    FDMemTableResultsSoftwareUsed.ClassName; { FDMemTableResultsSoftwareUsed: TStringField; }
    FDMemTableResultsDVUserID.ClassName; { FDMemTableResultsDVUserID: TStringField; }
    FDMemTableResultsMaterialID.ClassName; { FDMemTableResultsMaterialID: TStringField; }
    FDMemTableResultsLithology.ClassName; { FDMemTableResultsLithology: TStringField; }
    FDMemTableResultsApproachID.ClassName; { FDMemTableResultsApproachID: TStringField; }
    FDMemTableResultsTechniqueID.ClassName; { FDMemTableResultsTechniqueID: TStringField; }
    FDMemTableResultsInterpID.ClassName; { FDMemTableResultsInterpID: TStringField; }
    FDMemTableResultsMethodID.ClassName; { FDMemTableResultsMethodID: TStringField; }
    FDMemTableResultsOtherIntercept.ClassName; { FDMemTableResultsOtherIntercept: TFloatField; }
    FDMemTableResultsOtherInterceptPlus.ClassName; { FDMemTableResultsOtherInterceptPlus: TFloatField; }
    FDMemTableResultsOtherInterceptMinus.ClassName; { FDMemTableResultsOtherInterceptMinus: TFloatField; }
    FDMemTableResultsWeighting.ClassName; { FDMemTableResultsWeighting: TStringField; }
    FDMemTableResultsLudwig_p.ClassName; { FDMemTableResultsLudwig_p: TFloatField; }
    FDMemTableResultsConstraints.ClassName; { FDMemTableResultsConstraints: TStringField; }
    FDMemTableResultsConstraintAge.ClassName; { FDMemTableResultsConstraintAge: TFloatField; }
    FDMemTableResultsConstraintX.ClassName; { FDMemTableResultsConstraintX: TFloatField; }
    FDMemTableResultsConstraintY.ClassName; { FDMemTableResultsConstraintY: TFloatField; }
    FDMemTableResultsReferenceID.ClassName; { FDMemTableResultsReferenceID: TIntegerField; }
    FDMemTableResultsLabID.ClassName; { FDMemTableResultsLabID: TStringField; }
    FDMemTableResultsInitialModel.ClassName; { FDMemTableResultsInitialModel: TStringField; }
    FDMemTableResultsModelSourceMu.ClassName; { FDMemTableResultsModelSourceMu: TFloatField; }
    FDMemTableResultsTracerUncertainty.ClassName; { FDMemTableResultsTracerUncertainty: TFloatField; }
    FDMemTableResultsSampleID.ClassName; { FDMemTableResultsSampleID: TStringField; }
    FDMemTableResultsFrac.ClassName; { FDMemTableResultsFrac: TStringField; }
    FDMemTableResultsLongitude.ClassName; { FDMemTableResultsLongitude: TFloatField; }
    FDMemTableResultsLatitude.ClassName; { FDMemTableResultsLatitude: TFloatField; }
    FDMemTableResultsElevation.ClassName; { FDMemTableResultsElevation: TFloatField; }
    FDMemTableResultspLongitude.ClassName; { FDMemTableResultspLongitude: TFloatField; }
    FDMemTableResultspLatitude.ClassName; { FDMemTableResultspLatitude: TFloatField; }
    FDMemTableResultspElevation.ClassName; { FDMemTableResultspElevation: TFloatField; }
    FDMemTableResultsOriginalNo.ClassName; { FDMemTableResultsOriginalNo: TStringField; }
    FDMemTableResultsIGSN.ClassName; { FDMemTableResultsIGSN: TStringField; }
    FDMemTableResultssModelSourceMuPlus.ClassName; { FDMemTableResultssModelSourceMuPlus: TFloatField; }
    FDMemTableResultssModelSourceMuMinus.ClassName; { FDMemTableResultssModelSourceMuMinus: TFloatField; }
    FDStanStorageXMLLink1.ClassName; { FDStanStorageXMLLink1: TFDStanStorageXMLLink; }
  end;

{ fmZrEvap: TfmZrEvap }
  with TfmZrEvap(nil) do { Gd_Evap1.pas }
  begin
    bbOK.ClassName; { bbOK: TBitBtn; }
    Panel1.ClassName; { Panel1: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    eZrEvapStr.ClassName; { eZrEvapStr: TEdit; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmPlatAr: TfmPlatAr }
  with TfmPlatAr(nil) do { Gd_Plat2.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    bbCumHist.ClassName; { bbCumHist: TBitBtn; }
    bbStoreMdl.ClassName; { bbStoreMdl: TBitBtn; }
    bbSpreadSheet.ClassName; { bbSpreadSheet: TBitBtn; }
    Panel6.ClassName; { Panel6: TPanel; }
    Splitter1.ClassName; { Splitter1: TSplitter; }
    Panel5.ClassName; { Panel5: TPanel; }
    Panel2.ClassName; { Panel2: TPanel; }
    pTopLeft.ClassName; { pTopLeft: TPanel; }
    pResultTitle.ClassName; { pResultTitle: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    lResultTitle.ClassName; { lResultTitle: TLabel; }
    eTitle.ClassName; { eTitle: TEdit; }
    Panel8.ClassName; { Panel8: TPanel; }
    ChCum.ClassName; { ChCum: TChart; }
    LineSeries1.ClassName; { LineSeries1: TLineSeries; }
    LineSeries2.ClassName; { LineSeries2: TLineSeries; }
    LineSeries3.ClassName; { LineSeries3: TLineSeries; }
    LineSeries4.ClassName; { LineSeries4: TLineSeries; }
    PointSeries1.ClassName; { PointSeries1: TPointSeries; }
    LineSeries5.ClassName; { LineSeries5: TLineSeries; }
    LineSeries6.ClassName; { LineSeries6: TLineSeries; }
    PointSeries2.ClassName; { PointSeries2: TPointSeries; }
    PointSeries3.ClassName; { PointSeries3: TPointSeries; }
    ErrorPointSeries1.ClassName; { ErrorPointSeries1: TErrorPointSeries; }
    ErrorPointSeries2.ClassName; { ErrorPointSeries2: TErrorPointSeries; }
    LineSeries7.ClassName; { LineSeries7: TLineSeries; }
    PointSeries4.ClassName; { PointSeries4: TPointSeries; }
    PointSeries5.ClassName; { PointSeries5: TPointSeries; }
    Series15.ClassName; { Series15: TLineSeries; }
    TeeCommander2.ClassName; { TeeCommander2: TTeeCommander; }
    Panel3.ClassName; { Panel3: TPanel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    lWtAvAugmentedSD.ClassName; { lWtAvAugmentedSD: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    lWtAvPlus.ClassName; { lWtAvPlus: TLabel; }
    lWtAvMinus.ClassName; { lWtAvMinus: TLabel; }
    Label10.ClassName; { Label10: TLabel; }
    Label11.ClassName; { Label11: TLabel; }
    Label12.ClassName; { Label12: TLabel; }
    lWtAvAugmented95.ClassName; { lWtAvAugmented95: TLabel; }
    lErrorsBased.ClassName; { lErrorsBased: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label7.ClassName; { Label7: TLabel; }
    lWtAvPlusIncl.ClassName; { lWtAvPlusIncl: TLabel; }
    lWtAvMinusIncl.ClassName; { lWtAvMinusIncl: TLabel; }
    lWtAvIncl.ClassName; { lWtAvIncl: TLabel; }
    eWtAv.ClassName; { eWtAv: TEdit; }
    eIcnt.ClassName; { eIcnt: TEdit; }
    eWtAvExpectedSD.ClassName; { eWtAvExpectedSD: TEdit; }
    eWtAvPlus95.ClassName; { eWtAvPlus95: TEdit; }
    eWtAvMinus95.ClassName; { eWtAvMinus95: TEdit; }
    eWtAvExpected95.ClassName; { eWtAvExpected95: TEdit; }
    eWtAvObservedSD.ClassName; { eWtAvObservedSD: TEdit; }
    eWtAvObserved95.ClassName; { eWtAvObserved95: TEdit; }
    eMSWD.ClassName; { eMSWD: TEdit; }
    eWtAvAugmentedSD.ClassName; { eWtAvAugmentedSD: TEdit; }
    eWtAvAugmented95.ClassName; { eWtAvAugmented95: TEdit; }
    eProbabilityOfF.ClassName; { eProbabilityOfF: TEdit; }
    eNsamp.ClassName; { eNsamp: TEdit; }
    eWtAvPlus95Incl.ClassName; { eWtAvPlus95Incl: TEdit; }
    eWtAvMinus95Incl.ClassName; { eWtAvMinus95Incl: TEdit; }
    Panel4.ClassName; { Panel4: TPanel; }
    Splitter2.ClassName; { Splitter2: TSplitter; }
    TeeCommander1.ClassName; { TeeCommander1: TTeeCommander; }
    lModifyGraphSettings.ClassName; { lModifyGraphSettings: TLabel; }
    ChPlat.ClassName; { ChPlat: TChart; }
    Series8.ClassName; { Series8: TLineSeries; }
    Series5.ClassName; { Series5: TLineSeries; }
    Series4.ClassName; { Series4: TLineSeries; }
    Series3.ClassName; { Series3: TLineSeries; }
    Series10.ClassName; { Series10: TPointSeries; }
    Series9.ClassName; { Series9: TLineSeries; }
    Series1.ClassName; { Series1: TLineSeries; }
    Series6.ClassName; { Series6: TPointSeries; }
    Series2.ClassName; { Series2: TPointSeries; }
    Series11.ClassName; { Series11: TErrorPointSeries; }
    Series12.ClassName; { Series12: TErrorPointSeries; }
    Series13.ClassName; { Series13: TLineSeries; }
    Series14.ClassName; { Series14: TPointSeries; }
    Series7.ClassName; { Series7: TPointSeries; }
    pCheckBoxes.ClassName; { pCheckBoxes: TPanel; }
    cbCurrentSample.ClassName; { cbCurrentSample: TCheckBox; }
    cbLegend.ClassName; { cbLegend: TCheckBox; }
    pResiduals.ClassName; { pResiduals: TPanel; }
    lResidual.ClassName; { lResidual: TLabel; }
    Label8.ClassName; { Label8: TLabel; }
    Label9.ClassName; { Label9: TLabel; }
    Label13.ClassName; { Label13: TLabel; }
    Label14.ClassName; { Label14: TLabel; }
    Label15.ClassName; { Label15: TLabel; }
    dbnReg.ClassName; { dbnReg: TDBNavigator; }
    dbRegSample.ClassName; { dbRegSample: TDBEdit; }
    DBEdit2.ClassName; { DBEdit2: TDBEdit; }
    DBEdit3.ClassName; { DBEdit3: TDBEdit; }
    DBEdit5.ClassName; { DBEdit5: TDBEdit; }
    DBEdit6.ClassName; { DBEdit6: TDBEdit; }
    DBEdit7.ClassName; { DBEdit7: TDBEdit; }
    DBEdit8.ClassName; { DBEdit8: TDBEdit; }
    DBEdit9.ClassName; { DBEdit9: TDBEdit; }
    DBEdit10.ClassName; { DBEdit10: TDBEdit; }
    DBCheckBox1.ClassName; { DBCheckBox1: TDBCheckBox; }
    bbUpdate.ClassName; { bbUpdate: TBitBtn; }
    pTreeSmp.ClassName; { pTreeSmp: TPanel; }
    TreeView1.ClassName; { TreeView1: TTreeView; }
    SaveDialogModels.ClassName; { SaveDialogModels: TSaveDialog; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmGetMdlAt: TfmGetMdlAt }
  with TfmGetMdlAt(nil) do { Gd_MdlAt.pas }
  begin
    bbOK.ClassName; { bbOK: TBitBtn; }
    Panel1.ClassName; { Panel1: TPanel; }
    lAt.ClassName; { lAt: TLabel; }
    ISigma.ClassName; { ISigma: TLabel; }
    eAt.ClassName; { eAt: TEdit; }
    eSigma.ClassName; { eSigma: TEdit; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmPrecision: TfmPrecision }
  with TfmPrecision(nil) do { Gd_prec.pas }
  begin
    GroupBox1.ClassName; { GroupBox1: TGroupBox; }
    rbX.ClassName; { rbX: TRadioButton; }
    rbY.ClassName; { rbY: TRadioButton; }
    rbZ.ClassName; { rbZ: TRadioButton; }
    GroupBox2.ClassName; { GroupBox2: TGroupBox; }
    rb1Sigma.ClassName; { rb1Sigma: TRadioButton; }
    rb2Sigma.ClassName; { rb2Sigma: TRadioButton; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    cbPrecAll.ClassName; { cbPrecAll: TCheckBox; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmNewData: TfmNewData }
  with TfmNewData(nil) do { Gd_New.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    Panel3.ClassName; { Panel3: TPanel; }
    GroupBox2.ClassName; { GroupBox2: TGroupBox; }
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    eXWt.ClassName; { eXWt: TEdit; }
    eYWt.ClassName; { eYWt: TEdit; }
    cbxXWtType.ClassName; { cbxXWtType: TComboBox; }
    cbxYWtType.ClassName; { cbxYWtType: TComboBox; }
    eR.ClassName; { eR: TEdit; }
    Panel4.ClassName; { Panel4: TPanel; }
    GroupBox1.ClassName; { GroupBox1: TGroupBox; }
    cbxIsoSys.ClassName; { cbxIsoSys: TComboBox; }
    Panel5.ClassName; { Panel5: TPanel; }
    Label4.ClassName; { Label4: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    Panel6.ClassName; { Panel6: TPanel; }
    Panel2.ClassName; { Panel2: TPanel; }
    DBGrid1.ClassName; { DBGrid1: TDBGrid; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmGetHistValues: TfmGetHistValues }
  with TfmGetHistValues(nil) do { gd_HstVl.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    Panel2.ClassName; { Panel2: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    eMinimum.ClassName; { eMinimum: TEdit; }
    eWidth.ClassName; { eWidth: TEdit; }
    eNumInt.ClassName; { eNumInt: TEdit; }
    eMaximum.ClassName; { eMaximum: TEdit; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmSheet: TfmSheet }
  with TfmSheet(nil) do { Gd_sht.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    sbClose.ClassName; { sbClose: TSpeedButton; }
    bbSaveSheet.ClassName; { bbSaveSheet: TBitBtn; }
    sbSheet.ClassName; { sbSheet: TStatusBar; }
    Panel2.ClassName; { Panel2: TPanel; }
    Panel3.ClassName; { Panel3: TPanel; }
    FlexCelPreviewer1.ClassName; { FlexCelPreviewer1: TFlexCelPreviewer; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmSheetImport: TfmSheetImport }
  with TfmSheetImport(nil) do { Gd_shtim.pas }
  begin
    Splitter1.ClassName; { Splitter1: TSplitter; }
    Panel1.ClassName; { Panel1: TPanel; }
    bbOpenSheet.ClassName; { bbOpenSheet: TBitBtn; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    sbSheet.ClassName; { sbSheet: TStatusBar; }
    Panel2.ClassName; { Panel2: TPanel; }
    gbDefineFields.ClassName; { gbDefineFields: TGroupBox; }
    Label1.ClassName; { Label1: TLabel; }
    lSample.ClassName; { lSample: TLabel; }
    lXXStr.ClassName; { lXXStr: TLabel; }
    lYYStr.ClassName; { lYYStr: TLabel; }
    lXStr.ClassName; { lXStr: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    Label7.ClassName; { Label7: TLabel; }
    Label9.ClassName; { Label9: TLabel; }
    lYStr.ClassName; { lYStr: TLabel; }
    Label10.ClassName; { Label10: TLabel; }
    Label11.ClassName; { Label11: TLabel; }
    Label12.ClassName; { Label12: TLabel; }
    lZStr.ClassName; { lZStr: TLabel; }
    Label14.ClassName; { Label14: TLabel; }
    lR.ClassName; { lR: TLabel; }
    Label17.ClassName; { Label17: TLabel; }
    Label18.ClassName; { Label18: TLabel; }
    Label19.ClassName; { Label19: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label13.ClassName; { Label13: TLabel; }
    Label15.ClassName; { Label15: TLabel; }
    Label16.ClassName; { Label16: TLabel; }
    Label20.ClassName; { Label20: TLabel; }
    lAgeStr.ClassName; { lAgeStr: TLabel; }
    Label23.ClassName; { Label23: TLabel; }
    Label25.ClassName; { Label25: TLabel; }
    Label26.ClassName; { Label26: TLabel; }
    Label27.ClassName; { Label27: TLabel; }
    lWStr.ClassName; { lWStr: TLabel; }
    lZZStr.ClassName; { lZZStr: TLabel; }
    eSampleNo.ClassName; { eSampleNo: TEdit; }
    eXStr.ClassName; { eXStr: TEdit; }
    eZStr.ClassName; { eZStr: TEdit; }
    eYStr.ClassName; { eYStr: TEdit; }
    eRStr.ClassName; { eRStr: TEdit; }
    eXXStr.ClassName; { eXXStr: TEdit; }
    eXPrecStr.ClassName; { eXPrecStr: TEdit; }
    eYPrecStr.ClassName; { eYPrecStr: TEdit; }
    eZPrecStr.ClassName; { eZPrecStr: TEdit; }
    eYYStr.ClassName; { eYYStr: TEdit; }
    eXErrStr.ClassName; { eXErrStr: TEdit; }
    eYErrStr.ClassName; { eYErrStr: TEdit; }
    eRFlagStr.ClassName; { eRFlagStr: TEdit; }
    eXErrTypeStr.ClassName; { eXErrTypeStr: TEdit; }
    eYErrTypeStr.ClassName; { eYErrTypeStr: TEdit; }
    ePFlagStr.ClassName; { ePFlagStr: TEdit; }
    eLatitudeStr.ClassName; { eLatitudeStr: TEdit; }
    eLongitudeStr.ClassName; { eLongitudeStr: TEdit; }
    Memo1.ClassName; { Memo1: TMemo; }
    eZErrStr.ClassName; { eZErrStr: TEdit; }
    eZErrTypeStr.ClassName; { eZErrTypeStr: TEdit; }
    eAgeStr.ClassName; { eAgeStr: TEdit; }
    eAgeErrStr.ClassName; { eAgeErrStr: TEdit; }
    eWStr.ClassName; { eWStr: TEdit; }
    eWErrTypeStr.ClassName; { eWErrTypeStr: TEdit; }
    eWErrStr.ClassName; { eWErrStr: TEdit; }
    eWPrecStr.ClassName; { eWPrecStr: TEdit; }
    eZZStr.ClassName; { eZZStr: TEdit; }
    bbImport.ClassName; { bbImport: TBitBtn; }
    gbDefineRows.ClassName; { gbDefineRows: TGroupBox; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    meFromRow.ClassName; { meFromRow: TEdit; }
    meToRow.ClassName; { meToRow: TEdit; }
    gbIsoSys.ClassName; { gbIsoSys: TGroupBox; }
    lXaxisStr.ClassName; { lXaxisStr: TLabel; }
    lvs.ClassName; { lvs: TLabel; }
    lYAxisStr.ClassName; { lYAxisStr: TLabel; }
    cbxIsoSys.ClassName; { cbxIsoSys: TComboBox; }
    gbPrefix.ClassName; { gbPrefix: TGroupBox; }
    Label8.ClassName; { Label8: TLabel; }
    eSmpPrefix.ClassName; { eSmpPrefix: TEdit; }
    gbDefineTabSheet.ClassName; { gbDefineTabSheet: TGroupBox; }
    cbSheetName.ClassName; { cbSheetName: TComboBox; }
    TabControl.ClassName; { TabControl: TTabControl; }
    SheetData.ClassName; { SheetData: TStringGrid; }
    Tabs.ClassName; { Tabs: TTabSet; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    OpenDialogSprdSheet.ClassName; { OpenDialogSprdSheet: TOpenDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmAxOpt: TfmAxOpt }
  with TfmAxOpt(nil) do { Gd_AxVl.pas }
  begin
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    cbAugmentEllipses.ClassName; { cbAugmentEllipses: TCheckBox; }
    meXMinStr.ClassName; { meXMinStr: TEdit; }
    meXMaxStr.ClassName; { meXMaxStr: TEdit; }
    meYMinStr.ClassName; { meYMinStr: TEdit; }
    meYMaxStr.ClassName; { meYMaxStr: TEdit; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmRegressionResult: TfmRegressionResult }
  with TfmRegressionResult(nil) do { gdw_reg1.pas }
  begin
    Splitter1.ClassName; { Splitter1: TSplitter; }
    pButtonsTop.ClassName; { pButtonsTop: TPanel; }
    bbReregress.ClassName; { bbReregress: TBitBtn; }
    bClose.ClassName; { bClose: TButton; }
    bSpreadSheet.ClassName; { bSpreadSheet: TButton; }
    bExportForDateView.ClassName; { bExportForDateView: TButton; }
    bReRegress.ClassName; { bReRegress: TButton; }
    sbRegressionResult.ClassName; { sbRegressionResult: TStatusBar; }
    pLeft.ClassName; { pLeft: TPanel; }
    Panel1.ClassName; { Panel1: TPanel; }
    eCentroidStr.ClassName; { eCentroidStr: TLabel; }
    eSlopeStr.ClassName; { eSlopeStr: TLabel; }
    eInterceptStr.ClassName; { eInterceptStr: TLabel; }
    lCentroidX.ClassName; { lCentroidX: TLabel; }
    lCentroidY.ClassName; { lCentroidY: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    Label7.ClassName; { Label7: TLabel; }
    Label8.ClassName; { Label8: TLabel; }
    Label9.ClassName; { Label9: TLabel; }
    lConstrain.ClassName; { lConstrain: TLabel; }
    lConstrainAnd.ClassName; { lConstrainAnd: TLabel; }
    lConstrainNear.ClassName; { lConstrainNear: TLabel; }
    lConstrainMa.ClassName; { lConstrainMa: TLabel; }
    eCentroidX.ClassName; { eCentroidX: TEdit; }
    eSlope.ClassName; { eSlope: TEdit; }
    eIntercept.ClassName; { eIntercept: TEdit; }
    eCentroidY.ClassName; { eCentroidY: TEdit; }
    eInterceptErr.ClassName; { eInterceptErr: TEdit; }
    eSlopeErr.ClassName; { eSlopeErr: TEdit; }
    eXConstrain.ClassName; { eXConstrain: TEdit; }
    eYConstrain.ClassName; { eYConstrain: TEdit; }
    eConstrainAge.ClassName; { eConstrainAge: TEdit; }
    Panel3.ClassName; { Panel3: TPanel; }
    eMSWDStr.ClassName; { eMSWDStr: TLabel; }
    lMSWDon.ClassName; { lMSWDon: TLabel; }
    lAugmented.ClassName; { lAugmented: TLabel; }
    lIsochronErrorchron.ClassName; { lIsochronErrorchron: TLabel; }
    lFCrit.ClassName; { lFCrit: TLabel; }
    lMSWDForced.ClassName; { lMSWDForced: TLabel; }
    lMSWDof.ClassName; { lMSWDof: TLabel; }
    lProbabilityOfFit.ClassName; { lProbabilityOfFit: TLabel; }
    eMSWD.ClassName; { eMSWD: TEdit; }
    eNumberOfPointsRegressed.ClassName; { eNumberOfPointsRegressed: TEdit; }
    eNumberOfPoints.ClassName; { eNumberOfPoints: TEdit; }
    eProbabilityOfFit.ClassName; { eProbabilityOfFit: TEdit; }
    Panel4.ClassName; { Panel4: TPanel; }
    eIterationStr.ClassName; { eIterationStr: TLabel; }
    PanelDate.ClassName; { PanelDate: TPanel; }
    lRoStr.ClassName; { lRoStr: TLabel; }
    lEpsilonStr.ClassName; { lEpsilonStr: TLabel; }
    lDateStr.ClassName; { lDateStr: TLabel; }
    lMuErrPlusOnly.ClassName; { lMuErrPlusOnly: TLabel; }
    lRoErrPlusOnly.ClassName; { lRoErrPlusOnly: TLabel; }
    lDateErrPlusOnly.ClassName; { lDateErrPlusOnly: TLabel; }
    lEpsilon95Percent.ClassName; { lEpsilon95Percent: TLabel; }
    lRo95Percent.ClassName; { lRo95Percent: TLabel; }
    lDate95Percent.ClassName; { lDate95Percent: TLabel; }
    lDateErrMinusOnly.ClassName; { lDateErrMinusOnly: TLabel; }
    lMuErrMinusOnly.ClassName; { lMuErrMinusOnly: TLabel; }
    lRoErrMinusOnly.ClassName; { lRoErrMinusOnly: TLabel; }
    lDateMinusErrAdjusted.ClassName; { lDateMinusErrAdjusted: TLabel; }
    lDateAdjusted.ClassName; { lDateAdjusted: TLabel; }
    lDatePlusErrAdjusted.ClassName; { lDatePlusErrAdjusted: TLabel; }
    lDateErrPlusOnlyIncl.ClassName; { lDateErrPlusOnlyIncl: TLabel; }
    lDateErrMinusOnlyIncl.ClassName; { lDateErrMinusOnlyIncl: TLabel; }
    lDate95PercentIncl.ClassName; { lDate95PercentIncl: TLabel; }
    lDatePlusErrAdjustedIncl.ClassName; { lDatePlusErrAdjustedIncl: TLabel; }
    lDateMinusErrAdjustedIncl.ClassName; { lDateMinusErrAdjustedIncl: TLabel; }
    lDateAdjustedIncl.ClassName; { lDateAdjustedIncl: TLabel; }
    lDateDCincl.ClassName; { lDateDCincl: TLabel; }
    lDateDCexcl.ClassName; { lDateDCexcl: TLabel; }
    lDatePlusMinus.ClassName; { lDatePlusMinus: TLabel; }
    lDatePlusMinusIncl.ClassName; { lDatePlusMinusIncl: TLabel; }
    lRoPlusMinus.ClassName; { lRoPlusMinus: TLabel; }
    lEpsilonPlusMinus.ClassName; { lEpsilonPlusMinus: TLabel; }
    lAdditionalStr.ClassName; { lAdditionalStr: TLabel; }
    lLwrDateDCincl.ClassName; { lLwrDateDCincl: TLabel; }
    lLwrDatePlusErrIncl.ClassName; { lLwrDatePlusErrIncl: TLabel; }
    lLwrDatePlusErrAdjustedIncl.ClassName; { lLwrDatePlusErrAdjustedIncl: TLabel; }
    lLwrDateMinusErrAdjustedIncl.ClassName; { lLwrDateMinusErrAdjustedIncl: TLabel; }
    lLwrDateMinusErrIncl.ClassName; { lLwrDateMinusErrIncl: TLabel; }
    lLwrDate95PercentIncl.ClassName; { lLwrDate95PercentIncl: TLabel; }
    lLwrDateAdjustedIncl.ClassName; { lLwrDateAdjustedIncl: TLabel; }
    eDate.ClassName; { eDate: TEdit; }
    eRo.ClassName; { eRo: TEdit; }
    eEpsilon.ClassName; { eEpsilon: TEdit; }
    eDateErr.ClassName; { eDateErr: TEdit; }
    eRoErr.ClassName; { eRoErr: TEdit; }
    eEpsilonErr.ClassName; { eEpsilonErr: TEdit; }
    eDateErrMinus.ClassName; { eDateErrMinus: TEdit; }
    eRoErrMinus.ClassName; { eRoErrMinus: TEdit; }
    eMuErrMinus.ClassName; { eMuErrMinus: TEdit; }
    eDateAdjusted.ClassName; { eDateAdjusted: TEdit; }
    eDatePlusErrAdjusted.ClassName; { eDatePlusErrAdjusted: TEdit; }
    eDateMinusErrAdjusted.ClassName; { eDateMinusErrAdjusted: TEdit; }
    eDateErrIncl.ClassName; { eDateErrIncl: TEdit; }
    eDateErrMinusIncl.ClassName; { eDateErrMinusIncl: TEdit; }
    eDatePlusErrAdjustedIncl.ClassName; { eDatePlusErrAdjustedIncl: TEdit; }
    eDateMinusErrAdjustedIncl.ClassName; { eDateMinusErrAdjustedIncl: TEdit; }
    eLwrDatePlusErrAdjustedIncl.ClassName; { eLwrDatePlusErrAdjustedIncl: TEdit; }
    eLwrDatePlusErrIncl.ClassName; { eLwrDatePlusErrIncl: TEdit; }
    eLwrDateMinusErrIncl.ClassName; { eLwrDateMinusErrIncl: TEdit; }
    eLwrDateMinusErrAdjustedIncl.ClassName; { eLwrDateMinusErrAdjustedIncl: TEdit; }
    eDateIncl.ClassName; { eDateIncl: TEdit; }
    eDateAdjustedIncl.ClassName; { eDateAdjustedIncl: TEdit; }
    Panel8.ClassName; { Panel8: TPanel; }
    eTitle.ClassName; { eTitle: TEdit; }
    pRight.ClassName; { pRight: TPanel; }
    Splitter4.ClassName; { Splitter4: TSplitter; }
    pTreeSmp.ClassName; { pTreeSmp: TPanel; }
    TreeView1.ClassName; { TreeView1: TTreeView; }
    pGraphicsResiduals.ClassName; { pGraphicsResiduals: TPanel; }
    Splitter2.ClassName; { Splitter2: TSplitter; }
    pGraphics.ClassName; { pGraphics: TPanel; }
    pCheckBoxes.ClassName; { pCheckBoxes: TPanel; }
    lEllipseMagnif.ClassName; { lEllipseMagnif: TLabel; }
    lTicksEvery.ClassName; { lTicksEvery: TLabel; }
    lMaxAgeConcordia.ClassName; { lMaxAgeConcordia: TLabel; }
    cbRegressionLine.ClassName; { cbRegressionLine: TCheckBox; }
    cbErrorEnvelope.ClassName; { cbErrorEnvelope: TCheckBox; }
    cbErrorEllipses.ClassName; { cbErrorEllipses: TCheckBox; }
    cbCurrentSample.ClassName; { cbCurrentSample: TCheckBox; }
    cbLegend.ClassName; { cbLegend: TCheckBox; }
    cbTicLabels.ClassName; { cbTicLabels: TCheckBox; }
    eTicksEvery.ClassName; { eTicksEvery: TEdit; }
    eMaxAgeConcordia.ClassName; { eMaxAgeConcordia: TEdit; }
    cbConcordiaUncertainties.ClassName; { cbConcordiaUncertainties: TCheckBox; }
    ChartReg.ClassName; { ChartReg: TChart; }
    Series8.ClassName; { Series8: TLineSeries; }
    Series5.ClassName; { Series5: TLineSeries; }
    Series4.ClassName; { Series4: TLineSeries; }
    Series3.ClassName; { Series3: TLineSeries; }
    Series10.ClassName; { Series10: TPointSeries; }
    Series9.ClassName; { Series9: TLineSeries; }
    Series1.ClassName; { Series1: TLineSeries; }
    Series6.ClassName; { Series6: TPointSeries; }
    Series2.ClassName; { Series2: TPointSeries; }
    Series11.ClassName; { Series11: TPointSeries; }
    Series12.ClassName; { Series12: TPointSeries; }
    Series13.ClassName; { Series13: TLineSeries; }
    Series14.ClassName; { Series14: TPointSeries; }
    Series7.ClassName; { Series7: TPointSeries; }
    Series15.ClassName; { Series15: TLineSeries; }
    Series16.ClassName; { Series16: TLineSeries; }
    pResiduals.ClassName; { pResiduals: TPanel; }
    lResidual.ClassName; { lResidual: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label10.ClassName; { Label10: TLabel; }
    Label13.ClassName; { Label13: TLabel; }
    dbnReg.ClassName; { dbnReg: TDBNavigator; }
    dbRegSample.ClassName; { dbRegSample: TDBEdit; }
    DBEdit2.ClassName; { DBEdit2: TDBEdit; }
    DBEdit3.ClassName; { DBEdit3: TDBEdit; }
    DBEdit5.ClassName; { DBEdit5: TDBEdit; }
    DBEdit6.ClassName; { DBEdit6: TDBEdit; }
    DBEdit7.ClassName; { DBEdit7: TDBEdit; }
    DBEdit8.ClassName; { DBEdit8: TDBEdit; }
    DBEdit9.ClassName; { DBEdit9: TDBEdit; }
    DBEdit10.ClassName; { DBEdit10: TDBEdit; }
    DBCheckBox1.ClassName; { DBCheckBox1: TDBCheckBox; }
    bbUpdate.ClassName; { bbUpdate: TBitBtn; }
    eLwrDateIncl.ClassName; { eLwrDateIncl: TEdit; }
    eLwrDateAdjustedIncl.ClassName; { eLwrDateAdjustedIncl: TEdit; }
    PrinterSetupDialog1.ClassName; { PrinterSetupDialog1: TPrinterSetupDialog; }
    PrintDialog1.ClassName; { PrintDialog1: TPrintDialog; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmConcordiaDate: TfmConcordiaDate }
  with TfmConcordiaDate(nil) do { Gd_Ccda.pas }
  begin
    pButtonsTop.ClassName; { pButtonsTop: TPanel; }
    bClose.ClassName; { bClose: TButton; }
    bSprdSheet.ClassName; { bSprdSheet: TButton; }
    bCumHist.ClassName; { bCumHist: TButton; }
    bExportForDateView.ClassName; { bExportForDateView: TButton; }
    bRecalculate.ClassName; { bRecalculate: TButton; }
    pResultsBottom.ClassName; { pResultsBottom: TPanel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    lWtAvAugmentedSD.ClassName; { lWtAvAugmentedSD: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    Label7.ClassName; { Label7: TLabel; }
    lWtAvPlus.ClassName; { lWtAvPlus: TLabel; }
    lWtAvMinus.ClassName; { lWtAvMinus: TLabel; }
    Label10.ClassName; { Label10: TLabel; }
    Label11.ClassName; { Label11: TLabel; }
    Label12.ClassName; { Label12: TLabel; }
    lErrorsBased.ClassName; { lErrorsBased: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label8.ClassName; { Label8: TLabel; }
    Label9.ClassName; { Label9: TLabel; }
    Label13.ClassName; { Label13: TLabel; }
    lNotApplicable.ClassName; { lNotApplicable: TLabel; }
    Label14.ClassName; { Label14: TLabel; }
    Label15.ClassName; { Label15: TLabel; }
    Label16.ClassName; { Label16: TLabel; }
    Label17.ClassName; { Label17: TLabel; }
    Label19.ClassName; { Label19: TLabel; }
    eDateWO.ClassName; { eDateWO: TEdit; }
    eIcnt.ClassName; { eIcnt: TEdit; }
    eXCentroid.ClassName; { eXCentroid: TEdit; }
    eDatePlusMinusWO.ClassName; { eDatePlusMinusWO: TEdit; }
    eDatePlusMinusW.ClassName; { eDatePlusMinusW: TEdit; }
    edfEquivalence.ClassName; { edfEquivalence: TEdit; }
    eYCentroid.ClassName; { eYCentroid: TEdit; }
    eProbEquivalence.ClassName; { eProbEquivalence: TEdit; }
    eMSWDEquivalence.ClassName; { eMSWDEquivalence: TEdit; }
    eDateW.ClassName; { eDateW: TEdit; }
    eProbWO.ClassName; { eProbWO: TEdit; }
    eProbW.ClassName; { eProbW: TEdit; }
    edfWO.ClassName; { edfWO: TEdit; }
    eMSWDw.ClassName; { eMSWDw: TEdit; }
    edfW.ClassName; { edfW: TEdit; }
    eMSWDwo.ClassName; { eMSWDwo: TEdit; }
    ePb76.ClassName; { ePb76: TEdit; }
    eSigma75.ClassName; { eSigma75: TEdit; }
    eSigma68.ClassName; { eSigma68: TEdit; }
    eSigma76.ClassName; { eSigma76: TEdit; }
    eFe.ClassName; { eFe: TEdit; }
    eFwo.ClassName; { eFwo: TEdit; }
    eFw.ClassName; { eFw: TEdit; }
    eNsamp.ClassName; { eNsamp: TEdit; }
    pGraphs.ClassName; { pGraphs: TPanel; }
    Splitter1.ClassName; { Splitter1: TSplitter; }
    pResultLeft.ClassName; { pResultLeft: TPanel; }
    Panel6.ClassName; { Panel6: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    lResultTitle.ClassName; { lResultTitle: TLabel; }
    eTitle.ClassName; { eTitle: TEdit; }
    pGraphRight.ClassName; { pGraphRight: TPanel; }
    Splitter4.ClassName; { Splitter4: TSplitter; }
    Panel7.ClassName; { Panel7: TPanel; }
    lEllipseMagnif.ClassName; { lEllipseMagnif: TLabel; }
    lResidual.ClassName; { lResidual: TLabel; }
    Label18.ClassName; { Label18: TLabel; }
    lMaxAgeConcordia.ClassName; { lMaxAgeConcordia: TLabel; }
    Label21.ClassName; { Label21: TLabel; }
    DBCheckBox1.ClassName; { DBCheckBox1: TDBCheckBox; }
    DBEdit3.ClassName; { DBEdit3: TDBEdit; }
    DBNavigator1.ClassName; { DBNavigator1: TDBNavigator; }
    cbCurrentSample.ClassName; { cbCurrentSample: TCheckBox; }
    cbLegend.ClassName; { cbLegend: TCheckBox; }
    cbTicLabels.ClassName; { cbTicLabels: TCheckBox; }
    eTicksEvery.ClassName; { eTicksEvery: TEdit; }
    eMaxAgeConcordia.ClassName; { eMaxAgeConcordia: TEdit; }
    eTicFormat.ClassName; { eTicFormat: TEdit; }
    ChartConcordia.ClassName; { ChartConcordia: TChart; }
    Series5.ClassName; { Series5: TLineSeries; }
    Series4.ClassName; { Series4: TLineSeries; }
    Series11.ClassName; { Series11: TLineSeries; }
    Series10.ClassName; { Series10: TLineSeries; }
    Series8.ClassName; { Series8: TPointSeries; }
    Series1.ClassName; { Series1: TLineSeries; }
    TeeFunction1.ClassName; { TeeFunction1: TSmoothingFunction; }
    Series9.ClassName; { Series9: TLineSeries; }
    Series3.ClassName; { Series3: TPointSeries; }
    Series2.ClassName; { Series2: TPointSeries; }
    Series13.ClassName; { Series13: TPointSeries; }
    Series14.ClassName; { Series14: TPointSeries; }
    Series6.ClassName; { Series6: TLineSeries; }
    Series12.ClassName; { Series12: TPointSeries; }
    Series7.ClassName; { Series7: TPointSeries; }
    Series15.ClassName; { Series15: TLineSeries; }
    Series16.ClassName; { Series16: TLineSeries; }
    pTreeSmp.ClassName; { pTreeSmp: TPanel; }
    TreeView1.ClassName; { TreeView1: TTreeView; }
    SaveDialogModels.ClassName; { SaveDialogModels: TSaveDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmWtAv: TfmWtAv }
  with TfmWtAv(nil) do { Gd_WtAv.pas }
  begin
    Splitter2.ClassName; { Splitter2: TSplitter; }
    pButtons.ClassName; { pButtons: TPanel; }
    bClose.ClassName; { bClose: TButton; }
    bSprdSheet.ClassName; { bSprdSheet: TButton; }
    bCumHist.ClassName; { bCumHist: TButton; }
    bExport.ClassName; { bExport: TButton; }
    bRecalculate.ClassName; { bRecalculate: TButton; }
    pTop.ClassName; { pTop: TPanel; }
    Splitter1.ClassName; { Splitter1: TSplitter; }
    pTopLeft.ClassName; { pTopLeft: TPanel; }
    pResultTitle.ClassName; { pResultTitle: TPanel; }
    lResultTitle.ClassName; { lResultTitle: TLabel; }
    Panel1.ClassName; { Panel1: TPanel; }
    lEvap.ClassName; { lEvap: TLabel; }
    Panel2.ClassName; { Panel2: TPanel; }
    ChCum.ClassName; { ChCum: TChart; }
    LineSeries1.ClassName; { LineSeries1: TLineSeries; }
    LineSeries2.ClassName; { LineSeries2: TLineSeries; }
    LineSeries3.ClassName; { LineSeries3: TLineSeries; }
    LineSeries4.ClassName; { LineSeries4: TLineSeries; }
    PointSeries1.ClassName; { PointSeries1: TPointSeries; }
    LineSeries5.ClassName; { LineSeries5: TLineSeries; }
    LineSeries6.ClassName; { LineSeries6: TLineSeries; }
    PointSeries2.ClassName; { PointSeries2: TPointSeries; }
    PointSeries3.ClassName; { PointSeries3: TPointSeries; }
    ErrorPointSeries1.ClassName; { ErrorPointSeries1: TErrorPointSeries; }
    ErrorPointSeries2.ClassName; { ErrorPointSeries2: TErrorPointSeries; }
    LineSeries7.ClassName; { LineSeries7: TLineSeries; }
    PointSeries4.ClassName; { PointSeries4: TPointSeries; }
    PointSeries5.ClassName; { PointSeries5: TPointSeries; }
    Series15.ClassName; { Series15: TLineSeries; }
    TeeCommander2.ClassName; { TeeCommander2: TTeeCommander; }
    pGraph.ClassName; { pGraph: TPanel; }
    Splitter5.ClassName; { Splitter5: TSplitter; }
    ChWtAv.ClassName; { ChWtAv: TChart; }
    Series8.ClassName; { Series8: TLineSeries; }
    Series5.ClassName; { Series5: TLineSeries; }
    Series4.ClassName; { Series4: TLineSeries; }
    Series3.ClassName; { Series3: TLineSeries; }
    Series10.ClassName; { Series10: TPointSeries; }
    Series9.ClassName; { Series9: TLineSeries; }
    Series1.ClassName; { Series1: TLineSeries; }
    Series6.ClassName; { Series6: TPointSeries; }
    Series2.ClassName; { Series2: TPointSeries; }
    Series11.ClassName; { Series11: TErrorPointSeries; }
    Series12.ClassName; { Series12: TErrorPointSeries; }
    Series13.ClassName; { Series13: TLineSeries; }
    Series14.ClassName; { Series14: TPointSeries; }
    Series7.ClassName; { Series7: TPointSeries; }
    TeeCommander1.ClassName; { TeeCommander1: TTeeCommander; }
    lModifyGraphSettings.ClassName; { lModifyGraphSettings: TLabel; }
    pTreeSmp.ClassName; { pTreeSmp: TPanel; }
    TreeView1.ClassName; { TreeView1: TTreeView; }
    pBottom.ClassName; { pBottom: TPanel; }
    Splitter4.ClassName; { Splitter4: TSplitter; }
    pResiduals.ClassName; { pResiduals: TPanel; }
    Label8.ClassName; { Label8: TLabel; }
    Label9.ClassName; { Label9: TLabel; }
    Label13.ClassName; { Label13: TLabel; }
    lResidual.ClassName; { lResidual: TLabel; }
    lEllipseMagnif.ClassName; { lEllipseMagnif: TLabel; }
    DBeSample.ClassName; { DBeSample: TDBEdit; }
    DBeUncertainty.ClassName; { DBeUncertainty: TDBEdit; }
    DBeMisfit.ClassName; { DBeMisfit: TDBEdit; }
    DBePercentActual.ClassName; { DBePercentActual: TDBEdit; }
    dbcbInclude.ClassName; { dbcbInclude: TDBCheckBox; }
    dbnSamples.ClassName; { dbnSamples: TDBNavigator; }
    cbLegend.ClassName; { cbLegend: TCheckBox; }
    pBottomLeft.ClassName; { pBottomLeft: TPanel; }
    Splitter3.ClassName; { Splitter3: TSplitter; }
    pBottomRight.ClassName; { pBottomRight: TPanel; }
    lCaption1.ClassName; { lCaption1: TLabel; }
    lCaption2.ClassName; { lCaption2: TLabel; }
    pWtAverages.ClassName; { pWtAverages: TPanel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    lWtAvAugmentedSD.ClassName; { lWtAvAugmentedSD: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    Label7.ClassName; { Label7: TLabel; }
    lWtAvPlus.ClassName; { lWtAvPlus: TLabel; }
    lWtAvMinus.ClassName; { lWtAvMinus: TLabel; }
    Label10.ClassName; { Label10: TLabel; }
    Label11.ClassName; { Label11: TLabel; }
    Label12.ClassName; { Label12: TLabel; }
    lWtAvAugmented95.ClassName; { lWtAvAugmented95: TLabel; }
    lErrorsBased.ClassName; { lErrorsBased: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label14.ClassName; { Label14: TLabel; }
    lWtAvIncl.ClassName; { lWtAvIncl: TLabel; }
    lWtAvPlusIncl.ClassName; { lWtAvPlusIncl: TLabel; }
    lWtAvMinusIncl.ClassName; { lWtAvMinusIncl: TLabel; }
    eWtAv.ClassName; { eWtAv: TEdit; }
    eIcnt.ClassName; { eIcnt: TEdit; }
    eWtAvExpectedSD.ClassName; { eWtAvExpectedSD: TEdit; }
    eWtAvPlus95.ClassName; { eWtAvPlus95: TEdit; }
    eWtAvMinus95.ClassName; { eWtAvMinus95: TEdit; }
    eWtAvExpected95.ClassName; { eWtAvExpected95: TEdit; }
    eWtAvObservedSD.ClassName; { eWtAvObservedSD: TEdit; }
    eWtAvObserved95.ClassName; { eWtAvObserved95: TEdit; }
    eMSWD.ClassName; { eMSWD: TEdit; }
    eWtAvAugmentedSD.ClassName; { eWtAvAugmentedSD: TEdit; }
    eWtAvAugmented95.ClassName; { eWtAvAugmented95: TEdit; }
    eProbabilityOfF.ClassName; { eProbabilityOfF: TEdit; }
    eNSamp.ClassName; { eNSamp: TEdit; }
    eWtAvPlus95Incl.ClassName; { eWtAvPlus95Incl: TEdit; }
    eWtAvMinus95Incl.ClassName; { eWtAvMinus95Incl: TEdit; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    ChartEditor1.ClassName; { ChartEditor1: TChartEditor; }
    ChartEditor2.ClassName; { ChartEditor2: TChartEditor; }
    VirtualImageListWtAv.ClassName; { VirtualImageListWtAv: TVirtualImageList; }
  end;

{ fmParam: TfmParam }
  with TfmParam(nil) do { GD_param.pas }
  begin
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    Label5.ClassName; { Label5: TLabel; }
    Label6.ClassName; { Label6: TLabel; }
    Label7.ClassName; { Label7: TLabel; }
    Label8.ClassName; { Label8: TLabel; }
    Label9.ClassName; { Label9: TLabel; }
    Label10.ClassName; { Label10: TLabel; }
    Panel1.ClassName; { Panel1: TPanel; }
    bbClose.ClassName; { bbClose: TBitBtn; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmRegUser3: TfmRegUser3 }
  with TfmRegUser3(nil) do { RegUser3.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    bbCancel.ClassName; { bbCancel: TBitBtn; }
    bbOK.ClassName; { bbOK: TBitBtn; }
    GroupBox3.ClassName; { GroupBox3: TGroupBox; }
    Label1.ClassName; { Label1: TLabel; }
    lSoftwareName.ClassName; { lSoftwareName: TLabel; }
    eRegisteredUser.ClassName; { eRegisteredUser: TEdit; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ fmModelPbPostFm: TfmModelPbPostFm }
  with TfmModelPbPostFm(nil) do { Gd_PostFm.pas }
  begin
    Panel1.ClassName; { Panel1: TPanel; }
    sbClose.ClassName; { sbClose: TSpeedButton; }
    bbSaveSheet.ClassName; { bbSaveSheet: TBitBtn; }
    Panel2.ClassName; { Panel2: TPanel; }
    Label1.ClassName; { Label1: TLabel; }
    Label2.ClassName; { Label2: TLabel; }
    Label3.ClassName; { Label3: TLabel; }
    Label4.ClassName; { Label4: TLabel; }
    eFormation.ClassName; { eFormation: TEdit; }
    eRequired.ClassName; { eRequired: TEdit; }
    bbCalculate.ClassName; { bbCalculate: TBitBtn; }
    SaveDialogSprdSheet.ClassName; { SaveDialogSprdSheet: TSaveDialog; }
    VirtualImageList1.ClassName; { VirtualImageList1: TVirtualImageList; }
  end;

{ dmImageCollection: TdmImageCollection }
  with TdmImageCollection(nil) do { ImageCollection_dm.pas }
  begin
    ImageCollection1.ClassName; { ImageCollection1: TImageCollection; }
  end;

end;

end.
