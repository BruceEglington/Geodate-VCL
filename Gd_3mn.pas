unit Gd_3mn;

// Main interface for Geodate for Windows software
// Programmed by Bruce Eglington (1995-2023)
//
// Last updated 2023
//

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Menus,
  SysUtils, System.IOUtils, System.UITypes,
  VCL.Themes,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, DB, IniFiles,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, GDW_Varb,
  SVGIconVirtualImageList;

type
  TGDW1_MainForm = class(TForm)
    MainMenuGDW1: TMainMenu;
    Panel1: TPanel;
    StatusLine: TPanel;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialogGEODATE: TOpenDialog;
    Edit1: TMenuItem;
    WindowNewSampleSelectionItem: TMenuItem;
    WindowNewRegressionResultsItem: TMenuItem;
    WindowNewRegressionGraphItem: TMenuItem;
    WindowNewEpsilonItem: TMenuItem;
    WindowNewInitialRatiosItem: TMenuItem;
    WindowNewItem: TMenuItem;
    N6: TMenuItem;
    Options1: TMenuItem;
    ToolsConvert: TMenuItem;
    Models: TMenuItem;
    Averages: TMenuItem;
    Regress1: TMenuItem;
    FileSaveAsItem: TMenuItem;
    OptionsDirectories: TMenuItem;
    SaveDialogGEODATE: TSaveDialog;
    FileSaveItem: TMenuItem;
    RegressUnconstrained: TMenuItem;
    RegressConstrained: TMenuItem;
    EditEdit: TMenuItem;
    N7: TMenuItem;
    OptionsMSWD: TMenuItem;
    FileAddItem: TMenuItem;
    ModelsTRatio: TMenuItem;
    ModelsTCHUR: TMenuItem;
    ModelsTDM: TMenuItem;
    N8: TMenuItem;
    ModelsRDate: TMenuItem;
    ModelsRIndividual: TMenuItem;
    AveragesX: TMenuItem;
    AveragesY: TMenuItem;
    AveragesZ: TMenuItem;
    OptionsPbModels: TMenuItem;
    Singlestage: TMenuItem;
    SK2stage: TMenuItem;
    Userdefined: TMenuItem;
    N9: TMenuItem;
    Models238U204Pb: TMenuItem;
    Models207Pb206Pb: TMenuItem;
    ConcordiaWt1: TMenuItem;
    OptionsNormal: TMenuItem;
    OptionsDiscordance: TMenuItem;
    HelpBugReport: TMenuItem;
    N10: TMenuItem;
    Models238U204PbDateSame: TMenuItem;
    ModelsEpsilonIndividual: TMenuItem;
    ModelsEpsilonDate: TMenuItem;
    FileImportSpreadSheet: TMenuItem;
    FileExportSpreadsheet: TMenuItem;
    N11: TMenuItem;
    ToolsConvertConcordia2TeraWasserburg: TMenuItem;
    ToolsConvertTeraWasserburg2Concordia: TMenuItem;
    Registeruser1: TMenuItem;
    Label1: TLabel;
    N5: TMenuItem;
    AverageArArPlateau: TMenuItem;
    AverageConcordia: TMenuItem;
    N12: TMenuItem;
    Memo1: TMemo;
    OptionsAlphaLevel: TMenuItem;
    Models206Pb238U: TMenuItem;
    OptionsEllipseMagnification: TMenuItem;
    OptionsEllipseMagnification1: TMenuItem;
    OptionsEllipseMagnification2: TMenuItem;
    lItemsHaveChanged: TLabel;
    ModelsRPostFormation: TMenuItem;
    ModelsTRD: TMenuItem;
    OptionsDefaultErrorType: TMenuItem;
    OptionsDefaultErrorTypePercent: TMenuItem;
    OptionsDefaultErrorTypeActual: TMenuItem;
    ModelsT2DM: TMenuItem;
    ModelsT2DMDate: TMenuItem;
    N3: TMenuItem;
    ModelsT2DMfromAgeEpsilonValues: TMenuItem;
    Styles1: TMenuItem;
    Panel2: TPanel;
    bSaveGDWFile: TButton;
    bOpen: TButton;
    bExit: TButton;
    bImportSpreadSheet: TButton;
    bEdit: TButton;
    lRegisteredUser: TLabel;
    lProjectName: TLabel;
    bRegressUnconstrained: TButton;
    bRegressConstrained: TButton;
    Test1: TMenuItem;
    N4: TMenuItem;
    FileOpenLegacy: TMenuItem;
    OpenDialogLegacyGEODATE: TOpenDialog;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FileOpenItemClick(Sender: TObject);
    procedure HelpAboutItemClick(Sender: TObject);
    procedure FileExitItemClick(Sender: TObject);
    procedure OptionsDirectoriesClick(Sender: TObject);
    procedure FileSaveAsItemClick(Sender: TObject);
    procedure FileSaveItemClick(Sender: TObject);
    procedure RegressUnconstrainedClick(Sender: TObject);
    procedure RegressConstrainedClick(Sender: TObject);
    procedure EditEditClick(Sender: TObject);
    procedure OptionsMSWDClick(Sender: TObject);
    procedure FileAddItemClick(Sender: TObject);
    procedure SinglestageClick(Sender: TObject);
    procedure SK2stageClick(Sender: TObject);
    procedure UserdefinedClick(Sender: TObject);
    procedure OptionsDiscordanceClick(Sender: TObject);
    procedure OptionsNormalClick(Sender: TObject);
    procedure Models207Pb206PbClick(Sender: TObject);
    procedure AveragesXClick(Sender: TObject);
    procedure AveragesYClick(Sender: TObject);
    procedure AveragesZClick(Sender: TObject);
    procedure ModelsTRatioClick(Sender: TObject);
    procedure ModelsTCHURClick(Sender: TObject);
    procedure ModelsTDMClick(Sender: TObject);
    procedure ModelsRDateClick(Sender: TObject);
    procedure ModelsRIndividualClick(Sender: TObject);
    procedure Models238U204PbClick(Sender: TObject);
    procedure ModelsEpsilonDateClick(Sender: TObject);
    procedure ModelsEpsilonIndividualClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FileExportSpreadsheetClick(Sender: TObject);
    procedure FileImportSpreadsheetClick(Sender: TObject);
    procedure ToolsConvertConcordia2TeraWasserburgClick(Sender: TObject);
    procedure Registeruser1Click(Sender: TObject);
    procedure HelpBugReportClick(Sender: TObject);
    procedure ToolsConvertTeraWasserburg2ConcordiaClick(Sender: TObject);
    procedure ResultsTable1Click(Sender: TObject);
    procedure DatabaseTables1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AverageConcordiaClick(Sender: TObject);
    procedure OptionsAlphaLevelClick(Sender: TObject);
    procedure AverageArArPlateauClick(Sender: TObject);
    procedure Models206Pb238UClick(Sender: TObject);
    procedure OptionsEllipseMagnification1Click(Sender: TObject);
    procedure OptionsEllipseMagnification2Click(Sender: TObject);
    procedure ModelsRPostFormationClick(Sender: TObject);
    procedure ModelsTRDClick(Sender: TObject);
    procedure OptionsDefaultErrorTypeActualClick(Sender: TObject);
    procedure OptionsDefaultErrorTypePercentClick(Sender: TObject);
    procedure ModelsT2DMClick(Sender: TObject);
    procedure ModelsT2DMDateClick(Sender: TObject);
    procedure ModelsT2DMfromAgeEpsilonValuesClick(Sender: TObject);
    procedure StyleClick(Sender: TObject);
    procedure Test1Click(Sender: TObject);
    procedure FileOpenLegacyClick(Sender: TObject);
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
    procedure ShowHint(Sender: TObject);
    procedure CheckAcceptableUser;
    procedure CheckMuChoice;
  public
    { Public declarations }
    //ChosenStyle : string;
    procedure GetIniFile;
    procedure SetIniFile;
    procedure GDWSetEnabled;
  end;

var
  GDW1_MainForm  : TGDW1_MainForm;
  StartUp        : boolean = true;

implementation

uses
  allsorts, gd_about, gd_file,
  gdw_reg1, gd_drv, Gd_edit, Gd_MSWD, gdw_regp,
  gd_WtAv, Gd_MdlAt, reguser3,
  gd_sht, Gd_shtim, Gd_Ccda, Gd_Plat2,
  Gd_IsotopeType,
  Gd_PostFm, midaslib, dmGdMSWD, dmGdtmpDB;

{$R *.DFM}

var
  EditForm     : TfmEdit;
  RegressForm  : TfmRegressionResult;
  IsotopeSystemForm : TfmIsotopeSystemType;
  MSWDForm     : TfmMSWD;
  WtAvForm     : TfmWtAv;
  ConcordiaDateForm : TfmConcordiaDate;
  GetMdlAtForm : TfmGetMdlAt;
  OptDirForm   : TfmOptDir;
  //NewDataForm  : TfmNewData;
  //SprdSheetForm : TfmSheet;
  SprdSheetImportForm : TfmSheetImport;
  RegisterUserForm : TfmRegUser3;
  //BugReportForm : TfmBugReport;
  //GenSheetForm  : TfmGenSheet;
  PlatArForm    : TfmPlatAr;

procedure TGDW1_MainForm.CheckAcceptableUser;
var
  i : integer;
begin
  AcceptableUser := false;
  i := Pos('bre',RegisteredUser);
  if (i > 0) then AcceptableUser := true;
end;

procedure TGDW1_MainForm.FormCreate(Sender: TObject);
var
  Style: String;
  Item: TMenuItem;
begin
  //MainPanel.Align := alClient;
  { position the form at the top of display }
  //Left := 0;
  //Top := 0;
  Application.OnHint := ShowHint;
  DateString := DateToStr(Date);
  {
  if AllowMessages then ShowMessage('Date = '+DateString);
  }
  GetIniFile;
  TStyleManager.TrySetStyle(GlobalChosenStyle);
  //Add child menu items based on available styles.
  for Style in TStyleManager.StyleNames do
  begin
    Item := TMenuItem.Create(Styles1);
    Item.Caption := Style;
    Item.OnClick := StyleClick;
    if TStyleManager.ActiveStyle.Name = Style then
      Item.Checked := true;
    Styles1.Add(Item);
  end;
end;

procedure TGDW1_MainForm.ShowHint(Sender: TObject);
begin
  StatusLine.Caption := Application.Hint;
end;

procedure TGDW1_MainForm.GetIniFile;
var
  AppIni   : TIniFile;
  i        : integer;
  tmpStr   : string;
  tmpStr1  : string;
  ICode    : integer;
  PublicPath : string;
  tmpTracerStr : string;
  IniFileName : string;
begin
  tmpStr := '1';
  //PublicPath := TPath.GetPublicPath;
  //CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  //Used to use CSIDL_COMMON_APPDATA but some users do not have access to this
  //and don't know how to change their system settings and permissions to all
  //software to write to this path.
  //Now changed to use CSIDL_COMMON_DOCUMENTS which automatically permits
  //all users to have both read and write permission
  PublicPath := TPath.GetHomePath;
  CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  IniFilename := CommonFilePath + 'Geodate.ini';
  AppIni := TIniFile.Create(IniFilename);
  try
    RegisteredUser := AppIni.ReadString('Registration','Registered user','not defined');
    AllowMessages := AppIni.ReadBool('Development','Allow Messages',false);
    GlobalChosenStyle := AppIni.ReadString('Styles','Chosen style','Windows');
    if (GlobalChosenStyle = '') then GlobalChosenStyle := 'Windows';
    dmGDWtmp.ChosenStyle := GlobalChosenStyle;
    {
    GdwNetFileDir := AppIni.ReadString('Default directories','NetFileDir','C:\');
    }
    Drive1 := AppIni.ReadString('Default directories','Drive1',CommonFilePath+'Geodate\');
    Drive2 := AppIni.ReadString('Default directories','Drive2',CommonFilePath+'Geodate\DATA\');
    Drive3 := AppIni.ReadString('Default directories','Drive3',CommonFilePath+'Geodate\TEMP\');
    cdsPath := AppIni.ReadString('Default directories','cds',CommonFilePath+'Geodate\');
    LastCountry := AppIni.ReadString('Last country','LastCountry','SA');
    tmpStr := AppIni.ReadString('Statistics','Alpha','0.050');
    Val(tmpStr,FAlpha,ICode);
    if (FAlpha < 0.01) then FAlpha := 0.050;
    tmpStr := AppIni.ReadString('Statistics','Ellipse magnifier','2');
    if (tmpStr = '1') then
    begin
      OptionsEllipseMagnification1.Checked := true;
      OptionsEllipseMagnification2.Checked := false;
      EllipseMagnif := 1.0;
    end else
    begin
      OptionsEllipseMagnification1.Checked := false;
      OptionsEllipseMagnification2.Checked := true;
      EllipseMagnif := 1.96;
    end;
    tmpStr := AppIni.ReadString('Last analysis type','AnalType','1');
    AnalType := tmpStr[1];
    iAnalTyp := Get_IAnal_from_AnalType(AnalType);
    for i := 0 to Maxtype do
    begin
      Process[i] := AppIni.ReadString(Process[i],'Method',DefaultProcess[i]);
      XRatioStr[i] := AppIni.ReadString(Process[i],'X Ratio',DefaultXRatioStr[i]);
      YRatioStr[i] := AppIni.ReadString(Process[i],'Y Ratio',DefaultYRatioStr[i]);
      ZRatioStr[i] := AppIni.ReadString(Process[i],'Z Ratio',DefaultZRatioStr[i]);
      WRatioStr[i] := AppIni.ReadString(Process[i],'W Ratio',DefaultWRatioStr[i]);
      Element[i,1] := AppIni.ReadString(Process[i],'X element',DefaultElement[i,1]);
      Element[i,2] := AppIni.ReadString(Process[i],'Y element',DefaultElement[i,2]);
      Element[i,3] := AppIni.ReadString(Process[i],'Z element',DefaultElement[i,3]);
      GraphXRatioStr[i] := AppIni.ReadString(Process[i],'Graph X Ratio',DefaultGraphXRatioStr[i]);
      GraphYRatioStr[i] := AppIni.ReadString(Process[i],'Graph Y Ratio',DefaultGraphYRatioStr[i]);
      GraphZRatioStr[i] := AppIni.ReadString(Process[i],'Graph Z Ratio',DefaultGraphZRatioStr[i]);
      tmpStr1 := DefaultDecayConstantSource[i];
      DecayConstantSource[i] := AppIni.ReadString(Process[i],'Decay Constant source',tmpStr1);
      DecayConst[i] := StrToFloat(AppIni.ReadString(Process[i],
        'Decay Constant',FormatFloat('0.00000E+00',DefaultDecayConst[i])));
      DecayConstUncertainty[i] := StrToFloat(AppIni.ReadString(Process[i],
        'Decay Constant % Uncertainty',FormatFloat('00.00000',DefaultDecayConstUncertainty[i])));
      TracerUncertainty[i] := StrToFloat(AppIni.ReadString(Process[i],
        'Tracer % Uncertainty',FormatFloat('0.000000',DefaultTracerUncertainty[i])));

      if (i in [1,2,7,9,14,15,16]) then
      begin
        tmpStr1 := DefaultCHURModelName[i];
        CHURModelName[i] := AppIni.ReadString(Process[i],'CHUR model name',tmpStr1);
        CHUR[i,1] := StrToFloat(AppIni.ReadString(Process[i],
          'CHUR 1',FormatFloat('0.0000000E+00',DefaultCHUR[i,1])));
        CHUR[i,2] := StrToFloat(AppIni.ReadString(Process[i],
          'CHUR 2',FormatFloat('0.0000000E+00',DefaultCHUR[i,2])));
        CHUR[i,3] := StrToFloat(AppIni.ReadString(Process[i],
          'CHUR 3',FormatFloat('0.0000000E+00',DefaultCHUR[i,3])));
        tmpStr1 := DefaultDMModelName[i];
        DMModelName[i] := AppIni.ReadString(Process[i],'DM model name',tmpStr1);
        DM[i,1] := StrToFloat(AppIni.ReadString(Process[i],
          'DM 1',FormatFloat('0.00000000E+00',DefaultDM[i,1])));
        DM[i,2] := StrToFloat(AppIni.ReadString(Process[i],
          'DM 2',FormatFloat('0.00000000E+00',DefaultDM[i,2])));
        DM[i,3] := StrToFloat(AppIni.ReadString(Process[i],
          'DM 3',FormatFloat('0.00000000E+00',DefaultDM[i,3])));
        tmpStr1 := DefaultCCModelName[i];
        CCModelName[i] := AppIni.ReadString(Process[i],'CC model name',tmpStr1);
        CC[i,1] := StrToFloat(AppIni.ReadString(Process[i],
          'CC 1',FormatFloat('0.0000000E+00',DefaultCC[i,1])));
        CC[i,2] := StrToFloat(AppIni.ReadString(Process[i],
          'CC 2',FormatFloat('0.0000000E+00',DefaultCC[i,2])));
        CC[i,3] := StrToFloat(AppIni.ReadString(Process[i],
          'CC 3',FormatFloat('0.0000000E+00',DefaultCC[i,3])));
      end;
      CalcFac[i,1] := StrToFloat(AppIni.ReadString(Process[i],
        'Calc Fac 1',FormatFloat('0.000000E+00',DefaultCalcFac[i,1])));
      CalcFac[i,2] := StrToFloat(AppIni.ReadString(Process[i],
        'Calc Fac 2',FormatFloat('0.000000E+00',DefaultCalcFac[i,2])));
      if (i = 3) then
      begin
        MuV[0,1] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start Date',FormatFloat('0.000E+00',DefaultMuV[0,1])));
        MuV[0,2] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 206Pb/204Pb',FormatFloat('00.000',DefaultMuV[0,2])));
        MuV[0,3] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 207Pb/204Pb',FormatFloat('00.000',DefaultMuV[0,3])));
        MuV[0,4] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 208Pb/204Pb',FormatFloat('00.000',DefaultMuV[0,4])));
        MuV[0,5] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 238U/204Pb',FormatFloat('00.000',DefaultMuV[0,5])));
        MuV[1,1] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start Date',FormatFloat('0.000E+00',DefaultMuV[1,1])));
        MuV[1,2] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 206Pb/204Pb',FormatFloat('00.000',DefaultMuV[1,2])));
        MuV[1,3] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 207Pb/204Pb',FormatFloat('00.000',DefaultMuV[1,3])));
        MuV[1,4] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 208Pb/204Pb',FormatFloat('00.000',DefaultMuV[1,4])));
        MuV[1,5] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 238U/204Pb',FormatFloat('00.000',DefaultMuV[1,5])));
        MuV[2,1] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start Date',FormatFloat('0.000E+00',DefaultMuV[2,1])));
        MuV[2,2] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 206Pb/204Pb',FormatFloat('00.000',DefaultMuV[2,2])));
        MuV[2,3] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 207Pb/204Pb',FormatFloat('00.000',DefaultMuV[2,3])));
        MuV[2,4] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 208Pb/204Pb',FormatFloat('00.000',DefaultMuV[2,4])));
        MuV[2,5] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 238U/204Pb',FormatFloat('00.000',DefaultMuV[2,5])));
        mu_choice := Round(StrToFloat(AppIni.ReadString(Process[i],
          'Default model','1')));
        if (mu_choice = 0) then
        begin
          Singlestage.Checked := true;
          SK2Stage.Checked := false;
          UserDefined.Checked := false;
        end;
        if (mu_choice = 1) then
        begin
          Singlestage.Checked := false;
          SK2Stage.Checked := true;
          UserDefined.Checked := false;
        end;
        if (mu_choice = 2) then
        begin
          Singlestage.Checked := false;
          SK2Stage.Checked := false;
          UserDefined.Checked := true;
        end;
      end;
      if (i in [8]) then
      begin
        U238U235 := StrToFloat(AppIni.ReadString(Process[i],
        '238U/235U',FormatFloat('000.000',DefaultU238U235)));
        tmpStr := AppIni.ReadString(Process[i],'Default weighting method','N');
        ClearNull(tmpStr);
        if (tmpStr = 'N') then
        begin
          OptionsNormal.Checked := true;
          OptionsDiscordance.Checked := false;
        end;
        if (tmpStr = 'U') then
        begin
          OptionsNormal.Checked := false;
          OptionsDiscordance.Checked := true;
        end;
      end;
      if (i in [17]) then
      begin
        BlanketZErrVal := StrToFloat(AppIni.ReadString(Process[i],
          'Default minimum 207Pb/206Pb','0.001'));
      end;
    end;
    GraphColour[1,1] := AppIni.ReadInteger('Graph colours','Points included Red',255);
    GraphColour[1,2] := AppIni.ReadInteger('Graph colours','Points included Blue',000);
    GraphColour[1,3] := AppIni.ReadInteger('Graph colours','Points included Green',000);
    GraphColour[2,1] := AppIni.ReadInteger('Graph colours','Points excluded Red',000);
    GraphColour[2,2] := AppIni.ReadInteger('Graph colours','Points excluded Blue',255);
    GraphColour[2,3] := AppIni.ReadInteger('Graph colours','Points excluded Green',000);
    GraphColour[3,1] := AppIni.ReadInteger('Graph colours','Ellipse included Red',255);
    GraphColour[3,2] := AppIni.ReadInteger('Graph colours','Ellipse included Blue',255);
    GraphColour[3,3] := AppIni.ReadInteger('Graph colours','Ellipse included Green',000);
    GraphColour[4,1] := AppIni.ReadInteger('Graph colours','Ellipse excluded Red',000);
    GraphColour[4,2] := AppIni.ReadInteger('Graph colours','Ellipse excluded Blue',255);
    GraphColour[4,3] := AppIni.ReadInteger('Graph colours','Ellipse excluded Green',000);
    GraphColour[5,1] := AppIni.ReadInteger('Graph colours','Regression line Red',000);
    GraphColour[5,2] := AppIni.ReadInteger('Graph colours','Regression line Blue',000);
    GraphColour[5,3] := AppIni.ReadInteger('Graph colours','Regression line Green',255);
    GraphColour[6,1] := AppIni.ReadInteger('Graph colours','Error envelope Red',000);
    GraphColour[6,2] := AppIni.ReadInteger('Graph colours','Error envelope Blue',000);
    GraphColour[6,3] := AppIni.ReadInteger('Graph colours','Error envelope Green',200);
    GraphColour[7,1] := AppIni.ReadInteger('Graph colours','Concordia and model curves Red',000);
    GraphColour[7,2] := AppIni.ReadInteger('Graph colours','Concordia and model curves Blue',000);
    GraphColour[7,3] := AppIni.ReadInteger('Graph colours','Concordia and model curves Green',000);
    GraphColour[8,1] := AppIni.ReadInteger('Graph colours','Concordia date ellipse Red',000);
    GraphColour[8,2] := AppIni.ReadInteger('Graph colours','Concordia date ellipse Blue',000);
    GraphColour[8,3] := AppIni.ReadInteger('Graph colours','Concordia date ellipse Green',000);
    GraphColour[9,1] := AppIni.ReadInteger('Graph colours','Cumulative histogram Red',255);
    GraphColour[9,2] := AppIni.ReadInteger('Graph colours','Cumulative histogram Blue',255);
    GraphColour[9,3] := AppIni.ReadInteger('Graph colours','Cumulative histogram Green',000);
  finally
    AppIni.Free;
  end;
end;

procedure TGDW1_MainForm.SetIniFile;
var
  AppIni   : TIniFile;
  i        : integer;
  PublicPath : string;
  IniFileName : string;
begin
  //PublicPath := TPath.GetPublicPath;
  //CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  //Used to use CSIDL_COMMON_APPDATA but some users do not have access to this
  //and don't know how to change their system settings and permissions to all
  //software to write to this path.
  //Now changed to use CSIDL_COMMON_DOCUMENTS which automatically permits
  //all users to have both read and write permission
  PublicPath := TPath.GetHomePath;
  CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  IniFilename := CommonFilePath + 'Geodate.ini';
  AppIni := TIniFile.Create(IniFilename);
  try
    AppIni.WriteString('Registration','Registered user',RegisteredUser);
    AppIni.WriteBool('Development','Allow Messages',AllowMessages);
    AppIni.WriteString('Styles','Chosen style',GlobalChosenStyle);
    {
    AppIni.WriteString('Default directories','NetFileDir',GdwNetFileDir);
    }
    AppIni.WriteString('Default directories','Drive1',Drive1);
    AppIni.WriteString('Default directories','Drive2',Drive2);
    AppIni.WriteString('Default directories','Drive3',Drive3);
    AppIni.WriteString('Default directories','cds',cdsPath);
    AppIni.WriteString('Last country','LastCountry',LastCountry);
    AppIni.WriteString('Statistics','Alpha',FormatFloat('0.000',FAlpha));
    if (OptionsEllipseMagnification1.Checked)
      then AppIni.WriteString('Statistics','Ellipse magnifier','1')
      else AppIni.WriteString('Statistics','Ellipse magnifier','2');
    AppIni.WriteString('Last analysis type','AnalType',AnalType);
    for i := 0 to MaxType do
    begin
      AppIni.WriteString(Process[i],'Method',Process[i]);
      AppIni.WriteString(Process[i],'X Ratio',XRatioStr[i]);
      AppIni.WriteString(Process[i],'Y Ratio',YRatioStr[i]);
      AppIni.WriteString(Process[i],'Z Ratio',ZRatioStr[i]);
      AppIni.WriteString(Process[i],'W Ratio',WRatioStr[i]);
      AppIni.WriteString(Process[i],'X element',Element[i,1]);
      AppIni.WriteString(Process[i],'Y element',Element[i,2]);
      AppIni.WriteString(Process[i],'Z element',Element[i,3]);
      AppIni.WriteString(Process[i],'Graph X Ratio',GraphXRatioStr[i]);
      AppIni.WriteString(Process[i],'Graph Y Ratio',GraphYRatioStr[i]);
      AppIni.WriteString(Process[i],'Graph Z Ratio',GraphZRatioStr[i]);
      if (DecayConst[i] > 0.0) then
      begin
        AppIni.WriteString(Process[i],'Decay Constant source',DecayConstantSource[i]);
        AppIni.WriteString(Process[i],'Decay Constant',FormatFloat('0.00000E+00',DecayConst[i]));
        AppIni.WriteString(Process[i],'Decay Constant % Uncertainty',FormatFloat('00.00000',DecayConstUncertainty[i]));
      end;
      AppIni.WriteString(Process[i],'Tracer % Uncertainty',FormatFloat('0.000000',TracerUncertainty[i]));
      if (i in [1,2,7,9,14,15,16]) then
      begin
        AppIni.WriteString(Process[i],'CHUR model name',CHURModelName[i]);
        AppIni.WriteString(Process[i],'CHUR 1',FormatFloat('0.0000000E+00',CHUR[i,1]));
        AppIni.WriteString(Process[i],'CHUR 2',FormatFloat('0.0000000E+00',CHUR[i,2]));
        AppIni.WriteString(Process[i],'CHUR 3',FormatFloat('0.0000000E+00',CHUR[i,3]));
        AppIni.WriteString(Process[i],'DM model name',DMModelName[i]);
        AppIni.WriteString(Process[i],'DM 1',FormatFloat('0.00000000E+00',DM[i,1]));
        AppIni.WriteString(Process[i],'DM 2',FormatFloat('0.00000000E+00',DM[i,2]));
        AppIni.WriteString(Process[i],'DM 3',FormatFloat('0.00000000E+00',DM[i,3]));
        AppIni.WriteString(Process[i],'CC model name',CCModelName[i]);
        AppIni.WriteString(Process[i],'CC 1',FormatFloat('0.00000000E+00',CC[i,1]));
        AppIni.WriteString(Process[i],'CC 2',FormatFloat('0.00000000E+00',CC[i,2]));
        AppIni.WriteString(Process[i],'CC 3',FormatFloat('0.00000000E+00',CC[i,3]));
      end;
      if ((CalcFac[i,1] <> 0.0) and (CalcFac[i,2] <> 0.0)) then
      begin
        AppIni.WriteString(Process[i],'Calc Fac 1',FormatFloat('0.000000E+00',CalcFac[i,1]));
        AppIni.WriteString(Process[i],'Calc Fac 2',FormatFloat('0.000000E+00',CalcFac[i,2]));
      end;
      if (i = 3) then
      begin
        AppIni.WriteString(Process[i],'Single stage model Start Date',FormatFloat('0.000E+00',MuV[0,1]));
        AppIni.WriteString(Process[i],'Single stage model Start 206Pb/204Pb',FormatFloat('00.000',MuV[0,2]));
        AppIni.WriteString(Process[i],'Single stage model Start 207Pb/204Pb',FormatFloat('00.000',MuV[0,3]));
        AppIni.WriteString(Process[i],'Single stage model Start 208Pb/204Pb',FormatFloat('00.000',MuV[0,4]));
        AppIni.WriteString(Process[i],'Single stage model Start 238U/204Pb',FormatFloat('00.000',MuV[0,5]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start Date',FormatFloat('0.000E+00',MuV[1,1]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 206Pb/204Pb',FormatFloat('00.000',MuV[1,2]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 207Pb/204Pb',FormatFloat('00.000',MuV[1,3]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 208Pb/204Pb',FormatFloat('00.000',MuV[1,4]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 238U/204Pb',FormatFloat('00.000',MuV[1,5]));
        AppIni.WriteString(Process[i],'User defined model Start Date',FormatFloat('0.000E+00',MuV[2,1]));
        AppIni.WriteString(Process[i],'User defined model Start 206Pb/204Pb',FormatFloat('00.000',MuV[2,2]));
        AppIni.WriteString(Process[i],'User defined model Start 207Pb/204Pb',FormatFloat('00.000',MuV[2,3]));
        AppIni.WriteString(Process[i],'User defined model Start 208Pb/204Pb',FormatFloat('00.000',MuV[2,4]));
        AppIni.WriteString(Process[i],'User defined model Start 238U/204Pb',FormatFloat('00.000',MuV[2,5]));
        AppIni.WriteString(Process[i],'Default model',FormatFloat('0',1.0*mu_choice));
      end;
      if (i in [8]) then
      begin
        if (OptionsNormal.Checked) then AnalType8 := 'N';
        if (OptionsDiscordance.Checked) then AnalType8 := 'U';
        AppIni.WriteString(Process[i],'Default weighting method',AnalType8);
        AppIni.WriteString(Process[i],'238U/235U',FormatFloat('000.000',U238U235));
      end;
      if (i in [17]) then
      begin
        AppIni.WriteString(Process[i],'Default minimum 207Pb/206Pb',FormatFloat('0.00000',BlanketZErrVal));
      end;
    end;
    AppIni.WriteInteger('Graph colours','Points included Red',GraphColour[1,1]);
    AppIni.WriteInteger('Graph colours','Points included Blue',GraphColour[1,2]);
    AppIni.WriteInteger('Graph colours','Points included Green',GraphColour[1,3]);
    AppIni.WriteInteger('Graph colours','Points excluded Red',GraphColour[2,1]);
    AppIni.WriteInteger('Graph colours','Points excluded Blue',GraphColour[2,2]);
    AppIni.WriteInteger('Graph colours','Points excluded Green',GraphColour[2,3]);
    AppIni.WriteInteger('Graph colours','Ellipse included Red',GraphColour[3,1]);
    AppIni.WriteInteger('Graph colours','Ellipse included Blue',GraphColour[3,2]);
    AppIni.WriteInteger('Graph colours','Ellipse included Green',GraphColour[3,3]);
    AppIni.WriteInteger('Graph colours','Ellipse excluded Red',GraphColour[4,1]);
    AppIni.WriteInteger('Graph colours','Ellipse excluded Blue',GraphColour[4,2]);
    AppIni.WriteInteger('Graph colours','Ellipse excluded Green',GraphColour[4,3]);
    AppIni.WriteInteger('Graph colours','Regression line Red',GraphColour[5,1]);
    AppIni.WriteInteger('Graph colours','Regression line Blue',GraphColour[5,2]);
    AppIni.WriteInteger('Graph colours','Regression line Green',GraphColour[5,3]);
    AppIni.WriteInteger('Graph colours','Error envelope Red',GraphColour[6,1]);
    AppIni.WriteInteger('Graph colours','Error envelope Blue',GraphColour[6,2]);
    AppIni.WriteInteger('Graph colours','Error envelope Green',GraphColour[6,3]);
    AppIni.WriteInteger('Graph colours','Concordia and model curves Red',GraphColour[7,1]);
    AppIni.WriteInteger('Graph colours','Concordia and model curves Blue',GraphColour[7,2]);
    AppIni.WriteInteger('Graph colours','Concordia and model curves Green',GraphColour[7,3]);
    AppIni.WriteInteger('Graph colours','Concordia date ellipse Red',GraphColour[8,1]);
    AppIni.WriteInteger('Graph colours','Concordia date ellipse Blue',GraphColour[8,2]);
    AppIni.WriteInteger('Graph colours','Concordia date ellipse Green',GraphColour[8,3]);
    AppIni.WriteInteger('Graph colours','Cumulative histogram Red',GraphColour[9,1]);
    AppIni.WriteInteger('Graph colours','Cumulative histogram Blue',GraphColour[9,2]);
    AppIni.WriteInteger('Graph colours','Cumulative histogram Green',GraphColour[9,3]);
  finally
    AppIni.Free;
  end;
end;

procedure TGDW1_MainForm.FileOpenItemClick(Sender: TObject);
var
  ICode : integer;
  PosDot : integer;
begin
  OpenDialogGEODATE.InitialDir := Drive2;
  iAnalTyp := Get_IAnal_from_AnalType(AnalType);
  //ShowMessage('0 '+AnalType+' '+FormatFloat('#0.#',1.0*iAnalTyp));
  {
  AnalType := UpCase(AnalType);
  if CharInSet(AnalType,['0','1','2','3','4','5','6','7','8','9'])
  //if (AnalType in ['0','1','2','3','4','5','6','7','8','9'])
    then Val(AnalType, iAnalTyp, ICode)
    else begin
      if (AnalType = 'A') then iAnalTyp := 10;
      if (AnalType = 'B') then iAnalTyp := 11;
      if (AnalType = 'C') then iAnalTyp := 12;
      if (AnalType = 'D') then iAnalTyp := 13;
      if (AnalType = 'E') then iAnalTyp := 14;
      if (AnalType = 'F') then iAnalTyp := 15;
      if (AnalType = 'G') then iAnalTyp := 16;
      if (AnalType = 'H') then iAnalTyp := 17;
      if (AnalType = 'I') then iAnalTyp := 18;
      if (AnalType = 'J') then iAnalTyp := 19;
      if CharInSet(AnalType,['A','B','C','D','E','F','G','H','I','J'])
      //if (AnalType in ['A','B','C','D','E','F','G','H','I'])
        then ICode := 0
        else ICode := -1;
    end;
    }
  if (iAnalTyp < 0) then Exit;
  OpenDialogGEODATE.FilterIndex := iAnalTyp + 1;
  if OpenDialogGEODATE.Execute then
  begin
    Drive2 := ExtractFileDir(OpenDialogGEODATE.FileName);
    SPath := ExtractFilePath(OpenDialogGEODATE.FileName);
    AssignFile(Geodate_file, OpenDialogGEODATE.FileName);
    AnalType := OpenDialogGEODATE.FileName[Length(OpenDialogGEODATE.FileName)];
    {
    AnalType := UpCase(AnalType);
    if CharInSet(AnalType,['0','1','2','3','4','5','6','7','8','9'])
    //if (AnalType in ['0','1','2','3','4','5','6','7','8','9'])
      then Val(AnalType, iAnalTyp, ICode)
      else begin
        if (AnalType = 'A') then iAnalTyp := 10;
        if (AnalType = 'B') then iAnalTyp := 11;
        if (AnalType = 'C') then iAnalTyp := 12;
        if (AnalType = 'D') then iAnalTyp := 13;
        if (AnalType = 'E') then iAnalTyp := 14;
        if (AnalType = 'F') then iAnalTyp := 15;
        if (AnalType = 'G') then iAnalTyp := 16;
        if (AnalType = 'H') then iAnalTyp := 17;
        if (AnalType = 'I') then iAnalTyp := 18;
        if (AnalType = 'J') then iAnalTyp := 19;
        if CharInSet(AnalType,['A','B','C','D','E','F','G','H','I','J'])
        //if (AnalType in ['A','B','C','D','E','F','G','H','I'])
          then ICode := 0
          else ICode := -1;
      end;
    }
    iAnalTyp := Get_Ianal_from_AnalType(AnalType);
    if (iAnalTyp < 0) then Exit;
    if ((Sender = FileOpenItem) or (Sender = bOpen)) then
    begin
      NumberOfPoints := 0;
      ProcessOption := 'N';
      Age := 0.0;
      Intercept := 0.0;
      InitRatio := 0.0;
      lProjectName.Visible := true;
      ProjectName := ExtractFileName(OpenDialogGEODATE.FileName);
      //ShowMessage(ProjectName);
      PosDot := Pos('.',ProjectName);
      ProjectName := Copy(ProjectName,1,PosDot-1);
      //ShowMessage(ProjectName);
      //ProjectName[0] := Char(Length(ProjectName)-4);
      lProjectName.Caption := 'Project = '+ProjectName;
      lItemsHaveChanged.Visible := false;
      ItemsHaveChanged := false;
    end;
    if (Sender = FileAddItem) then
    begin
      if AllowMessages then
        MessageDlg(OpenDialogGEODATE.FileName+' added', mtInformation,[mbOk], 0);
      ProcessOption := 'F';
    end;
    Reset(Geodate_file);
    Get_Geodate_File_Data;
    CloseFile(Geodate_file);
    {
    SPath := OpenDialogGEODATE.FileName;
    }
    try
      dmMSWD.cdsF.FileName := cdsPath+'TableF.cds';
      dmMSWD.cdsF.LoadFromFile(dmMSWD.cdsF.FileName);
    except
      MessageDlg('Unable to open MSWD definition file',mtWarning,[mbOK],0);
    end;
    try
      MSWDForm := TfmMSWD.Create(Self);
      MSWDForm.ExitBtnClick(Sender);
    finally
      MSWDForm.Free;
    end;
    GDWSetEnabled;
    FileAddItem.Enabled := true;
    bSaveGDWFile.Enabled := true;
    FileSaveItem.Enabled := true;
    FileSaveAsItem.Enabled := true;
    Edit1.Enabled := true;
    Regress1.Enabled := true;
    Models.Enabled := true;
    Averages.Enabled := true;
  end;
  //ShowMessage('1 '+AnalType+' '+FormatFloat('#0.#',1.0*iAnalTyp));
end;

procedure TGDW1_MainForm.GDWSetEnabled;
begin
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
    //  A = Tera-Wasserburg
    //  B = K-Ar
    //  C = Ar-Ar
    //  D = Ar inverse
    //  E = K-Ca
    //  F = Re-Os
    //  G = La-Ba
    //  H = Evaporation Pb
    //  I = Ar plateau
    //  J = T(2DM) from Age-Epsilon value pairs
    FileExportSpreadsheet.Enabled := true;
    Edit1.Enabled := true;
    EditEdit.Enabled := true;
    bEdit.Enabled := true;
    Regress1.Enabled := true;
    bRegressConstrained.Enabled := true;
    bRegressUnconstrained.Enabled := true;
    Models.Enabled := false;
    Averages.Enabled := true;
    AveragesX.Caption := '&X : '+XRatioStr[iAnalTyp];
    AveragesY.Caption := '&Y : '+YRatioStr[iAnalTyp];
    AveragesZ.Caption := '&Z : '+ZRatioStr[iAnalTyp];
    Models238U204Pb.Enabled := false;
    Models238U204PbDateSame.Enabled := false;
    Models207Pb206Pb.Enabled := false;
    Models206Pb238U.Enabled := false;
    ModelsTRatio.Enabled := false;
    ModelsTCHUR.Enabled := false;
    ModelsTDM.Enabled := false;
    ModelsT2DM.Enabled := false;
    ModelsT2DMDate.Enabled := false;
    ModelsTRD.Enabled := false;
    ModelsRDate.Enabled := false;
    ModelsRIndividual.Enabled := false;
    ModelsEpsilonDate.Enabled := false;
    ModelsEpsilonIndividual.Enabled := false;
    ModelsT2DMfromAgeEpsilonValues.Enabled := false;
    AverageConcordia.Enabled := false;
    AverageArArPlateau.Enabled := false;
    ModelsRPostFormation.Enabled := false;
    ModelsTCHUR.Caption := 'T (&CHUR)';
    case AnalType of
      '0' : begin
      end;
      '1' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTDM.Enabled := true;
        ModelsT2DM.Enabled := true;
        ModelsT2DMDate.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
      end;
      '2' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTDM.Enabled := true;
        ModelsT2DM.Enabled := true;
        ModelsT2DMDate.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
      end;
      '3' : begin
        Models.Enabled := true;
        Models238U204Pb.Enabled := true;
        Models238U204PbDateSame.Enabled := true;
        Models207Pb206Pb.Enabled := true;
        ModelsRPostFormation.Enabled := true;
      end;
      '4' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
      end;
      '5' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
      end;
      '6' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
      end;
      '7' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTDM.Enabled := true;
        ModelsT2DM.Enabled := true;
        ModelsT2DMDate.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
      end;
      '8' : begin
        Models.Enabled := true;
        Models207Pb206Pb.Enabled := true;
        if (OptionsNormal.Checked) then AnalType8 := 'N';
        if (OptionsDiscordance.Checked) then AnalType8 := 'U';
        ToolsConvert.Enabled := true;
        ToolsConvertConcordia2TeraWasserburg.Enabled := true;
        ToolsConvertTeraWasserburg2Concordia.Enabled := false;
        Models206Pb238U.Enabled := true;
        AverageConcordia.Enabled := true;
      end;
      '9' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTDM.Enabled := true;
        ModelsT2DM.Enabled := true;
        ModelsT2DMDate.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
      end;
      'A' : begin
        Models.Enabled := true;
        Models207Pb206Pb.Enabled := true;
        if (OptionsNormal.Checked) then AnalType8 := 'N';
        if (OptionsDiscordance.Checked) then AnalType8 := 'U';
        ToolsConvert.Enabled := true;
        ToolsConvertConcordia2TeraWasserburg.Enabled := false;
        ToolsConvertTeraWasserburg2Concordia.Enabled := true;
        Models206Pb238U.Enabled := true;
        AverageConcordia.Enabled := true;
      end;
      'B' : begin
        Models.Enabled := true;
      end;
      'C' : begin
        Models.Enabled := false;
        AverageArArPlateau.Enabled := false;
        AverageArArPlateau.Enabled := true;
      end;
      'D' : begin
        Models.Enabled := false;
        AverageArArPlateau.Enabled := false;
      end;
      'E' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTDM.Enabled := true;
        ModelsT2DM.Enabled := true;
        ModelsT2DMDate.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
      end;
      'F' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTRD.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
        ModelsTCHUR.Caption := 'T (MA)';
      end;
      'G' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := true;
        ModelsTCHUR.Enabled := true;
        ModelsTDM.Enabled := true;
        ModelsT2DM.Enabled := true;
        ModelsT2DMDate.Enabled := true;
        ModelsRDate.Enabled := true;
        ModelsRIndividual.Enabled := true;
        ModelsEpsilonDate.Enabled := true;
        ModelsEpsilonIndividual.Enabled := true;
      end;
      'H' : begin
        Models.Enabled := true;
        Models207Pb206Pb.Enabled := true;
        AnalType8 := 'E';
      end;
      'I' : begin
        Models.Enabled := false;
        AverageArArPlateau.Enabled := true;
      end;
      'J' : begin
        Models.Enabled := true;
        ModelsTRatio.Enabled := false;
        ModelsTCHUR.Enabled := false;
        ModelsTDM.Enabled := false;
        ModelsT2DM.Enabled := false;
        ModelsT2DMDate.Enabled := false;
        ModelsRDate.Enabled := false;
        ModelsRIndividual.Enabled := false;
        ModelsEpsilonDate.Enabled := false;
        ModelsEpsilonIndividual.Enabled := false;
        ModelsT2DMfromAgeEpsilonvalues.Enabled := true;
      end;
    end;
end;

procedure TGDW1_MainForm.HelpAboutItemClick(Sender: TObject);
begin
  try
    AboutBox := TAboutBox.Create(Self);
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;

procedure TGDW1_MainForm.FileExitItemClick(Sender: TObject);
begin
  //ShowMessage(ChosenStyle);
  SetIniFile;
  Close;
end;

procedure TGDW1_MainForm.OptionsDefaultErrorTypeActualClick(Sender: TObject);
begin
  OptionsDefaultErrorTypePercent.Checked := not OptionsDefaultErrorTypePercent.Checked;
  OptionsDefaultErrorTypeActual.Checked := not OptionsDefaultErrorTypeActual.Checked;
  DefaultErrorType := 'a';
end;

procedure TGDW1_MainForm.OptionsDefaultErrorTypePercentClick(Sender: TObject);
begin
  OptionsDefaultErrorTypePercent.Checked := not OptionsDefaultErrorTypePercent.Checked;
  OptionsDefaultErrorTypeActual.Checked := not OptionsDefaultErrorTypeActual.Checked;
  DefaultErrorType := '%';
end;

procedure TGDW1_MainForm.OptionsDirectoriesClick(Sender: TObject);
begin
  try
    OptDirForm := TfmOptDir.Create(Self);
    OptDirForm.ShowModal;
  finally
    OptDirForm.Free;
  end;
end;

procedure TGDW1_MainForm.FileSaveAsItemClick(Sender: TObject);
begin
  //ShowMessage('2 '+AnalType);
  SaveDialogGEODATE.InitialDir := SPath;
  case AnalType of
    '0' : SaveDialogGEODATE.DefaultExt := '.GE0';
    '1' : SaveDialogGEODATE.DefaultExt := '.GE1';
    '2' : SaveDialogGEODATE.DefaultExt := '.GE2';
    '3' : SaveDialogGEODATE.DefaultExt := '.GE3';
    '4' : SaveDialogGEODATE.DefaultExt := '.GE4';
    '5' : SaveDialogGEODATE.DefaultExt := '.GE5';
    '6' : SaveDialogGEODATE.DefaultExt := '.GE6';
    '7' : SaveDialogGEODATE.DefaultExt := '.GE7';
    '8' : SaveDialogGEODATE.DefaultExt := '.GE8';
    '9' : SaveDialogGEODATE.DefaultExt := '.GE9';
    'A' : SaveDialogGEODATE.DefaultExt := '.GEA';
    'B' : SaveDialogGEODATE.DefaultExt := '.GEB';
    'C' : SaveDialogGEODATE.DefaultExt := '.GEC';
    'D' : SaveDialogGEODATE.DefaultExt := '.GED';
    'E' : SaveDialogGEODATE.DefaultExt := '.GEE';
    'F' : SaveDialogGEODATE.DefaultExt := '.GEF';
    'G' : SaveDialogGEODATE.DefaultExt := '.GEG';
    'H' : SaveDialogGEODATE.DefaultExt := '.GEH';
    'I' : SaveDialogGEODATE.DefaultExt := '.GEI';
    'J' : SaveDialogGEODATE.DefaultExt := '.GEJ';
  end;
  iAnalTyp := Get_Ianal_from_AnalType(AnalType);
  if (iAnalTyp < 0) then Exit;
  SaveDialogGEODATE.FileName := ProjectName;
  SaveDialogGEODATE.FilterIndex := iAnalTyp + 1;
  if SaveDialogGEODATE.Execute then
  begin
    AssignFile(Geodate_file, SaveDialogGEODATE.FileName);
    Rewrite(Geodate_file);
    Write_Geodate_File_Data;
    CloseFile(Geodate_file);
    Drive2 := ExtractFileDir(SaveDialogGEODATE.FileName);
    SPath := ExtractFilePath(SaveDialogGEODATE.FileName);
    OpenDialogGEODATE.FileName := SaveDialogGEODATE.FileName;
    lProjectName.Visible := true;
    ProjectName := ExtractFileName(OpenDialogGEODATE.FileName);
    lProjectName.Caption := 'Project = '+ProjectName;
    lItemsHaveChanged.Visible := false;
    ItemsHaveChanged := false;
  end;
end;

procedure TGDW1_MainForm.FileSaveItemClick(Sender: TObject);
begin
  SaveDialogGEODATE.FileName := OpenDialogGEODATE.FileName;
  AssignFile(Geodate_file, SaveDialogGEODATE.FileName);
  Rewrite(Geodate_file);
  Write_Geodate_File_Data;
  CloseFile(Geodate_file);
  lItemsHaveChanged.Visible := false;
  ItemsHaveChanged := false;
end;

procedure TGDW1_MainForm.RegressUnconstrainedClick(Sender: TObject);
var
  J : integer;
begin
  AdjustForNegativeIntercept := false;
  try
    RegressForm := TfmRegressionResult.Create(Self);
    ProcessOption := 'R';
    {
    AnalType8 := 'N';
    }
    ConstrainFlag := false;
    {
    if ((iAnalTyp in [0..17]) or ((iAnalTyp in [11..13]) and AcceptableUser))
    }
    if (iAnalTyp in [0..17]) then
    begin
      NumberOfPointsRegressed:=0;
      for J:=1 to NumberOfPoints do begin
         if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
      end;
      if (NumberOfPointsRegressed > 1) then
      begin
        RegressForm.ShowModal;
      end else
      begin
        MessageDlg('Insufficient data for regression', mtWarning,[mbOk], 0);
      end;
    end else
      MessageDlg('This option is not available to this user.', mtInformation,
      [mbOk], 0);
  finally
    CheckMuChoice;
    RegressForm.Free;
  end;
end;

procedure TGDW1_MainForm.RegressConstrainedClick(Sender: TObject);
var
  J : integer;
begin
  AdjustForNegativeIntercept := false;
  try
    RegressForm := TfmRegressionResult.Create(Self);
    ProcessOption := 'C';
    AnalType8 := 'N';
    ConstrainFlag := true;
    if iAnalTyp in [0..MaxType] then
    begin
      NumberOfPointsRegressed:=0;
      for J:=1 to NumberOfPoints do begin
         if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
      end;
      if (NumberOfPointsRegressed > 1) then
      begin
        RegressForm.ShowModal
      end else
      begin
        MessageDlg('Insufficient data for regression', mtWarning,[mbOk], 0);
      end;
    end else
      MessageDlg('This option is not available to this user.', mtInformation,
      [mbOk], 0);
  finally
    RegressForm.Free;
  end;
end;

procedure TGDW1_MainForm.EditEditClick(Sender: TObject);
begin
  try
    EditForm := TfmEdit.Create(Self);
    EditForm.ShowModal;
  finally
    if ItemsHaveChanged then
    begin
      lItemsHaveChanged.Visible := true;
      FileSaveItem.Enabled := true;
      FileSaveAsItem.Enabled := true;
    end;
    EditForm.Free
  end;
  try
    MSWDForm := TfmMSWD.Create(Self);
    MSWDForm.ExitBtnClick(Sender);
  finally
    MSWDForm.Free;
  end;
end;

procedure TGDW1_MainForm.OptionsMSWDClick(Sender: TObject);
begin
  try
    MSWDForm := TfmMSWD.Create(Self);
    MSWDForm.ShowModal;
  finally
    MSWDForm.Free;
  end;
end;

procedure TGDW1_MainForm.FileAddItemClick(Sender: TObject);
begin
  FileOpenItemClick(Sender);
end;

procedure TGDW1_MainForm.CheckMuchoice;
begin
  case mu_choice of
    0 : begin
      Singlestage.Checked := true;
      SK2Stage.Checked := false;
      UserDefined.Checked := false;
    end;
    1 : begin
      Singlestage.Checked := false;
      SK2Stage.Checked := true;
      UserDefined.Checked := false;
    end;
    2 : begin
      Singlestage.Checked := false;
      SK2Stage.Checked := false;
      UserDefined.Checked := true;
    end;
  end;
end;

procedure TGDW1_MainForm.SinglestageClick(Sender: TObject);
begin
  Singlestage.Checked := true;
  SK2Stage.Checked := false;
  UserDefined.Checked := false;
  mu_choice := 0;
end;

procedure TGDW1_MainForm.SK2stageClick(Sender: TObject);
begin
  Singlestage.Checked := false;
  SK2Stage.Checked := true;
  UserDefined.Checked := false;
  mu_choice := 1;
end;

procedure TGDW1_MainForm.StyleClick(Sender: TObject);
var
  StyleName : String;
  i : integer;
begin
  //get style name
  StyleName := TMenuItem(Sender).Caption;
  StyleName := StringReplace(StyleName, '&', '',
    [rfReplaceAll,rfIgnoreCase]);
  GlobalChosenStyle := StyleName;
  dmGDWtmp.ChosenStyle := GlobalChosenStyle;
  //set active style
  Application.ProcessMessages;
  TStyleManager.SetStyle(GlobalChosenStyle);
  dmGDWtmp.ChosenStyle := GlobalChosenStyle;
  Application.ProcessMessages;
  //check the currently selected menu item
  (Sender as TMenuItem).Checked := true;
  //uncheck all other style menu items
  for i := 0 to Styles1.Count-1 do
  begin
    if not Styles1.Items[i].Equals(Sender) then
      Styles1.Items[i].Checked := false;
  end;
  for i := 0 to Styles1.Count-1 do
  begin
    if Styles1.Items[i].Checked then GlobalChosenStyle := StringReplace(Styles1.Items[i].Caption, '&', '',
    [rfReplaceAll,rfIgnoreCase]);
  end;
  TStyleManager.SetStyle(GlobalChosenStyle);
  try
    dmGDWtmp.ChosenStyle := GlobalChosenStyle;
  finally
    dmGDWtmp.ChosenStyle := GlobalChosenStyle;
  end;
end;

procedure TGDW1_MainForm.UserdefinedClick(Sender: TObject);
begin
  Singlestage.Checked := false;
  SK2Stage.Checked := false;
  UserDefined.Checked := true;
  mu_choice := 2;
end;

procedure TGDW1_MainForm.OptionsDiscordanceClick(Sender: TObject);
begin
  AnalType8 := 'U';
  OptionsDiscordance.Checked := true;
  OptionsNormal.Checked := false;
end;

procedure TGDW1_MainForm.OptionsNormalClick(Sender: TObject);
begin
  AnalType8 := 'N';
  OptionsNormal.Checked := true;
  OptionsDiscordance.Checked := false;
end;

procedure TGDW1_MainForm.Models207Pb206PbClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 6;
    WtAvForm.AllSame := true;
    //if (AnalType in ['8','A'])
    if (CharInSet(AnalType,['8','A']))
      then AllSame := false
      else AllSame := true;
    AnalType8 := 'N';
    if OptionsNormal.Checked then AnalType8 := 'N';
    if OptionsDiscordance.Checked then AnalType8 := 'U';
    if (AnalType = 'A') then AnalType8 := 'N';
    if (AnalType = 'H') then AnalType8 := 'E';
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
    if OptionsNormal.Checked then AnalType8 := 'N';
    if OptionsDiscordance.Checked then AnalType8 := 'U';
  end;
end;

procedure TGDW1_MainForm.AveragesXClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 1;
    WtAvForm.AllSame := true;
    AnalType8 := 'N';
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.AveragesYClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 2;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.AveragesZClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 3;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsTRatioClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 6;
    WtAvForm.AllSame := true;
    GetMdlAtForm := TfmGetMdlAt.Create(Self);
    GetMdlAtForm.bDate := false;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      GetMdlAtForm.ShowModal;
      if (GetMdlAtForm.ModalResult = mrOK) then
        WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    GetMdlAtForm.Free;
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsT2DMClick(Sender: TObject);
var
  J : integer;
begin
  if ((DM[iAnalTyp,1] = 0.0) and (CC[iAnalTyp,1] = 0.0)) then
  begin
    try
      WtAvForm := TfmWtAv.Create(Self);
      WtAvForm.VarbNox := 10;
      WtAvForm.AllSame := false;
      NumberOfPointsRegressed:=0;
      for J:=1 to NumberOfPoints do begin
         if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
      end;
      if (NumberOfPointsRegressed > 0) then
      begin
        WtAvForm.ShowModal;
      end else
      begin
        MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
      end;
    finally
      WtAvForm.Free;
    end;
  end else
  begin
    MessageDlg('Linear model required. Modify the GeoDate.INI file',mtWarning,[mbOK],0);
  end;
end;

procedure TGDW1_MainForm.ModelsTCHURClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 7;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsTDMClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 8;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsRDateClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 4;
    WtAvForm.AllSame := true;
    GetMdlAtForm := TfmGetMdlAt.Create(Self);
    GetMdlAtForm.bDate := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      GetMdlAtForm.ShowModal;
      if (GetMdlAtForm.ModalResult = mrOK) then
        WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    GetMdlAtForm.Free;
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsRIndividualClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 4;
    WtAvForm.AllSame := false;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.Models238U204PbClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 4;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (Sender = Models238U204Pb) then
    begin
      WtAvForm.AllSame := false;
      if (NumberOfPointsRegressed > 0) then
      begin
        WtAvForm.ShowModal;
      end else
      begin
        MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
      end;
    end;
    if (Sender = Models238U204PbDateSame) then
    begin
      try
        WtAvForm.AllSame := true;
        GetMdlAtForm := TfmGetMdlAt.Create(Self);
        GetMdlAtForm.bDate := true;
        if (NumberOfPointsRegressed > 0) then
        begin
          GetMdlAtForm.ShowModal;
          if (GetMdlAtForm.ModalResult = mrOK) then
            WtAvForm.ShowModal;
        end else
        begin
          MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
        end;
      finally
        GetMdlAtForm.Free;
      end;
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsEpsilonDateClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 5;
    GetMdlAtForm := TfmGetMdlAt.Create(Self);
    GetMdlAtForm.bDate := true;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      GetMdlAtForm.ShowModal;
      if (GetMdlAtForm.ModalResult = mrOK) then
        WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
    GetMdlAtForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsEpsilonIndividualClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 5;
    WtAvForm.AllSame := false;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.FormActivate(Sender: TObject);
begin
  if Startup then
  begin
    Startup := False;
  end;
end;

procedure TGDW1_MainForm.FileExportSpreadsheetClick(Sender: TObject);
//var
  //i    : integer;
begin
  {
  try
    SprdSheetForm := TfmSheet.Create(Self);
    SprdSheetForm.SprdSheet.Row := 1;
    SprdSheetForm.SprdSheet.Col := 1;
    SprdSheetForm.SprdSheet.Text := 'Sample';
    SprdSheetForm.SprdSheet.Col := 2;
    SprdSheetForm.SprdSheet.Text := Element[iAnalTyp,1];
    SprdSheetForm.SprdSheet.Col := 3;
    SprdSheetForm.SprdSheet.Text := Element[iAnalTyp,2];
    SprdSheetForm.SprdSheet.Col := 4;
    SprdSheetForm.SprdSheet.Text := XRatioStr[iAnalTyp];
    SprdSheetForm.SprdSheet.Col := 5;
    SprdSheetForm.SprdSheet.Text := 'Precision';
    SprdSheetForm.SprdSheet.Col := 6;
    SprdSheetForm.SprdSheet.Text := '1 sigma';
    SprdSheetForm.SprdSheet.Col := 8;
    SprdSheetForm.SprdSheet.Text := YRatioStr[iAnalTyp];
    SprdSheetForm.SprdSheet.Col := 9;
    SprdSheetForm.SprdSheet.Text := 'Precision';
    SprdSheetForm.SprdSheet.Col := 10;
    SprdSheetForm.SprdSheet.Text := '1 sigma';
    SprdSheetForm.SprdSheet.Col := 12;
    SprdSheetForm.SprdSheet.Text := 'R';
    SprdSheetForm.SprdSheet.Col := 13;
    SprdSheetForm.SprdSheet.Text := ZRatioStr[iAnalTyp];
    SprdSheetForm.SprdSheet.Col := 14;
    SprdSheetForm.SprdSheet.Text := 'Precision';
    SprdSheetForm.SprdSheet.Col := 15;
    SprdSheetForm.SprdSheet.Text := 'RFlg';
    SprdSheetForm.SprdSheet.Col := 16;
    SprdSheetForm.SprdSheet.Text := 'PFlg';
    SprdSheetForm.SprdSheet.Col := 17;
    SprdSheetForm.SprdSheet.Text := 'Latitude';
    SprdSheetForm.SprdSheet.Col := 18;
    SprdSheetForm.SprdSheet.Text := 'Longitude';
    for i := 1 to NumberOfPoints do
    begin
      SprdSheetForm.SprdSheet.Row := i+1;
      SprdSheetForm.SprdSheet.Col := 1;
      SprdSheetForm.SprdSheet.Text := SmpNo[i];
      SprdSheetForm.SprdSheet.Col := 2;
      SprdSheetForm.SprdSheet.Number := Conc[i,1];
      SprdSheetForm.SprdSheet.Col := 3;
      SprdSheetForm.SprdSheet.Number := Conc[i,2];
      SprdSheetForm.SprdSheet.Col := 4;
      SprdSheetForm.SprdSheet.Number := Ratio[i,1];
      SprdSheetForm.SprdSheet.Col := 5;
      SprdSheetForm.SprdSheet.Number := XPrec[i];
      SprdSheetForm.SprdSheet.Col := 6;
      SprdSheetForm.SprdSheet.Number := ErrorWt[i,1];
      SprdSheetForm.SprdSheet.Col := 7;
      if (ErrTyp[i] in ['1','2']) then
        SprdSheetForm.SprdSheet.Text := '%'
      else
        SprdSheetForm.SprdSheet.Text := 'a';
      SprdSheetForm.SprdSheet.Col := 8;
      SprdSheetForm.SprdSheet.Number := Ratio[i,2];
      SprdSheetForm.SprdSheet.Col := 9;
      SprdSheetForm.SprdSheet.Number := YPrec[i];
      SprdSheetForm.SprdSheet.Col := 10;
      SprdSheetForm.SprdSheet.Number := ErrorWt[i,2];
      SprdSheetForm.SprdSheet.Col := 11;
      if (ErrTyp[i] in ['1','3']) then
        SprdSheetForm.SprdSheet.Text := '%'
      else
        SprdSheetForm.SprdSheet.Text := 'a';
      SprdSheetForm.SprdSheet.Col := 12;
      SprdSheetForm.SprdSheet.Number := R[i];
      SprdSheetForm.SprdSheet.Col := 13;
      SprdSheetForm.SprdSheet.Number := Ratio[i,3];
      SprdSheetForm.SprdSheet.Col := 14;
      SprdSheetForm.SprdSheet.Number := ZPrec[i];
      SprdSheetForm.SprdSheet.Col := 15;
      SprdSheetForm.SprdSheet.Text := RFlg[i];
      SprdSheetForm.SprdSheet.Col := 16;
      SprdSheetForm.SprdSheet.Text := PFlg[i];
      SprdSheetForm.SprdSheet.Col := 17;
      SprdSheetForm.SprdSheet.Number := Latitude[i];
      SprdSheetForm.SprdSheet.Col := 18;
      SprdSheetForm.SprdSheet.Number := Longitude[i];
    end;
    SprdSheetForm.SprdSheet.Row := 1;
    SprdSheetForm.SprdSheet.Col := 1;
    SprdSheetForm.ShowModal;
  finally
    SprdSheetForm.Free;
  end;
  }
end;

procedure TGDW1_MainForm.FileImportSpreadsheetClick(Sender: TObject);
var
  ModalResultLocal : boolean;
begin
  try
    ModalResultLocal := false;
    SprdSheetImportForm := TfmSheetImport.Create(Self);
    SprdSheetImportForm.ShowModal;
    if (SprdSheetImportForm.bbImport.ModalResult = mrOK) then
    begin
      ModalResultLocal := true;
      EditEdit.Enabled := true;
      bEdit.Enabled := true;
      Edit1.Enabled := true;
      try
        EditForm := TfmEdit.Create(Self);
        EditForm.ShowModal;
        //ShowMessage('1'+ProjectName);
        if (EditForm.ModalResult = mrOK) then
        begin
          //ShowMessage('2'+ProjectName);
          SaveDialogGEODATE.Filename := ProjectName;
          FileSaveAsItemClick(Sender);
          GDWSetEnabled;
        end;
      finally
        EditForm.Free;
      end;
    end else
    begin
      ModalResultLocal := false;
    end;
  finally
    SprdSheetImportForm.Free;
  end;
  if (ModalResultLocal) then
  begin
    try
      MSWDForm := TfmMSWD.Create(Self);
      MSWDForm.ExitBtnClick(Sender);
    finally
      MSWDForm.Free;
    end;
  end;
end;

procedure TGDW1_MainForm.Test1Click(Sender: TObject);
var
  CurrentStyle : string;
begin
  CurrentStyle := GlobalChosenStyle;
  ShowMessage(CurrentStyle);
  ShowMessage(dmGDWtmp.ChosenStyle);
end;

procedure TGDW1_MainForm.ToolsConvertConcordia2TeraWasserburgClick(
  Sender: TObject);
begin
  ConvertConcordia2TeraWasserburg;
  if (Sender = ToolsConvertConcordia2TeraWasserburg) then
  begin
    SaveDialogGEODATE.FileName := ProjectName;
    FileSaveAsItemClick(Sender);
  end;
  GDWSetEnabled;
end;

procedure TGDW1_MainForm.Registeruser1Click(Sender: TObject);
begin
  try
    RegisterUserForm := TfmRegUser3.Create(Self);
    RegisterUserForm.SoftwareName := ProgramName;
    if (RegisterUserForm.ShowModal = mrOK) then
    begin
      RegisteredUser := RegisterUserForm.RegisteredUser;
      lRegisteredUser.Caption := 'Registered to: '+RegisteredUser;
      CheckAcceptableUser;
    end;
  finally
    RegisterUserForm.Free;
  end;
end;

procedure TGDW1_MainForm.HelpBugReportClick(Sender: TObject);
begin
  {
  try
    BugReportForm := TfmBugReport.Create(Self);
    BugReportForm.SoftwareName := ProgramName;
    BugReportForm.RegisteredUser := RegisteredUser;
    BugReportForm.ShowModal;
  finally
    BugReportForm.Free;
  end;
  }
end;

procedure TGDW1_MainForm.FileOpenLegacyClick(Sender: TObject);
var
  ICode : integer;
  PosDot : integer;
begin
  OpenDialogLegacyGEODATE.InitialDir := Drive2;
  AnalType := UpCase(AnalType);
  {
  if CharInSet(AnalType,['0','1','2','3','4','5','6','7','8','9'])
  //if (AnalType in ['0','1','2','3','4','5','6','7','8','9'])
    then Val(AnalType, iAnalTyp, ICode)
    else begin
      if (AnalType = 'A') then iAnalTyp := 10;
      if (AnalType = 'B') then iAnalTyp := 11;
      if (AnalType = 'C') then iAnalTyp := 12;
      if (AnalType = 'D') then iAnalTyp := 13;
      if (AnalType = 'E') then iAnalTyp := 14;
      if (AnalType = 'F') then iAnalTyp := 15;
      if (AnalType = 'G') then iAnalTyp := 16;
      if (AnalType = 'H') then iAnalTyp := 17;
      if (AnalType = 'I') then iAnalTyp := 18;
      if (AnalType = 'J') then iAnalTyp := 19;
      if CharInSet(AnalType,['A','B','C','D','E','F','G','H','I','J'])
      //if (AnalType in ['A','B','C','D','E','F','G','H','I'])
        then ICode := 0
        else ICode := -1;
    end;
  if (ICode <> 0) then Exit;
  }
  iAnalTyp := Get_Ianal_from_AnalType(AnalType);
  if (iAnalTyp < 0) then Exit;
  OpenDialogLegacyGEODATE.FilterIndex := iAnalTyp + 1;
  if OpenDialogLegacyGEODATE.Execute then
  begin
    Drive2 := ExtractFileDir(OpenDialogLegacyGEODATE.FileName);
    SPath := ExtractFilePath(OpenDialogLegacyGEODATE.FileName);
    AssignFile(York_file, OpenDialogLegacyGEODATE.FileName);
    AnalType := OpenDialogLegacyGEODATE.FileName[Length(OpenDialogLegacyGEODATE.FileName)];
    AnalType := UpCase(AnalType);
    iAnalTyp := Get_Ianal_from_AnalType(AnalType);
    if (iAnalTyp < 0) then Exit;
    {
    if CharInSet(AnalType,['0','1','2','3','4','5','6','7','8','9'])
    //if (AnalType in ['0','1','2','3','4','5','6','7','8','9'])
      then Val(AnalType, iAnalTyp, ICode)
      else begin
        if (AnalType = 'A') then iAnalTyp := 10;
        if (AnalType = 'B') then iAnalTyp := 11;
        if (AnalType = 'C') then iAnalTyp := 12;
        if (AnalType = 'D') then iAnalTyp := 13;
        if (AnalType = 'E') then iAnalTyp := 14;
        if (AnalType = 'F') then iAnalTyp := 15;
        if (AnalType = 'G') then iAnalTyp := 16;
        if (AnalType = 'H') then iAnalTyp := 17;
        if (AnalType = 'I') then iAnalTyp := 18;
        if (AnalType = 'J') then iAnalTyp := 19;
        if CharInSet(AnalType,['A','B','C','D','E','F','G','H','I','J'])
        //if (AnalType in ['A','B','C','D','E','F','G','H','I'])
          then ICode := 0
          else ICode := -1;
      end;
    if (ICode <> 0) then Exit;
    }
    if ((Sender = FileOpenItem) or (Sender = bOpen) or (Sender = FileOpenLegacy)) then
    begin
      NumberOfPoints := 0;
      ProcessOption := 'N';
      Age := 0.0;
      Intercept := 0.0;
      InitRatio := 0.0;
      lProjectName.Visible := true;
      ProjectName := ExtractFileName(OpenDialogLegacyGEODATE.FileName);
      //ShowMessage(ProjectName);
      PosDot := Pos('.',ProjectName);
      ProjectName := Copy(ProjectName,1,PosDot-1);
      //ShowMessage(ProjectName);
      //ProjectName[0] := Char(Length(ProjectName)-4);
      lProjectName.Caption := 'Project = '+ProjectName;
      lItemsHaveChanged.Visible := false;
      ItemsHaveChanged := false;
    end;
    if (Sender = FileAddItem) then
    begin
      if AllowMessages then
        MessageDlg(OpenDialogLegacyGEODATE.FileName+' added', mtInformation,[mbOk], 0);
      ProcessOption := 'F';
    end;
    Reset(York_file);
    Get_File_Data;
    CloseFile(York_file);
    {
    SPath := OpenDialogGEODATE.FileName;
    }
    try
      dmMSWD.cdsF.FileName := cdsPath+'TableF.cds';
      dmMSWD.cdsF.LoadFromFile(dmMSWD.cdsF.FileName);
    except
      MessageDlg('Unable to open MSWD definition file',mtWarning,[mbOK],0);
    end;
    try
      MSWDForm := TfmMSWD.Create(Self);
      MSWDForm.ExitBtnClick(Sender);
    finally
      MSWDForm.Free;
    end;
    GDWSetEnabled;
    FileAddItem.Enabled := true;
    bSaveGDWFile.Enabled := true;
    FileSaveItem.Enabled := true;
    FileSaveAsItem.Enabled := true;
    Edit1.Enabled := true;
    Regress1.Enabled := true;
    Models.Enabled := true;
    Averages.Enabled := true;
  end;
end;

procedure TGDW1_MainForm.ToolsConvertTeraWasserburg2ConcordiaClick(
  Sender: TObject);
begin
  ConvertTeraWasserburg2Concordia;
  if (Sender = ToolsConvertTeraWasserburg2Concordia) then
  begin
    SaveDialogGEODATE.FileName := ProjectName;
    FileSaveAsItemClick(Sender);
  end;
  GDWSetEnabled;
end;

procedure TGDW1_MainForm.ResultsTable1Click(Sender: TObject);
begin
  ShowMessage('Not yet implemented');
end;

procedure TGDW1_MainForm.DatabaseTables1Click(Sender: TObject);
begin
  {
  try
    GenSheetForm := TfmGenSheet.Create(Self);
    GenSheetForm.FileOpenPath := TTPath;
    GenSheetForm.ShowModal;
    TTPath := GenSheetForm.FileSavePath;
  finally
    GenSheetForm.Free;
  end;
  }
end;

procedure TGDW1_MainForm.FormShow(Sender: TObject);
//var
  //i : integer;
begin
  XConstrain := 0.0;
  YConstrain := 0.0;
  AgeConstrain := 0.0;
  AdjustForNegativeIntercept := false;
  OptionsNormal.Checked := false;
  OptionsDiscordance.Checked := false;
  EllipseMagnif := 1.96;
  AnalType8 := 'N';
  N_Rep := 999;
  FAlpha := 0.050;
  NumberOfPoints := 0;
  GetIniFile;
  iAnalTyp := Get_Ianal_from_AnalType(AnalType);
  if (RegisteredUser = 'not defined') then
  begin
    MessageDlg('Please register so as to receive notice of updates',mtInformation,[mbOK],0);
  end;
  {
  CheckAcceptableUser;
  Session.NetFileDir := GdwNetFileDir;
  try
    with dmDVResults do
    begin
      DB1.Connected := false;
      DB1.DriverName := '';
      DB1.Params[0] := '';
      DB1.Params[1] := '';
      DB1.Params[2] := '';
      DB1.AliasName := 'GdwResults';
    end;
  except
  end;
  }
  {
  if AllowMessages then ShowMessage('NetFileDir = '+fmOptDir.GdwNetFileDir);
  if AllowMessages then ShowMessage('Drive1 = '+Drive1);
  if AllowMessages then ShowMessage('Drive2 = '+Drive2);
  if AllowMessages then ShowMessage('Drive3 = '+Drive3);
  }
  SPath := Drive2;
  TTPath := Drive3;
  lRegisteredUser.Caption := 'Registered to: '+RegisteredUser;
  FileExportSpreadsheet.Visible := false;
  FileExportSpreadsheet.Enabled := false;
  EditEdit.Enabled := false;
  bEdit.Enabled := false;
  Edit1.Enabled := false;
  Regress1.Enabled := false;
  Models.Enabled := false;
  bRegressConstrained.Enabled := false;
  bRegressUnconstrained.Enabled := false;
  Averages.Enabled := false;
  OpenDialogGEODATE.InitialDir := Drive2;
  SaveDialogGEODATE.InitialDir := Drive2;
  lProjectName.Visible := false;
  ToolsConvert.Enabled := false;
  ToolsConvertConcordia2TeraWasserburg.Enabled := false;
  ToolsConvertTeraWasserburg2Concordia.Enabled := false;
  PlotErrorEnvelope := 'N';
  ChooseEllipse := 'N';
  lItemsHaveChanged.Visible := false;
  ItemsHaveChanged := false;
  FileAddItem.Enabled := true;
  bSaveGDWFile.Enabled := false;
  FileSaveItem.Enabled := false;
  FileSaveAsItem.Enabled := false;
end;

procedure TGDW1_MainForm.AverageConcordiaClick(Sender: TObject);
var
  //i,
  J : integer;
begin
  try
    ConcordiaDateForm := TfmConcordiaDate.Create(Self);
    ConcordiaDateForm.VarbNox := 9;
    ConcordiaDateForm.AllSame := true;
    AnalType8 := 'N';
    TemporaryAnalType := '';
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      if (AnalType = 'A') then
      begin
        TemporaryAnalType := AnalType;
        ToolsConvertTeraWasserburg2ConcordiaClick(Sender);
      end;
      ConcordiaDateForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    ConcordiaDateForm.Close;
    ConcordiaDateForm.Free;
    if OptionsNormal.Checked then AnalType8 := 'N';
    if OptionsDiscordance.Checked then AnalType8 := 'U';
  end;
end;

procedure TGDW1_MainForm.OptionsAlphaLevelClick(Sender: TObject);
var
  iCode : integer;
  FAlphaStr : string;
begin
  iCode := 1;
  repeat
    FAlphaStr := FormatFloat('0.000',FAlpha);
    InputBox('Define Alpha','Define the alpha level for statistical assessments',FAlphaStr);
    try
      FAlpha := StrToFloat(FAlphaStr);
      iCode := 0;
    except
      MessageDlg('Incorrect value for alpha',mtWarning,[mbOK],0);
      FAlpha := 0.050;
      iCode := 1;
    end;
    {
    Val(FAlphaStr,FAlpha,ICode);
    }
    if (FAlpha < 0.01) then FAlpha := 0.050;
  until (iCode = 0);
end;

procedure TGDW1_MainForm.AverageArArPlateauClick(Sender: TObject);
var
  J : integer;
begin
  try
    PlatArForm := TfmPlatAr.Create(Self);
    PlatArForm.VarbNox := 6;
    PlatArForm.AllSame := false;
    ArArJ := Ratio[1,3];
    ArArJ1sig := Ratio[2,3];
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      PlatArForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    PlatArForm.Free;
  end;
end;

procedure TGDW1_MainForm.Models206Pb238UClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 4;
    WtAvForm.AllSame := true;
    {
    if OptionsNormal.Checked then AnalType8 := 'N';
    if OptionsDiscordance.Checked then AnalType8 := 'U';
    if (AnalType = 'A') then AnalType8 := 'N';
    if (AnalType = 'H') then AnalType8 := 'E';
    }
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
    if OptionsNormal.Checked then AnalType8 := 'N';
    if OptionsDiscordance.Checked then AnalType8 := 'U';
  end;
end;

procedure TGDW1_MainForm.OptionsEllipseMagnification1Click(
  Sender: TObject);
begin
  OptionsEllipseMagnification1.Checked := true;
  OptionsEllipseMagnification2.Checked := false;
  EllipseMagnif := 1.0;
end;

procedure TGDW1_MainForm.OptionsEllipseMagnification2Click(
  Sender: TObject);
begin
  OptionsEllipseMagnification1.Checked := false;
  OptionsEllipseMagnification2.Checked := true;
  EllipseMagnif := 1.96;
end;

{
procedure TGDW1_MainForm.Timer1Timer(Sender: TObject);
begin
  SplashScreen.Close;
  SplashScreen.Free;
end;
}

procedure TGDW1_MainForm.ModelsRPostFormationClick(Sender: TObject);
var
  J : integer;
  PostFmForm : TfmModelPbPostFm;
begin
  try
    PostFmForm := TfmModelPbPostFm.Create(Self);
    PostFmForm.ShowModal;
  finally
    PostFmForm.Free;
  end;
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 11;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsTRDClick(Sender: TObject);
var
  J : integer;
begin
  try
    WtAvForm := TfmWtAv.Create(Self);
    WtAvForm.VarbNox := 10;
    WtAvForm.AllSame := true;
    NumberOfPointsRegressed:=0;
    for J:=1 to NumberOfPoints do begin
       if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
    end;
    if (NumberOfPointsRegressed > 0) then
    begin
      WtAvForm.ShowModal;
    end else
    begin
      MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
    end;
  finally
    WtAvForm.Free;
  end;
end;

procedure TGDW1_MainForm.ModelsT2DMDateClick(Sender: TObject);
var
  J : integer;
begin
  if ((DM[iAnalTyp,1] = 0.0) and (CC[iAnalTyp,1] = 0.0)) then
  begin
    try
      WtAvForm := TfmWtAv.Create(Self);
      WtAvForm.VarbNox := 10;
      GetMdlAtForm := TfmGetMdlAt.Create(Self);
      GetMdlAtForm.bDate := true;
      WtAvForm.AllSame := true;
      NumberOfPointsRegressed:=0;
      for J:=1 to NumberOfPoints do begin
         if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
      end;
      if (NumberOfPointsRegressed > 0) then
      begin
        GetMdlAtForm.ShowModal;
        if (GetMdlAtForm.ModalResult = mrOK) then
          WtAvForm.ShowModal;
      end else
      begin
        MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
      end;
    finally
      WtAvForm.Free;
      GetMdlAtForm.Free;
    end;
  end else
  begin
    MessageDlg('Linear models required. Modify the GeoDate.INI file',mtWarning,[mbOK],0);
  end;
end;

procedure TGDW1_MainForm.ModelsT2DMfromAgeEpsilonValuesClick(Sender: TObject);
var
  J : integer;
begin
  try
    IsotopeSystemForm := TfmIsotopeSystemType.Create(Self);
    ProcessOption := 'J';
    if iAnalTyp in [0..MaxType] then
    begin
      NumberOfPointsRegressed:=0;
      for J:=1 to NumberOfPoints do begin
         if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
      end;
      if (NumberOfPointsRegressed > 1) then
      begin
        IsotopeSystemForm.ShowModal;
      end else
      begin
        MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
      end;
    end else
      MessageDlg('This option is not available to this user.', mtInformation,
      [mbOk], 0);
  finally
    IsotopeSystemForm.Free;
  end;
  if ((DM[iAnalTyp,1] = 0.0) and (CC[iAnalTyp,1] = 0.0)) then
  begin
    try
      WtAvForm := TfmWtAv.Create(Self);
      WtAvForm.VarbNox := 13;
      WtAvForm.AllSame := false;
      NumberOfPointsRegressed:=0;
      for J:=1 to NumberOfPoints do begin
         if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
      end;
      if (NumberOfPointsRegressed > 0) then
      begin
        WtAvForm.ShowModal;
      end else
      begin
        MessageDlg('Insufficient data for calculation', mtWarning,[mbOk], 0);
      end;
    finally
      WtAvForm.Free;
    end;
  end else
  begin
    MessageDlg('Linear model required. Modify the GeoDate.INI file',mtWarning,[mbOK],0);
  end;
end;

end.
