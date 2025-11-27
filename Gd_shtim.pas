unit Gd_shtim;

interface

uses
  SysUtils, System.IOUtils,
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Buttons, OleCtrls, ExtCtrls, StdCtrls, Mask, IniFiles,
  Tabs, Grids,ToolWin,
  AxCtrls,
  VCL.FlexCel.Core, FlexCel.XlsAdapter, FlexCel.Render, FlexCel.Preview,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, ImageCollection_dm;

type
  TfmSheetImport = class(TForm)
    Panel1: TPanel;
    sbSheet: TStatusBar;
    SaveDialogSprdSheet: TSaveDialog;
    bbOpenSheet: TBitBtn;
    OpenDialogSprdSheet: TOpenDialog;
    gbDefineFields: TGroupBox;
    Label1: TLabel;
    lSample: TLabel;
    lXXStr: TLabel;
    lYYStr: TLabel;
    lXStr: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lYStr: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lZStr: TLabel;
    Label14: TLabel;
    lR: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    gbDefineRows: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    bbImport: TBitBtn;
    gbIsoSys: TGroupBox;
    cbxIsoSys: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    meFromRow: TEdit;
    meToRow: TEdit;
    lXaxisStr: TLabel;
    lvs: TLabel;
    lYAxisStr: TLabel;
    eSampleNo: TEdit;
    eXStr: TEdit;
    eZStr: TEdit;
    eYStr: TEdit;
    eRStr: TEdit;
    eXXStr: TEdit;
    eXPrecStr: TEdit;
    eYPrecStr: TEdit;
    eZPrecStr: TEdit;
    eYYStr: TEdit;
    eXErrStr: TEdit;
    eYErrStr: TEdit;
    eRFlagStr: TEdit;
    eXErrTypeStr: TEdit;
    eYErrTypeStr: TEdit;
    ePFlagStr: TEdit;
    bbCancel: TBitBtn;
    gbPrefix: TGroupBox;
    eSmpPrefix: TEdit;
    Label8: TLabel;
    Label13: TLabel;
    eLatitudeStr: TEdit;
    Label15: TLabel;
    eLongitudeStr: TEdit;
    Panel2: TPanel;
    Splitter1: TSplitter;
    TabControl: TTabControl;
    gbDefineTabSheet: TGroupBox;
    cbSheetName: TComboBox;
    Memo1: TMemo;
    Tabs: TTabSet;
    SheetData: TStringGrid;
    VirtualImageList1: TVirtualImageList;
    Label16: TLabel;
    eZErrStr: TEdit;
    Label20: TLabel;
    eZErrTypeStr: TEdit;
    lAgeStr: TLabel;
    eAgeStr: TEdit;
    Label23: TLabel;
    eAgeErrStr: TEdit;
    eWStr: TEdit;
    eWErrTypeStr: TEdit;
    Label25: TLabel;
    eWErrStr: TEdit;
    Label26: TLabel;
    eWPrecStr: TEdit;
    Label27: TLabel;
    lWStr: TLabel;
    lZZStr: TLabel;
    eZZStr: TEdit;
    procedure bbOpenSheetClick(Sender: TObject);
    procedure bbImportClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxIsoSysChange(Sender: TObject);
    procedure eSampleNoExit(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure cbSheetNameChange(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabsClick(Sender: TObject);
    procedure SheetDataSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Xls : TXlsFile;
    procedure GetIniFile;
    procedure UpdateIniFile;
    function ConvertCol2Int(AnyString : string) : integer;
    procedure FillTabs;
    procedure ClearGrid;
    procedure FillGrid(const Formatted: boolean);
    function GetStringFromCell(iRow,iCol : integer) : string;
  public
    { Public declarations }
  end;

var
  fmSheetImport: TfmSheetImport;

implementation

{$R *.DFM}

uses
  allsorts, gdw_varb, gd_drv, dmGdtmpDB;

var
  iRec, iRecCount      : integer;

procedure TfmSheetImport.bbOpenSheetClick(Sender: TObject);
var
  tmpStr    : string;
  i : integer;
  PosDot : integer;
begin
  cbSheetname.Items.Clear;
  gbDefineTabSheet.Visible := false;
  gbIsoSys.Visible := false;
  OpenDialogSprdSheet.InitialDir := TTPath;
  ProjectName := ExtractFileName(OpenDialogSprdSheet.FileName);
  PosDot := Pos('.',ProjectName);
  ProjectName := Copy(ProjectName,1,PosDot-1);
  if OpenDialogSprdSheet.Execute then
  begin
    Drive3 := SysUtils.ExtractFileDir(OpenDialogSprdSheet.FileName);
    TTPath := SysUtils.ExtractFilePath(OpenDialogSprdSheet.FileName);
    //Open the Excel file.
    if Xls = nil then Xls := TXlsFile.Create(false);
    xls.Open(OpenDialogSprdSheet.FileName);
    FillTabs;
    Tabs.TabIndex := Xls.ActiveSheet - 1;
    cbSheetName.ItemIndex := Xls.ActiveSheet - 1;
    ProjectName := SysUtils.ExtractFileName(OpenDialogSprdSheet.FileName);
    PosDot := Pos('.',ProjectName);
    ProjectName := Copy(ProjectName,1,PosDot-1);
    FillGrid(true);
    gbIsoSys.Visible := true;
    gbDefineTabSheet.Visible := true;
    cbxIsoSysChange(Sender);
  end;
end;

procedure TfmSheetImport.FillTabs;
var
  s: integer;
begin
  Tabs.Tabs.Clear;
  cbSheetname.Items.Clear;
  for s := 1 to Xls.SheetCount do
  begin
    Tabs.Tabs.Add(Xls.GetSheetName(s));
    cbSheetname.Items.Add(Xls.GetSheetName(s));
  end;
end;

procedure TfmSheetImport.ClearGrid;
var
  r: integer;
begin
  for r := 1 to SheetData.RowCount do SheetData.Rows[r].Clear;
end;

procedure TfmSheetImport.FillGrid(const Formatted: boolean);
var
  r, c, cIndex: Integer;
  v: TCellValue;
begin
  if Xls = nil then exit;

  if (Tabs.TabIndex + 1 <= Xls.SheetCount) and (Tabs.TabIndex >= 0) then Xls.ActiveSheet := Tabs.TabIndex + 1 else Xls.ActiveSheet := 1;
  //Clear data in previous grid
  ClearGrid;
  SheetData.RowCount := 1;
  SheetData.ColCount := 1;

  SheetData.RowCount := Xls.RowCount + 1; //Include fixed row
  SheetData.ColCount := Xls.ColCount + 1; //Include fixed col. NOTE THAT COLCOUNT IS SLOW. We use it here because we really need it. See the Performance.pdf doc.

  if (SheetData.ColCount > 1) then SheetData.FixedCols := 1; //it is deleted when we set the width to 1.
  if (SheetData.RowCount > 1) then SheetData.FixedRows := 1;

  for r := 1 to Xls.RowCount do
  begin
    //Instead of looping in all the columns, we will just loop in the ones that have data. This is much faster.
    for cIndex := 1 to Xls.ColCountInRow(r) do
    begin
      c := Xls.ColFromIndex(r, cIndex); //The real column.
      if Formatted then
      begin
        SheetData.Cells[c, r] := Xls.GetStringFromCell(r, c);
      end
      else
      begin
        v := Xls.GetCellValue(r, c);
        SheetData.Cells[c, r] := v.ToString;
      end;
    end;
  end;

  //Fill the row headers
  for r := 1 to SheetData.RowCount - 1 do
  begin
    SheetData.Cells[0, r] := IntToStr(r);
    SheetData.RowHeights[r] := Round(Xls.GetRowHeight(r) / TExcelMetrics.RowMultDisplay(Xls));
  end;

  //Fill the column headers
  for c := 1 to SheetData.ColCount - 1 do
  begin
    SheetData.Cells[c, 0] := TCellAddress.EncodeColumn(c);
    SheetData.ColWidths[c] := Round(Xls.GetColWidth(c) / TExcelMetrics.ColMult(Xls));
  end;
end;


procedure TfmSheetImport.GetIniFile;
var
  AppIni   : TIniFile;
  PublicPath : string;
  IniFileName : string;
begin
  PublicPath := TPath.GetHomePath;
  CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  IniFilename := CommonFilePath + 'Geodate.ini';
  AppIni := TIniFile.Create(IniFilename);
  try
    eSampleNo.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Sample','A');
    eXXStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],Element[iAnalTyp,1],'B');
    eYYStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],Element[iAnalTyp,2],'C');
    eZZStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],Element[iAnalTyp,3],'0');
    eXStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],XRatioStr[iAnalTyp],'D');
    eXPrecStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'X Precision','E');
    eXErrStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'X Uncertainty','F');
    eXErrTypeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'X Uncertainty type','G');
    eYStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],YRatioStr[iAnalTyp],'H');
    eYPrecStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Y Precision','I');
    eYErrStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Y Uncertainty','J');
    eYErrTypeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Y Uncertainty type','K');
    eZStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],ZRatioStr[iAnalTyp],'M');
    eZPrecStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Z Precision','0');
    eZErrStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Z Uncertainty','0');
    eZErrTypeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Z Uncertainty type','0');
    eRStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Error Correlation','L');
    eRFlagStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'R Flag','O');
    ePFlagStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'P Flag','P');
    eAgeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Age','M');
    eAgeErrStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Age Uncertainty','J');
    eWStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],WRatioStr[iAnalTyp],'M');
    eWPrecStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'W Precision','N');
    eWErrStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'W Uncertainty','J');
    eWErrTypeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'W Uncertainty type','K');
    eLatitudeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Latitude','Q');
    eLongitudeStr.Text := AppIni.ReadString('Spreadsheet columns for '+Process[iAnalTyp],'Longitude','R');
  finally
    AppIni.Free;
  end;
end;

procedure TfmSheetImport.TabControlChange(Sender: TObject);
begin
  {
  Data.ApplySheet;
  FlexCelImport1.ActiveSheet:= TabControl.TabIndex+1;
  }
  cbSheetname.ItemIndex := TabControl.TabIndex;
  {
  Data.Zoom := 70;
  Data.LoadSheet;
  }
  //sbFindLastRowClick(Sender);
end;

procedure TfmSheetImport.TabsClick(Sender: TObject);
begin
  FillGrid(true);
end;

procedure TfmSheetImport.UpdateIniFile;
var
  AppIni   : TIniFile;
  PublicPath : string;
  IniFileName : string;
begin
  PublicPath := TPath.GetHomePath;
  CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  IniFilename := CommonFilePath + 'Geodate.ini';
  AppIni := TIniFile.Create(IniFilename);
  //PublicPath := TPath.GetPublicPath;
  //CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  //IniFilename := CommonFilePath + 'GDW.ini';
  //AppIni := TIniFile.Create(IniFilename);
  try
    //cdsPath := AppIni.ReadString('FilePath','XMLfile',CommonFilePath+'Data\');
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Sample',eSampleNo.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],Element[iAnalTyp,1],eXXStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],Element[iAnalTyp,2],eYYStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],Element[iAnalTyp,3],eZZStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],XRatioStr[iAnalTyp],eXStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'X Precision',eXPrecStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'X Uncertainty',eXErrStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'X Uncertainty type',eXErrTypeStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],YRatioStr[iAnalTyp],eYStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Y Precision',eYPrecStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Y Uncertainty',eYErrStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Y Uncertainty type',eYErrTypeStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Error Correlation',eRStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],ZRatioStr[iAnalTyp],eZStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Z Precision',eZPrecStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Z Uncertainty',eZErrStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Z Uncertainty type',eZErrTypeStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'R Flag',eRFlagStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'P Flag',ePFlagStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Age',eAgeStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Age Uncertainty',eAgeErrStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],WRatioStr[iAnalTyp],eWStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'W Precision',eWPrecStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'W Uncertainty',eWErrStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'W Uncertainty type',eWErrTypeStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Latitude',eLatitudeStr.Text);
    AppIni.WriteString('Spreadsheet columns for '+Process[iAnalTyp],'Longitude',eLongitudeStr.Text);
  finally
    AppIni.Free;
  end;
end;

function TfmSheetImport.ConvertCol2Int(AnyString : string) : integer;
var
  itmp    : integer;
  tmpStr  : string;
  tmpChar : char;
begin
    AnyString := UpperCase(AnyString);
    tmpStr := AnyString;
    ClearNull(tmpStr);
    Result := 0;
    if (length(tmpStr) = 2) then
    begin
      tmpChar := tmpStr[1];
      itmp := (ord(tmpChar)-64)*26;
      tmpChar := tmpStr[2];
      Result := itmp+(ord(tmpChar)-64);
    end else
    begin
      tmpChar := tmpStr[1];
      Result := (ord(tmpChar)-64);
    end;
end;

procedure TfmSheetImport.bbImportClick(Sender: TObject);
var
  j     : integer;
  iCode : integer;
  IgnoreErrorsR, IgnoreErrorsRFlag, IgnoreErrorsPFlag : boolean;
  i : integer;
  FromRow, ToRow : integer;
  tmpStr, tmpXErrTypeStr, tmpYErrTypeStr,
  tmpZErrTypeStr, tmpWErrTypeStr : string;
  iSampleNo, iXX, iYY, iZZ,
  iX, iXPrec, iXErr, iXErrType,
  iY, iYPrec, iYErr, iYErrType,
  iZ, iZPrec, iZErr, iZErrType,
  iAge, iAgeErr,
  iW, iWPrec, iWErr, iWErrType,
  iR,
  iRFlag, iPFlag,
  iLatitude,
  iLongitude : integer;
  PosDot : integer;
  ButtonSelected : integer;

begin
  ProjectName := ExtractFileName(OpenDialogSprdSheet.FileName);
  PosDot := Pos('.',ProjectName);
  ProjectName := Copy(ProjectName,1,PosDot-1);
  iCode := 1;
  repeat
    tmpStr := meFromRow.Text;
    Val(tmpStr, FromRow, iCode);
    if (iCode = 0) then
    begin
      tmpStr := meToRow.Text;
      Val(tmpStr, ToRow, iCode);
    end else
    begin
      ShowMessage('Incorrect value entered for From row');
      Exit;
    end;
    if (iCode = 0) then
    begin
      if (ToRow >= FromRow) then iCode := 0
                            else iCode := -1;
    end else
    begin
      ShowMessage('Incorrect value entered for To row');
      Exit;
    end;
    if (iCode <> 0)
      then begin
        ShowMessage('Incorrect values entered for rows to import');
        Exit;
      end;
  until (iCode = 0);
  if ((ToRow-FromRow) > MaxSamp) then
  begin
        ShowMessage('Too many samples. Maximum allowed is '+Int2Str(MaxSamp));
        ToRow := FromRow+MaxSamp-1;
  end;

  iSampleNo := ConvertCol2Int(eSampleNo.Text);
  iXX := ConvertCol2Int(eXXStr.Text);
  iYY := ConvertCol2Int(eYYStr.Text);
  iZZ := ConvertCol2Int(eZZStr.Text);
  iX := ConvertCol2Int(eXStr.Text);
  iXPrec := ConvertCol2Int(eXPrecStr.Text);
  iXErr := ConvertCol2Int(eXErrStr.Text);
  iXErrType := ConvertCol2Int(eXErrTypeStr.Text);
  iY := ConvertCol2Int(eYStr.Text);
  iYPrec := ConvertCol2Int(eYPrecStr.Text);
  iYErr := ConvertCol2Int(eYErrStr.Text);
  iYErrType := ConvertCol2Int(eYErrTypeStr.Text);
  iZ := ConvertCol2Int(eZStr.Text);
  iZPrec := ConvertCol2Int(eZPrecStr.Text);
  iZErr := ConvertCol2Int(eZErrStr.Text);
  iZErrType := ConvertCol2Int(eZErrTypeStr.Text);
  iR := ConvertCol2Int(eRStr.Text);
  iRFlag := ConvertCol2Int(eRFlagStr.Text);
  iPFlag := ConvertCol2Int(ePFlagStr.Text);
  iAge := ConvertCol2Int(eAgeStr.Text);
  iAgeErr := ConvertCol2Int(eAgeErrStr.Text);
  iW := ConvertCol2Int(eWStr.Text);
  iWPrec := ConvertCol2Int(eWPrecStr.Text);
  iWErr := ConvertCol2Int(eWErrStr.Text);
  iWErrType := ConvertCol2Int(eWErrTypeStr.Text);
  iLatitude := ConvertCol2Int(eLatitudeStr.Text);
  iLongitude := ConvertCol2Int(eLongitudeStr.Text);

  j := 0;
  IgnoreErrorsR := false;
  IgnoreErrorsRFlag := false;
  IgnoreErrorsPFlag := false;
  for i := FromRow to ToRow do
  begin
    j := j + 1;
    Conc[j,1] := 0.0;
    Conc[j,2] := 0.0;
    Conc[j,3] := 0.0;
    Ratio[j,1] := 0.0;
    XPrec[j] := 0.0;
    ErrorWt[j,1] := 1.0;
    Ratio[j,2] := 0.0;
    YPrec[j] := 0.0;
    ErrorWt[j,2] := 1.0;
    Ratio[j,3] := 0.0;
    ZPrec[j] := 0.0;
    ErrorWt[j,3] := 1.0;
    R[j] := 0.0;
    ErrTyp[j] := '4';
    RFlg[j] := 'Y';
    PFlg[j] := 'Y';
    Ratio[j,0] := 0.0;
    ErrorWt[j,0] := 1.0;
    Ratio[j,4] := 0.0;
    WPrec[j] := 0.0;
    ErrorWt[j,4] := 1.0;
    Latitude[j] := 0.0;
    Longitude[j] := 0.0;
    tmpStr := '';
    if (iSampleNo > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iSampleNo);
      SmpNo[j] := eSmpPrefix.Text+tmpStr;
    end;
    tmpStr := '';
    if (iXX > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iXX);
      Val(tmpStr,Conc[j,1],iCode);
    end;
    tmpStr := '';
    if (iYY > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iYY);
      Val(tmpStr,Conc[j,2],iCode);
    end;
    tmpStr := '';
    if (iZZ > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iZZ);
      Val(tmpStr,Conc[j,3],iCode);
    end;
    tmpStr := '';
    if (iX > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iX);
      Val(tmpStr,Ratio[j,1],iCode);
    end;
    tmpStr := '';
    if (iXPrec > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iXPrec);
      Val(tmpStr,XPrec[j],iCode);
    end;
    tmpStr := '';
    if (iXErr > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iXErr);
      Val(tmpStr,ErrorWt[j,1],iCode);
    end;
    tmpStr := '';
    if (iXErrType > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iXErrType);
      tmpXErrTypeStr := tmpStr;
    end;
    tmpStr := '';
    if (iY > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iY);
      Val(tmpStr,Ratio[j,2],iCode);
    end;
    tmpStr := '';
    if (iYPrec > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iYPrec);
      Val(tmpStr,YPrec[j],iCode);
    end;
    tmpStr := '';
    if (iYErr > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iYErr);
      Val(tmpStr,ErrorWt[j,2],iCode);
    end;
    tmpStr := '';
    if (iYErrType > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iYErrType);
      tmpYErrTypeStr := tmpStr;
    end;
    tmpStr := '';
    if (iZ > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iZ);
      Val(tmpStr,Ratio[j,3],iCode);
    end;
    tmpStr := '';
    if (iZPrec > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iZPrec);
      Val(tmpStr,ZPrec[j],iCode);
    end;
    tmpStr := '';
    if (iZErr > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iZErr);
      Val(tmpStr,ErrorWt[j,3],iCode);
    end;
    tmpStr := '';
    if (iZErrType > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iZErrType);
      tmpZErrTypeStr := tmpStr;
    end;
    tmpStr := '';
    if ((tmpXErrTypeStr = '%') and
        (tmpYErrTypeStr = '%')) then ErrTyp[j] := '1';
    if ((tmpXErrTypeStr = '%') and
        (tmpYErrTypeStr = 'a')) then ErrTyp[j] := '2';
    if ((tmpXErrTypeStr = 'a') and
        (tmpYErrTypeStr = '%')) then ErrTyp[j] := '3';
    if ((tmpXErrTypeStr = 'a') and
        (tmpYErrTypeStr = 'a')) then ErrTyp[j] := '4';
    tmpStr := '';
    if (iR > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iR);
      Val(tmpStr,R[j],iCode);
      if (Abs(R[j]) >= 1.0) then
      begin
        if ((MessageDlg('Error correlation coefficient for sample '+SmpNo[j]+' is incorrect.',
          mtWarning, [mbOK,mbAll], 0) = mrOK) or (IgnoreErrorsR)) then
        begin
          R[j] := 0.0;
        end else
        begin
          if (Abs(R[j]) >= 1.0) then R[j] := 0.0;
          IgnoreErrorsR := true;
        end;
      end;
    end;
    tmpStr := '';
    if (iRFlag > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iRFlag);
      ClearNull(tmpStr);
      if ((tmpStr = 'Y') or (tmpStr = 'N')) then RFlg[j] := tmpStr[1]
      else begin
        if ((MessageDlg('Regress flag for sample '+SmpNo[j]+' is incorrect.',
          mtWarning, [mbOK,mbAll], 0) = mrOK) or (IgnoreErrorsRFlag)) then
        begin
          RFlg[j] := 'N';
        end else
        begin
          RFlg[j] := 'N';
          IgnoreErrorsRFlag := true;
        end;
      end;
    end;
    tmpStr := '';
    if (iPFlag > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iPFlag);
      ClearNull(tmpStr);
      if ((tmpStr = 'Y') or (tmpStr = 'N')) then PFlg[j] := tmpStr[1]
      else begin
        if ((MessageDlg('Plot flag for sample '+SmpNo[j]+' is incorrect.',
          mtWarning, [mbOK,mbAll], 0) = mrOK) or (IgnoreErrorsPFlag)) then
        begin
          PFlg[j] := 'Y';
        end else
        begin
          PFlg[j] := 'Y';
          IgnoreErrorsPFlag := true;
        end;
      end;
    end;
    tmpStr := '';
    if (iAge > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iAge);
      Val(tmpStr,Ratio[j,0],iCode);
    end;
    tmpStr := '';
    if (iAgeErr > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iAgeErr);
      Val(tmpStr,ErrorWt[j,0],iCode);
    end;
    tmpStr := '';
    if (iW > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iW);
      Val(tmpStr,Ratio[j,4],iCode);
    end;
    tmpStr := '';
    if (iWPrec > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iWPrec);
      Val(tmpStr,WPrec[j],iCode);
    end;
    tmpStr := '';
    if (iWErr > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iWErr);
      Val(tmpStr,ErrorWt[j,4],iCode);
    end;
    tmpStr := '';
    if (iWErrType > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iWErrType);
      tmpWErrTypeStr := tmpStr;
    end;
    tmpStr := '';
    if (iLatitude > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iLatitude);
      Val(tmpStr,Latitude[j],iCode);
    end;
    tmpStr := '';
    if (iLongitude > 0) then
    begin
      tmpStr := Xls.GetStringFromCell(i,iLongitude);
      Val(tmpStr,Longitude[j],iCode);
    end;
  end;
  NumberOfPoints := j;
  UpdateIniFile;
end;

function TfmSheetImport.GetStringFromCell(iRow,iCol : integer) : string;
begin
  Result := Xls.GetStringFromCell(iRow,iCol);
end;

procedure TfmSheetImport.SheetDataSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  //SelectedCell(aCol, aRow);
  CanSelect := true;
end;

procedure TfmSheetImport.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Xls);
  inherited;
end;

procedure TfmSheetImport.FormShow(Sender: TObject);
var
  i, j : integer;
  iLastIsoSys : integer;
begin
  gbIsoSys.Visible := false;
  lXAxisStr.Visible := false;
  lvs.Visible := false;
  lYAxisStr.Visible := false;
  gbDefineFields.Visible := false;
  gbDefineRows.Visible := false;
  //gbDefineFields.Visible := true;
  //gbDefineRows.Visible := true;
  gbDefineTabSheet.Visible := false;
  bbImport.Visible := false;
  //bbImport.Visible := true;
  gbPrefix.Visible := false;
  lYAxisStr.Visible := true;
  lYAxisStr.Caption := 'Select an isotope system to start';
  cbxIsoSys.Items.Clear;
  for i := 0 to Maxtype do
  begin
    cbxIsoSys.Items.Add(Process[i]);
  end;
  Title := '';
  N_Rep := 999;
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
    //  J = Age-Epsilon values
  case AnalType of
    '0' : iLastIsoSys := 0;
    '1' : iLastIsoSys := 1;
    '2' : iLastIsoSys := 2;
    '3' : iLastIsoSys := 3;
    '4' : iLastIsoSys := 4;
    '5' : iLastIsoSys := 5;
    '6' : iLastIsoSys := 6;
    '7' : iLastIsoSys := 7;
    '8' : iLastIsoSys := 8;
    '9' : iLastIsoSys := 9;
    'A' : iLastIsoSys := 10;
    'B' : iLastIsoSys := 11;
    'C' : iLastIsoSys := 12;
    'D' : iLastIsoSys := 13;
    'E' : iLastIsoSys := 14;
    'F' : iLastIsoSys := 15;
    'G' : iLastIsoSys := 16;
    'H' : iLastIsoSys := 17;
    'I' : iLastIsoSys := 18;
    'J' : iLastIsoSys := 19;
    'K' : iLastIsoSys := 20;
    'L' : iLastIsoSys := 21;
    'M' : iLastIsoSys := 22;
  end;
  cbxIsoSys.ItemIndex := iLastIsoSys;
end;

procedure TfmSheetImport.cbSheetNameChange(Sender: TObject);
begin
  //ImportSheetNumber := cbSheetName.ItemIndex+1;

  Tabs.TabIndex := cbSheetname.ItemIndex;
  {
  FlexCelImport1.ActiveSheet:= TabControl.TabIndex+1;
  Data.ApplySheet;
  Data.Zoom := 70;
  Data.LoadSheet;
  }
end;

procedure TfmSheetImport.cbxIsoSysChange(Sender: TObject);
begin
  iAnalTyp := cbxIsoSys.ItemIndex;
  GetIniFile;
  case iAnalTyp of
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
    18 : AnalType := 'I';
    19 : AnalType := 'J';
    20 : AnalType := 'K';
    21 : AnalType := 'L';
    22 : AnalType := 'M';
  end;
  lXXStr.Caption := Element[iAnalTyp,1];
  lYYStr.Caption := Element[iAnalTyp,2];
  lZZStr.Caption := Element[iAnalTyp,3];
  lXStr.Caption := XRatioStr[iAnalTyp];
  lYStr.Caption := YRatioStr[iAnalTyp];
  lZStr.Caption := ZRatioStr[iAnalTyp];
  lWStr.Caption := WRatioStr[iAnalTyp];
  gbDefineFields.Visible := true;
  gbDefineRows.Visible := true;
  bbImport.Visible := true;
  lXAxisStr.Caption := XRatioStr[iAnalTyp];
  lYAxisStr.Caption := YRatioStr[iAnalTyp];
  lXAxisStr.Visible := true;
  lvs.Visible := true;
  lYAxisStr.Visible := true;
  gbPrefix.Visible := true;
end;


procedure TfmSheetImport.eSampleNoExit(Sender: TObject);
var
  i, len : integer;
  tmpStr : string;
  tmpChar : char;
begin
  if (Sender = eSampleNo) then
  begin
    tmpStr := eSampleNo.Text;
    eSampleNo.Text := UpperCase(tmpStr);
  end;
  if (Sender = eXXStr) then
  begin
    tmpStr := eXXStr.Text;
    eXXStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eYYStr) then
  begin
    tmpStr := eYYStr.Text;
    eYYStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eZZStr) then
  begin
    tmpStr := eYYStr.Text;
    eYYStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eXStr) then
  begin
    tmpStr := eXStr.Text;
    eXStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eYStr) then
  begin
    tmpStr := eYStr.Text;
    eYStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eZStr) then
  begin
    tmpStr := eZStr.Text;
    eZStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eAgeStr) then
  begin
    tmpStr := eAgeStr.Text;
    eAgeStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eWStr) then
  begin
    tmpStr := eWStr.Text;
    eWStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eRStr) then
  begin
    tmpStr := eRStr.Text;
    eRStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eXPrecStr) then
  begin
    tmpStr := eXPrecStr.Text;
    eXPrecStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eYPrecStr) then
  begin
    tmpStr := eYPrecStr.Text;
    eYPrecStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eZPrecStr) then
  begin
    tmpStr := eZPrecStr.Text;
    eZPrecStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eWPrecStr) then
  begin
    tmpStr := eWPrecStr.Text;
    eWPrecStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eXErrStr) then
  begin
    tmpStr := eXErrStr.Text;
    eXErrStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eYErrStr) then
  begin
    tmpStr := eYErrStr.Text;
    eYErrStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eZErrStr) then
  begin
    tmpStr := eZErrStr.Text;
    eZErrStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eAgeErrStr) then
  begin
    tmpStr := eAgeErrStr.Text;
    eAgeErrStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eWErrStr) then
  begin
    tmpStr := eWErrStr.Text;
    eWErrStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eXErrTypeStr) then
  begin
    tmpStr := eXErrTypeStr.Text;
    eXErrTypeStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eYErrTypeStr) then
  begin
    tmpStr := eYErrTypeStr.Text;
    eYErrTypeStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eZErrTypeStr) then
  begin
    tmpStr := eZErrTypeStr.Text;
    eZErrTypeStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eWErrTypeStr) then
  begin
    tmpStr := eWErrTypeStr.Text;
    eWErrTypeStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eRFlagStr) then
  begin
    tmpStr := eRFlagStr.Text;
    eRFlagStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = ePFlagStr) then
  begin
    tmpStr := ePFlagStr.Text;
    ePFlagStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eLatitudeStr) then
  begin
    tmpStr := eLatitudeStr.Text;
    eLatitudeStr.Text := UpperCase(tmpStr);
  end;
  if (Sender = eLongitudeStr) then
  begin
    tmpStr := eLongitudeStr.Text;
    eLongitudeStr.Text := UpperCase(tmpStr);
  end;
  tmpStr := UpperCase(tmpStr);
  len := length(tmpStr);
  if ((len > 2) or (len < 1)) then
    ShowMessage('Incorrect column descriptor. Values may be zero (0) or A..ZZ');
  for i := 1 to len do
  begin
    tmpChar := tmpStr[i];
    if ((ord(tmpChar) <> 48) and ((ord(tmpChar) < 65) or (ord(tmpChar) > 90))) then
      ShowMessage('Incorrect column. Values may be zero (0) or A..ZZ');
  end;
end;

procedure TfmSheetImport.bbCancelClick(Sender: TObject);
begin
  ModalResult := mrNone;
  bbImport.ModalResult := mrNone;
  Close;
end;

end.
