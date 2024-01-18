unit gd_file;

interface

uses
  System.SysUtils,
  Dialogs, System.UITypes,
  allsorts,
  gdw_varb, gd_drv;

procedure TooMany;
procedure Memory_to_Record(J:integer);
procedure Record_to_Memory(J:integer);
procedure Get_File_Data;
procedure Write_File_Data;
procedure Geodate_Memory_to_Record(J:integer);
procedure Geodate_Record_to_Memory(J:integer);
procedure Get_Geodate_File_Data;
procedure Write_Geodate_File_Data;

implementation

procedure TooMany;
var
  MaxStr   : string[5];
begin
  Str(MaxSamp:4,MaxStr);
  MessageDlg('Too many samples!! Maximum allowed = '+MaxStr, mtWarning,[mbOk], 0);
end;

procedure Record_to_Memory(J:integer);
var
  AnTyp, ErTyp, RFlag, PFlag : char;
begin
  with TempdataRec do begin
    Smpno[J]:=Sano;
    Conc[J,1]:=XElemConc;
    Conc[J,2]:=YElemConc;
    Ratio[J,1]:=XRatio;
    XPrec[J]:=XPrecis;
    Ratio[J,2]:=YRatio;
    YPrec[J]:=YPrecis;
    ErrorWt[J,1]:=XError;
    ErrorWt[J,2]:=YError;
    R[J]:=Correl;
    //ErrTyp[J]:=ErTyp;
    ErTyp:=Char(ErTypANSI);
    AnTyp:=Char(AnTypANSI);
    //AnalTyp[J]:=AnTyp;
    //RFlg[J]:=RFlag;
    //PFlg[J]:=PFlag;
    RFlag:=Char(RFlagANSI);
    PFlag:=Char(PFlagANSI);
    Ratio[J,3]:=ZRatio;
    ZPrec[J]:=ZPrecis;
    ErrorWt[J,3]:=ZError;
  end;{with}
    ErrTyp[J]:=ErTyp;
    AnalTyp[J]:=AnTyp;
    RFlg[J]:=RFlag;
    PFlg[J]:=PFlag;
end;{procedure Record_to_Memory}


procedure Get_File_Data;
var
  Readdone : boolean;
begin
  Readdone:=false;
  FillChar(TempDataRec,SizeOf(TempDataRec),0);
  {$I-}
  if not (ProcessOption='F') then begin
    FillChar(TempDataRec,SizeOf(TempDataRec),0);
    Seek (York_File,0);
    Read (York_File,TempDataRec);
    Title:=TempDataRec.Tit;
    {
    MessageDlg('Title = '+Title,mtWarning,[mbOk], 0);
    }
    N_Rep := TempDataRec.NRep;
    if ((N_Rep > 0) and (N_Rep <= 999))
      then N_Rep := N_Rep
      else N_Rep := 60;
  end;
  I:=1;
  J:=NumberOfPoints;
  repeat
    NumberOfPoints:=NumberOfPoints+1;
    Seek (York_File,I);
    Read (York_File,TempDataRec);
    I:=I+1;
    J:=J+1;
    if (J<=MaxSamp) then
    begin
      RecordNo[J]:=FilePos(York_File);
      //ShowMessage(TempDataRec.ErTyp);
      Record_to_Memory(J);
      {
      MessageDlg('Sample = '+SmpNo[J],mtInformation,[mbOk], 0);
      }
    end
    else begin
      TooMany;
      Readdone:=true;
      J:=J-1;
      NumberOfPoints:=MaxSamp;
    end;
  until (EOF(York_file) or Readdone);
  {$I+}
end;{procedure Get_File_Data}

procedure Memory_to_Record(J:integer);
begin
  with TempdataRec do begin
    Sano:=Smpno[J];
    XElemConc:=Conc[J,1];
    YElemConc:=Conc[J,2];
    XRatio:=Ratio[J,1];
    XPrecis:=XPrec[J];
    YRatio:=Ratio[J,2];
    YPrecis:=YPrec[J];
    XError:=ErrorWt[J,1];
    YError:=ErrorWt[J,2];
    Correl:=R[J];
    ErTypANSI:=ANSIChar(ErrTyp[J]);
    AnTypANSI:=ANSIChar(AnalTyp[J]);
    RFlagANSI:=ANSIChar(UpCase(RFlg[J]));
    PFlagANSI:=ANSIChar(UpCase(PFlg[J]));
    ZRatio:=Ratio[J,3];
    ZPrecis:=ZPrec[J];
    ZError:=ErrorWt[J,3];
  end;{with}
end;{procedure Memory_to_Record}

procedure Write_File_Data;
var
  J       : integer;
begin
  FillChar(TempDataRec,SizeOf(TempDataRec),0);
  {$I-}
  TempDataRec.Tit:=Title;
  if ((N_Rep > 0) and (N_Rep <= 999))
    then TempDataRec.NRep := N_Rep
    else TempDataRec.NRep := 60;
  Seek (York_File, 0);
  Write (York_File, TempDataRec);
  for J:=1 to NumberOfPoints do begin
    Memory_to_Record(J);
    with TempDataRec do begin
      Seek (York_file, J);
      Write (York_File, TempDataRec);
    end;{with}
  end;{for}
  {$I+}
end;{procedure Write_File_Data}

{
  GeodateRecord      = record
         case shortint of
          1 : (Sano       :  string[20];
              XElemConc, YElemConc,
              XRatio, XPrecis, XError,
              YRatio, YPrecis, YError,
              Correl
                         :  double;
              AnTypANSI      :  ANSIchar;
              ErTypANSI      :  ANSIchar;
              RFlagANSI      :  ANSIchar;
              PFlagANSI      :  ANSIchar;
              //AnTyp      :  char;
              //ErTyp      :  char;
              //RFlag      :  char;
              //PFlag      :  char;
              //AnTypANSI      :  string[1];
              //ErTypANSI      :  string[1];
              //RFlagANSI      :  string[1];
              //PFlagANSI      :  string[1];
              ZRatio, ZPrecis, ZError
                         :  double;
              ZElemConc,
              Age, sAge,
              WRatio, WPrecis, WError,
              rho86 : double;
              ErTypZ, ErTypW : char          );
          2 : (Tit       :  string[80];
               NRep      :  smallint;
              sparefield :  array[1..45] of byte);
         end;
}
procedure Geodate_Record_to_Memory(J:integer);
var
  AnTyp, ErTyp, RFlag, PFlag : char;
begin
  with GeodateDataRec do begin
    Smpno[J]:=Sano;
    Conc[J,1]:=XElemConc;
    Conc[J,2]:=YElemConc;
    Conc[J,3]:=ZElemConc;
    Ratio[J,0]:=Age;
    ErrorWt[J,0]:=sAge;
    Ratio[J,1]:=XRatio;
    XPrec[J]:=XPrecis;
    ErrorWt[J,1]:=XError;
    Ratio[J,2]:=YRatio;
    YPrec[J]:=YPrecis;
    ErrorWt[J,2]:=YError;
    Ratio[J,3]:=ZRatio;
    ZPrec[J]:=ZPrecis;
    ErrorWt[J,3]:=ZError;
    R[J]:=Correl;
    //ErrTyp[J]:=ErTyp;
    ErTyp:=Char(ErTypANSI);
    AnTyp:=Char(AnTypANSI);
    ErTypZ:=Char(ErTypANSI);
    ErTypW:=Char(ErTypANSI);
    //AnalTyp[J]:=AnTyp;
    //RFlg[J]:=RFlag;
    //PFlg[J]:=PFlag;
    RFlag:=Char(RFlagANSI);
    PFlag:=Char(PFlagANSI);
    R2[J]:=rho86;
    Ratio[J,4]:=WRatio;
    WPrec[J]:=WPrecis;
    ErrorWt[J,4]:=WError;
  end;{with}
    ErrTyp[J]:=ErTyp;
    AnalTyp[J]:=AnTyp;
    RFlg[J]:=RFlag;
    PFlg[J]:=PFlag;
end;{procedure Record_to_Memory}


procedure Get_Geodate_File_Data;
var
  Readdone : boolean;
begin
  Readdone:=false;
  FillChar(GeodateDataRec,SizeOf(GeodateDataRec),0);
  {$I-}
  if not (ProcessOption='F') then begin
    FillChar(GeodateDataRec,SizeOf(GeodateDataRec),0);
    Seek (Geodate_File,0);
    Read (Geodate_File,GeodateDataRec);
    Title:=GeodateDataRec.Tit;
    {
    MessageDlg('Title = '+Title,mtWarning,[mbOk], 0);
    }
    N_Rep := GeodateDataRec.NRep;
    if ((N_Rep > 0) and (N_Rep <= 999))
      then N_Rep := N_Rep
      else N_Rep := 60;
  end;
  I:=1;
  J:=NumberOfPoints;
  repeat
    NumberOfPoints:=NumberOfPoints+1;
    Seek (Geodate_File,I);
    Read (Geodate_File,GeodateDataRec);
    I:=I+1;
    J:=J+1;
    if (J<=MaxSamp) then
    begin
      RecordNo[J]:=FilePos(Geodate_File);
      //ShowMessage(TempDataRec.ErTyp);
      Geodate_Record_to_Memory(J);
      {
      MessageDlg('Sample = '+SmpNo[J],mtInformation,[mbOk], 0);
      }
    end
    else begin
      TooMany;
      Readdone:=true;
      J:=J-1;
      NumberOfPoints:=MaxSamp;
    end;
  until (EOF(Geodate_file) or Readdone);
  {$I+}
end;{procedure Get_Geodate_File_Data}

procedure Geodate_Memory_to_Record(J:integer);
begin
  with GeodateDataRec do begin
    Sano:=Smpno[J];
    XElemConc:=Conc[J,1];
    YElemConc:=Conc[J,2];
    XRatio:=Ratio[J,1];
    XPrecis:=XPrec[J];
    YRatio:=Ratio[J,2];
    YPrecis:=YPrec[J];
    XError:=ErrorWt[J,1];
    YError:=ErrorWt[J,2];
    Correl:=R[J];
    ErTypANSI:=ANSIChar(ErrTyp[J]);
    AnTypANSI:=ANSIChar(AnalTyp[J]);
    RFlagANSI:=ANSIChar(UpCase(RFlg[J]));
    PFlagANSI:=ANSIChar(UpCase(PFlg[J]));
    ZRatio:=Ratio[J,3];
    ZPrecis:=ZPrec[J];
    ZError:=ErrorWt[J,3];
  end;{with}
end;{procedure Geodate_Memory_to_Record}

procedure Write_Geodate_File_Data;
var
  J       : integer;
begin
  FillChar(GeodateDataRec,SizeOf(GeodateDataRec),0);
  {$I-}
  GeodateDataRec.Tit:=Title;
  if ((N_Rep > 0) and (N_Rep <= 999))
    then GeodateDataRec.NRep := N_Rep
    else GeodateDataRec.NRep := 60;
  Seek (Geodate_File, 0);
  Write (Geodate_File, GeodateDataRec);
  for J:=1 to NumberOfPoints do begin
    Geodate_Memory_to_Record(J);
    with GeodateDataRec do begin
      Seek (Geodate_file, J);
      Write (Geodate_File, GeodateDataRec);
    end;{with}
  end;{for}
  {$I+}
end;{procedure Write_Geodate_File_Data}

{
procedure GetYorkSystemFile;
type
  YorkSystemRecord = packed record
    case shortint of
      1 : (Meth     : string[11];
           XRT      : string[11];
           YRT      : string[11];
           ZRT      : string[11];
           Elem1    : string[2];
           Elem2    : string[2];
           DC       : real;
           CHUR1    : real;
           CHUR2    : real;
           CHUR3    : real;
           DM1      : real;
           DM2      : real;
           DM3      : real;
           F1       : real;
           F2       : real;
           Extras   : array[1..10] of byte);
      2 : (YTit     : string[127]);
      3 : (A1       : real;
           B1       : real;
           C1       : real;
           D1       : real;
           E1       : real;
           A2       : real;
           B2       : real;
           C2       : real;
           D2       : real;
           E2       : real;
           A3       : real;
           B3       : real;
           C3       : real;
           D3       : real;
           E3       : real;
           A4       : real);
         end;
var
  YorkSystemRec  : YorkSystemRecord;
  YorkSystemFile : file of YorkSystemRecord;
  I       : integer;
begin
  AssignFile(YorkSystemFile,Drive1+'\GDW.SYD');
  Reset(YorkSystemFile);
  IOVal:=IOResult;
  if (IOVal = 0) then
  begin
    for I:=0 to MaxType do
    begin
      Seek(YorkSystemFile,I);
      Read(YorkSystemFile,YorkSystemRec);
      Process[I]:=YorkSystemRec.Meth;
      XRatioStr[I]:=YorkSystemRec.XRT;
      YRatioStr[I]:=YorkSystemRec.YRT;
      ZRatioStr[I]:=YorkSystemRec.ZRT;
      Element[I,1]:=YorkSystemRec.Elem1;
      Element[I,2]:=YorkSystemRec.Elem2;
      DecayConst[I]:=YorkSystemRec.DC;
      Chur[I,0]:=YorkSystemRec.CHUR1;
      Chur[I,1]:=YorkSystemRec.CHUR2;
      Chur[I,2]:=YorkSystemRec.CHUR3;
      DM[I,1]:=YorkSystemRec.DM1;
      DM[I,2]:=YorkSystemRec.DM2;
      DM[I,3]:=YorkSystemRec.DM3;
      CalcFac[I,1]:=YorkSystemRec.F1;
      CalcFac[I,2]:=YorkSystemRec.F2;
    end;
    Seek(YorkSystemFile,18);
    Read(YorkSystemFile,YorkSystemRec);
    MuV[0,1]:=YorkSystemRec.A1;
    MuV[0,2]:=YorkSystemRec.B1;
    MuV[0,3]:=YorkSystemRec.C1;
    MuV[0,4]:=YorkSystemRec.D1;
    MuV[0,5]:=YorkSystemRec.E1;
    MuV[1,1]:=YorkSystemRec.A2;
    MuV[1,2]:=YorkSystemRec.B2;
    MuV[1,3]:=YorkSystemRec.C2;
    MuV[1,4]:=YorkSystemRec.D2;
    MuV[1,5]:=YorkSystemRec.E2;
    MuV[2,1]:=YorkSystemRec.A3;
    MuV[2,2]:=YorkSystemRec.B3;
    MuV[2,3]:=YorkSystemRec.C3;
    MuV[2,4]:=YorkSystemRec.D3;
    MuV[2,5]:=YorkSystemRec.E3;
    mu_choice:=Round(YorkSystemRec.A4);
    Seek(YorkSystemFile,19);
    Read(YorkSystemFile,YorkSystemRec);
    CloseFile(YorkSystemFile);
    if (YorkSystemRec.YTit[1] <> '5') then
    begin
      MessageDlg('GDW system default file incompatibility',mtWarning,[mbOk], 0);
      Halt;
    end;
  end
  else begin
    MessageDlg('GDW system default file not found',mtWarning,[mbOk], 0);
    Halt;
  end;
end;
}

end.
