unit RegUser3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  System.IOUtils,
  StdCtrls, Buttons, ExtCtrls, IniFiles, OleCtrls, ComCtrls, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, SVGIconVirtualImageList;

type
  TfmRegUser3 = class(TForm)
    Panel1: TPanel;
    bbCancel: TBitBtn;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    eRegisteredUser: TEdit;
    lSoftwareName: TLabel;
    bbOK: TBitBtn;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    procedure FormShow(Sender: TObject);
    procedure bbOKClick(Sender: TObject);
    procedure bbPrintClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    //RegisteredUser    :  string[30];
    //SoftwareName      :  string[30];
    //VersionNumber     :  string[10];
    RegisteredUser    :  string;
    SoftwareName      :  string;
    VersionNumber     :  string;
    SubmitToAddress   :  string;
    IPAddress         :  string;
    SentByAddress     :  string;
  end;

var
  fmRegUser3: TfmRegUser3;

implementation

uses GDW_varb;
{$R *.DFM}

procedure TfmRegUser3.FormShow(Sender: TObject);
var
  AppIni   : TIniFile;
  PublicPath : string;
begin
  PublicPath := TPath.GetHomePath;
  CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  IniFilename := CommonFilePath + 'Geodate.ini';
  AppIni := TIniFile.Create(IniFilename);
  //PublicPath := TPath.GetPublicPath;
  //CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  //IniFilename := CommonFilePath + 'GDW.ini';
  //AppIni := TIniFile.Create(IniFilename);
  Softwarename := 'GDW';
  VersionNumber := 'v4.x';
  lSoftwareName.Caption := SoftwareName+' '+VersionNumber;
  RegisteredUser := AppIni.ReadString('Registration','Registered user','not defined');
  eRegisteredUser.Text := RegisteredUser;
end;

procedure TfmRegUser3.bbOKClick(Sender: TObject);
var
  AppIni   : TIniFile;
  PublicPath : string;
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
    RegisteredUser := eRegisteredUser.Text;
    RegisteredUser := AppIni.ReadString('Registration','Registered user','not defined');
  finally
    AppIni.Free;
  end;
  //SubmitToAddress := eSubmitToAddress.Text;
  //SentByAddress := eSentByAddress.Text;
  //AppIni := TIniFile.Create(SoftwareName+'.INI');
  //AppIni.WriteString('Registration','Developer address',eSubmitToAddress.Text);
  //AppIni.WriteString('Registration','Registered user',eRegisteredUser.Text);
  //AppIni.WriteString('Registration','User address',eSentByAddress.Text);
  //AppIni.Free;
end;

procedure TfmRegUser3.bbPrintClick(Sender: TObject);
var
  i,j : integer;
  Prn : textfile;
begin
  {
  MailMemo.Lines.Clear;
  MailMemo.Lines.Add('Software registration for '+lSoftwareName.Caption);
  MailMemo.Lines.Add('    ');
  MailMemo.Lines.Add('Send to: ');
  MailMemo.Lines.Add('         Bruce Eglington');
  MailMemo.Lines.Add('         fax : +27-12-841-1278 ');
  MailMemo.Lines.Add('         e-mail : '+eSubmitToAddress.Text);
  MailMemo.Lines.Add('    ');
  MailMemo.Lines.Add('Details for new user are:');
  MailMemo.Lines.Add('     '+'Registered name = '+eRegisteredUser.Text);
  MailMemo.Lines.Add('     '+'e-mail address = '+eSentByAddress.Text);
  MailMemo.Lines.Add('Postal address: ');
  j := 0;
  repeat
    if (mPostalAddress.Lines[j] = '') then mPostalAddress.Lines[j] := '-';
    MailMemo.Lines.Add('     '+mPostalAddress.Lines[j]);
    j := j+1;
  until ((mPostalAddress.Lines[j] = '') and ( j> 1));
  if (MailMemo.Lines.Count > 2) then
  begin
    try
      AssignPrn(Prn);
      Rewrite(Prn);
      Printer.Canvas.Font.Name := 'Courier New';
      Printer.Canvas.Font.Style := [fsBold];
      Printer.Canvas.Font.Size := 12;
      Writeln(Prn,'      ');
      Writeln(Prn,'      ');
      Writeln(Prn,'      ',MailMemo.Lines[0]);
      Printer.Canvas.Font.Style := [];
      Printer.Canvas.Font.Size := 10;
      for i := 1 to MailMemo.Lines.Count-1 do
      begin
        Writeln(Prn,'      ',MailMemo.Lines[i]);
      end;
    finally
      CloseFile(Prn);
    end;
  end;
  }
end;

end.
