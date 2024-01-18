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
  IniFileName : string;
begin
  PublicPath := TPath.GetHomePath;
  CommonFilePath := IncludeTrailingPathDelimiter(PublicPath) + 'EggSoft\';
  IniFilename := CommonFilePath + 'Geodate.ini';
  AppIni := TIniFile.Create(IniFilename);
  Softwarename := 'GDW';
  VersionNumber := 'v5.x';
  lSoftwareName.Caption := SoftwareName+' '+VersionNumber;
  RegisteredUser := AppIni.ReadString('Registration','Registered user','not defined');
  eRegisteredUser.Text := RegisteredUser;
end;

procedure TfmRegUser3.bbOKClick(Sender: TObject);
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

end.
