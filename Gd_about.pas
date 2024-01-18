unit Gd_about;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Vcl.Imaging.pngimage, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, SVGIconVirtualImageList;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TBitBtn;
    ProgramIcon: TImage;
    ProductName: TLabel;
    lVersion: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    lRegisteredUser: TLabel;
    Panel2: TPanel;
    Label1: TLabel;
    SVGIconVirtualImageList1: TSVGIconVirtualImageList;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}
uses
  Gdw_Varb, dmGdtmpDB;

procedure TAboutBox.FormShow(Sender: TObject);
begin
  lRegisteredUser.Caption := 'Registered to: '+RegisteredUser;
  lVersion.Caption := 'Version '+ProgVersion;
end;

end.
 
