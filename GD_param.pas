unit GD_param;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList;

type
  TfmParam = class(TForm)
    Panel1: TPanel;
    bbClose: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    VirtualImageList1: TVirtualImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmParam: TfmParam;

implementation

{$R *.DFM}

uses dmGdtmpDB;

end.
