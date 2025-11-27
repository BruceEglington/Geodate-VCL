unit gd_HstVl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, ImageCollection_dm;

type
  TfmGetHistValues = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    eMinimum: TEdit;
    eWidth: TEdit;
    eNumInt: TEdit;
    eMaximum: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    VirtualImageList1: TVirtualImageList;
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure eMinimumExit(Sender: TObject);
    procedure eWidthExit(Sender: TObject);
    procedure eNumIntExit(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    WidthInt   : double;
    NumInt     : integer;
  end;

var
  fmGetHistValues: TfmGetHistValues;

implementation

{$R *.DFM}

uses
  gdw_varb, dmGdtmpDB;

procedure TfmGetHistValues.bbOKClick(Sender: TObject);
var
  ICode      : integer;
begin
  ICode := -1;
  repeat
    try
      Val(eMinimum.Text,StartAtX,ICode);
      if (ICode = 0) then Val(eMaximum.Text,EndAtX,ICode);
      if (ICode = 0) then Val(eWidth.Text,WidthInt,ICode);
      if (ICode = 0) then Val(eNumInt.Text,NumInt,ICode);
    except
      ShowMessage('Error in one or more value entered');
    end;
  until (ICode = 0);
  ModalResult := mrOK;
end;

procedure TfmGetHistValues.FormShow(Sender: TObject);
begin
  ActiveControl := eMinimum;
  eMinimum.Text := FormatFloat('###0.00000',StartAtX);
  eMaximum.Text := FormatFloat('###0.00000',EndAtX);
  eWidth.Text := FormatFloat('###0.00000',WidthInt);
  eNumInt.Text := IntToStr(NumInt);
end;

procedure TfmGetHistValues.eMinimumExit(Sender: TObject);
var
  ICode   : integer;
begin
  ICode := -1;
  repeat
    try
      Val(eMinimum.Text,StartAtX,ICode);
      if (ICode = 0) then
      begin
        EndAtX := StartAtX + WidthInt*(NumInt+1);
        eMaximum.Text := FormatFloat('###0.00000',EndAtX);
      end;
    except
      ShowMessage('Incorrect value');
    end;
  until (ICode = 0);
end;

procedure TfmGetHistValues.eWidthExit(Sender: TObject);
var
  ICode   : integer;
begin
  ICode := -1;
  repeat
    try
      Val(eWidth.Text,WidthInt,ICode);
      if (ICode = 0) then
      begin
        EndAtX := StartAtX + WidthInt*(NumInt+1);
        eMaximum.Text := FormatFloat('###0.00000',EndAtX);
      end;
    except
      ShowMessage('Incorrect value');
    end;
  until (ICode = 0);
end;

procedure TfmGetHistValues.eNumIntExit(Sender: TObject);
var
  ICode   : integer;
begin
  ICode := -1;
  repeat
    try
      Val(eNumInt.Text,NumInt,ICode);
      if (NumInt < 1) then
      begin
        ICode := -1;
        eNumInt.Text := '5';
      end;
      if (ICode = 0) then
      begin
        EndAtX := StartAtX + WidthInt*(NumInt+1);
        eMaximum.Text := FormatFloat('###0.00000',EndAtX);
      end;
    except
      ShowMessage('Incorrect value');
    end;
  until (ICode = 0);
end;

procedure TfmGetHistValues.bbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmGetHistValues.FormCreate(Sender: TObject);
begin
  NumInt := 10;
end;

end.
