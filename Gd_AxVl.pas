unit Gd_AxVl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Mask, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  System.UITypes, ImageCollection_dm;

type
  TfmAxOpt = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    cbAugmentEllipses: TCheckBox;
    meXMinStr: TEdit;
    meXMaxStr: TEdit;
    meYMinStr: TEdit;
    meYMaxStr: TEdit;
    VirtualImageList1: TVirtualImageList;
    procedure bbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XMax, XMin, YMax, YMin : double;
  end;

var
  fmAxOpt: TfmAxOpt;

implementation

uses GDW_varb, dmGdtmpDB;

{$R *.DFM}

procedure TfmAxOpt.bbOKClick(Sender: TObject);
var
  iCode : integer;
  Finished : boolean;
begin
  Finished := false;
  repeat
    Val(meXMinStr.Text,StartAtX,iCode);
    if (iCode <> 0) then
    begin
      MessageDlg('Error in input for X Min',mtWarning,[mbOK],0);
      Finished := false;
    end else
    begin
      Finished := true;
    end;
    Val(meXMaxStr.Text,EndAtX,iCode);
    if (iCode <> 0) then
    begin
      MessageDlg('Error in input for X Min',mtWarning,[mbOK],0);
      Finished := false;
    end else
    begin
      if Finished then Finished := true;
    end;
    Val(meYMinStr.Text,StartAtY,iCode);
    if (iCode <> 0) then
    begin
      MessageDlg('Error in input for X Min',mtWarning,[mbOK],0);
      Finished := false;
    end else
    begin
      if Finished then Finished := true;
    end;
    Val(meYMaxStr.Text,EndAty,iCode);
    if (iCode <> 0) then
    begin
      MessageDlg('Error in input for X Min',mtWarning,[mbOK],0);
      Finished := false;
    end else
    begin
      if Finished then Finished := true;
    end;
  until Finished;
  if (CharInSet(AnalType8,['U','L']) and (cbAugmentEllipses.Checked))
  then ChooseEllipse := 'A';
end;

procedure TfmAxOpt.FormShow(Sender: TObject);
begin
  meXMinStr.Text := FormatFloat('####0.000000',XMin);
  meXMaxStr.Text := FormatFloat('####0.000000',XMax);
  meYMinStr.Text := FormatFloat('####0.000000',YMin);
  meYMaxStr.Text := FormatFloat('####0.000000',YMax);
  if CharInSet(AnalType8,['L','U']) then cbAugmentEllipses.Enabled := true
                                    else cbAugmentEllipses.Enabled := false;
  if (ChooseEllipse = 'A') then cbAugmentEllipses.Checked := true
                           else cbAugmentEllipses.Checked := false;
end;

end.
