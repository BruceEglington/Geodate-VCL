unit Gd_lkup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DB, DBCtrls, Grids, DBGrids;

type
  TfmLookup = class(TForm)
    Panel1: TPanel;
    bbOK: TBitBtn;
    Panel2: TPanel;
    dbgLookUp: TDBGrid;
    DBNavigator1: TDBNavigator;
    dsLookup: TDataSource;
    gbCountry: TGroupBox;
    dblCountry: TDBLookupComboBox;
    gbBlockingT: TGroupBox;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    gbLocate: TGroupBox;
    eLookup: TEdit;
    rgSelect: TRadioGroup;
    SpeedButton1: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure eLookupChange(Sender: TObject);
    procedure rgSelectClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmLookup: TfmLookup;

implementation

uses dmDV, GDW_varb;

{$R *.DFM}

procedure TfmLookup.FormShow(Sender: TObject);
begin
  dmDVResults.Country.Open;
  dmDVResults.CountryList.Open;
  dmDVResults.Suite.Open;
  dmDVResults.Lithology.Open;
  dmDVResults.Technique.Open;
  gbCountry.Visible := false;
  dmDVResults.Interpretation.Open;
  dblCountry.KeyValue := LastCountry;
  dmDVResults.Boundary.Open;
  gbBlockingT.Visible := false;
  dmDVResults.BlockingT.Open;
  gbLocate.Visible := false;
  dmDVResults.Equipment.Open;
end;

procedure TfmLookup.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  LastCountry := dblCountry.KeyValue;
  dmDVResults.Country.Close;
  dmDVResults.CountryList.Close;
  dmDVResults.Suite.Close;
  dmDVResults.Lithology.Close;
  dmDVResults.Technique.Close;
  dmDVResults.Interpretation.Close;
  dmDVResults.Boundary.Close;
  dmDVResults.BlockingT.Close;
  dmDVResults.Equipment.Close;
end;

procedure TfmLookup.eLookupChange(Sender: TObject);
begin
  case rgSelect.ItemIndex of
    0 : begin
      dmDVResults.CountryList.Locate('CountryAbr',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    1 : begin
      dmDVResults.Suite.Locate('FormationName',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    2 : begin
      dmDVResults.Lithology.Locate('Lithology',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    3 : begin
      dmDVResults.Technique.Locate('TechAbr',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    4 : begin
      dmDVResults.Interpretation.Locate('InterpAbr',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    5 : begin
      dmDVResults.Boundary.Locate('CountryAbr',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    6 : begin
      dmDVResults.BlockingT.Locate('IsoSystem',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
    7 : begin
      dmDVResults.Equipment.Locate('Equipment',eLookup.Text,[loPartialkey,loCaseInsensitive]);
    end;
  end;
end;

procedure TfmLookup.rgSelectClick(Sender: TObject);
begin
  gbLocate.Visible := false;
  eLookup.Text := '';
  case rgSelect.ItemIndex of
    0 : begin
      dsLookup.DataSet := dmDVResults.CountryList;
      gbCountry.Visible := false;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
    1 : begin
      gbCountry.Visible := true;
      dsLookup.DataSet := dmDVResults.Suite;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
    2 : begin
      dsLookup.DataSet := dmDVResults.Lithology;
      gbCountry.Visible := false;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
    3 : begin
      dsLookup.DataSet := dmDVResults.Technique;
      gbCountry.Visible := false;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
    4 : begin
      dsLookup.DataSet := dmDVResults.Interpretation;
      gbCountry.Visible := false;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
    5 : begin
      dsLookup.DataSet := dmDVResults.Boundary;
      gbCountry.Visible := false;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
    6 : begin
      dsLookup.DataSet := dmDVResults.BlockingT;
      gbCountry.Visible := false;
      gbBlockingT.Visible := true;
      gbLocate.Visible := true;
    end;
    7 : begin
      dsLookup.DataSet := dmDVResults.Equipment;
      gbCountry.Visible := false;
      gbBlockingT.Visible := false;
      gbLocate.Visible := true;
    end;
  end;
end;

procedure TfmLookup.SpeedButton1Click(Sender: TObject);
begin
  eLookup.Text := '';
end;

end.
