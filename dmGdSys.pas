unit dmGdSys;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBTables, DB;

type
  TdmGdwSys = class(TDataModule)
    GdwSys: TTable;
    dsGdwSys: TDataSource;
    GdwSysIsoSysNo: TSmallintField;
    GdwSysIsoSys: TStringField;
    GdwSysCanChange: TStringField;
    GdwSysXElement: TStringField;
    GdwSysYElement: TStringField;
    GdwSysXRatioName: TStringField;
    GdwSysYRatioName: TStringField;
    GdwSysZRatioName: TStringField;
    GdwSysDecayConst: TFloatField;
    GdwSysXFac: TFloatField;
    GdwSysYFac: TFloatField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmGdwSys: TdmGdwSys;

implementation

{$R *.DFM}

end.
