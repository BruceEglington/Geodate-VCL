unit dmGdMSWD;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBClient, midaslib, WideStrings;

type
  TdmMSWD = class(TDataModule)
    dsF: TDataSource;
    cdsF: TClientDataSet;
    cdsFstType: TWideStringField;
    cdsFstAlpha: TFloatField;
    cdsFstNRep: TFloatField;
    cdsFstNSmp: TFloatField;
    cdsFstFvalue: TFloatField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMSWD: TdmMSWD;

implementation

{$R *.DFM}

end.
