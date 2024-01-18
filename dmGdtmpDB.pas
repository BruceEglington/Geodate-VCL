unit dmGdtmpDB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBClient, Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList,
  Vcl.ImgList,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  SVGIconImageCollection;

type
  TdmGdwtmp = class(TDataModule)
    dsGdwEdit: TDataSource;
    dsGdwReg: TDataSource;
    dsGdwNew: TDataSource;
    cdsEdit: TClientDataSet;
    cdsNew: TClientDataSet;
    cdsEditProject: TStringField;
    cdsEditSample_No: TStringField;
    cdsEditXX: TFloatField;
    cdsEditYY: TFloatField;
    cdsEditXRatio: TFloatField;
    cdsEditXPrec: TFloatField;
    cdsEditXWt: TFloatField;
    cdsEditXWtType: TStringField;
    cdsEditYRatio: TFloatField;
    cdsEditYPrec: TFloatField;
    cdsEditYWt: TFloatField;
    cdsEditYWtType: TStringField;
    cdsEditZRatio: TFloatField;
    cdsEditZPrec: TFloatField;
    cdsEditR: TFloatField;
    cdsEditRFlag: TStringField;
    cdsEditPFlag: TStringField;
    cdsEditLatitude: TFloatField;
    cdsEditLongitude: TFloatField;
    cdsEditElevation: TFloatField;
    cdsNewSample_No: TStringField;
    cdsReg: TClientDataSet;
    cdsRegSample_No: TStringField;
    cdsRegRFlag: TStringField;
    cdsRegXWt: TFloatField;
    cdsRegXWtType: TStringField;
    cdsRegYWt: TFloatField;
    cdsRegYWtType: TStringField;
    cdsRegXDev: TFloatField;
    cdsRegYDev: TFloatField;
    cdsRegPFlag: TStringField;
    cdsRegProject: TStringField;
    cdsRegXElem: TFloatField;
    cdsRegYElem: TFloatField;
    cdsRegXRatio: TFloatField;
    cdsRegXPrec: TFloatField;
    cdsRegYRatio: TFloatField;
    cdsRegYPrec: TFloatField;
    cdsRegZRatio: TFloatField;
    cdsRegZPrec: TFloatField;
    cdsRegR: TFloatField;
    cdsRegi: TIntegerField;
    ImageCollection1: TImageCollection;
    FDMemTableData: TFDMemTable;
    FDMemTableDataSaNo: TStringField;
    FDMemTableDataXElemConc: TFloatField;
    FDMemTableDataYElemConc: TFloatField;
    FDMemTableDataZElemConc: TFloatField;
    FDMemTableDataX: TFloatField;
    FDMemTableDatapX: TFloatField;
    FDMemTableDatasX: TFloatField;
    FDMemTableDataeX: TStringField;
    FDMemTableDataY: TFloatField;
    FDMemTableDatapY: TFloatField;
    FDMemTableDatasY: TFloatField;
    FDMemTableDataeY: TStringField;
    FDMemTableDataZ: TFloatField;
    FDMemTableDatapZ: TFloatField;
    FDMemTableDatasZ: TFloatField;
    FDMemTableDataeZ: TStringField;
    FDMemTableDataW: TFloatField;
    FDMemTableDatapW: TFloatField;
    FDMemTableDatasW: TFloatField;
    FDMemTableDataeW: TStringField;
    FDMemTableDatarho: TFloatField;
    FDMemTableDatarho2: TFloatField;
    FDMemTableDataAnTyp: TStringField;
    FDMemTableDataRFlag: TStringField;
    FDMemTableDataPFlag: TStringField;
    FDMemTableDataAge: TFloatField;
    FDMemTableDatasAge: TFloatField;
    SVGIconImageCollection1: TSVGIconImageCollection;
    cdsEditZWt: TFloatField;
    cdsEditZWtType: TStringField;
  private
    { Private declarations }
  public
    { Public declarations }
    ChosenStyle : string;
  end;

var
  dmGdwtmp: TdmGdwtmp;

implementation

{$R *.DFM}


end.
