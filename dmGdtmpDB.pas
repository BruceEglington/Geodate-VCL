unit dmGdtmpDB;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBClient, Vcl.BaseImageCollection, Vcl.ImageCollection, System.ImageList,
  Vcl.ImgList,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.StorageXML, FireDAC.Stan.StorageBin;

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
    cdsEditZWt: TFloatField;
    cdsEditZWtType: TStringField;
    cdsEditWRatio: TFloatField;
    cdsEditWPrec: TFloatField;
    cdsEditWWt: TFloatField;
    cdsEditWWtType: TStringField;
    cdsEditAgeValue: TFloatField;
    cdsEditAge95pcValue: TFloatField;
    cdsEditRhoExtra: TFloatField;
    cdsEditZZ: TFloatField;
    cdsEditIsotopeSystem: TStringField;
    FDMemTableDataLatitude: TFloatField;
    FDMemTableDataLongitude: TFloatField;
    FDMemTableDataElevation: TFloatField;
    FDMemTableDataIsotopeProcess: TStringField;
    cdsEditIsotopeProcess: TStringField;
    FDMemTableDataIsotopeSystem: TStringField;
    FDMemTableDataProject: TStringField;
    cdsEditAnTyp: TStringField;
    FDMemTableResults: TFDMemTable;
    FDMemTableResultsProject: TStringField;
    FDMemTableResultsIsotopeSystemID: TStringField;
    FDMemTableResultsAgeX: TFloatField;
    FDMemTableResultsAgeY: TFloatField;
    FDMemTableResultsAgeZ: TFloatField;
    FDMemTableResultssAgeXPlus: TFloatField;
    FDMemTableResultssAgeXMinus: TFloatField;
    FDMemTableResultssAgeYPlus: TFloatField;
    FDMemTableResultssAgeYMinus: TFloatField;
    FDMemTableResultssAgeZPlus: TFloatField;
    FDMemTableResultssAgeZMinus: TFloatField;
    FDMemTableResultsDecayConst1: TFloatField;
    FDMemTableResultsDecayConst2: TFloatField;
    FDMemTableResultssDecayConst1: TFloatField;
    FDMemTableResultssDecayConst2: TFloatField;
    FDMemTableResultsIsotopeConstant: TFloatField;
    FDMemTableResultsMSWDequivalence: TFloatField;
    FDMemTableResultsnReplicates: TFloatField;
    FDMemTableResultsnSamples: TFloatField;
    FDMemTableResultsProbOfFitequivalence: TFloatField;
    FDMemTableResultsProbOfFitconcordance: TFloatField;
    FDMemTableResultsInitialRatio: TFloatField;
    FDMemTableResultssInitialRatio: TFloatField;
    FDMemTableResultsEpsilonGamma: TFloatField;
    FDMemTableResultssEpsilonGamma: TFloatField;
    FDMemTableResultsAugmentation: TStringField;
    FDMemTableResultsMSWDforced: TStringField;
    FDMemTableResultsAdditional: TStringField;
    FDMemTableResultsDateTimeCreated: TDateTimeField;
    FDMemTableResultsSoftwareUsed: TStringField;
    FDMemTableResultsMSWDconcordance: TFloatField;
    FDMemTableResultsDVUserID: TStringField;
    FDMemTableResultsMaterialID: TStringField;
    FDMemTableResultsLithology: TStringField;
    FDMemTableResultsApproachID: TStringField;
    FDMemTableResultsTechniqueID: TStringField;
    FDMemTableResultsInterpID: TStringField;
    FDMemTableResultsMethodID: TStringField;
    FDMemTableResultsOtherIntercept: TFloatField;
    FDMemTableResultsOtherInterceptPlus: TFloatField;
    FDMemTableResultsOtherInterceptMinus: TFloatField;
    FDMemTableResultsWeighting: TStringField;
    FDMemTableResultsLudwig_p: TFloatField;
    FDMemTableResultsConstraints: TStringField;
    FDMemTableResultsConstraintAge: TFloatField;
    FDMemTableResultsConstraintX: TFloatField;
    FDMemTableResultsConstraintY: TFloatField;
    FDMemTableResultsReferenceID: TIntegerField;
    FDMemTableResultsLabID: TStringField;
    FDMemTableResultsInitialModel: TStringField;
    FDMemTableResultsModelSourceMu: TFloatField;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    FDMemTableResultsTracerUncertainty: TFloatField;
    FDMemTableResultsSampleID: TStringField;
    FDMemTableResultsFrac: TStringField;
    FDMemTableResultsLongitude: TFloatField;
    FDMemTableResultsLatitude: TFloatField;
    FDMemTableResultsElevation: TFloatField;
    FDMemTableResultspLongitude: TFloatField;
    FDMemTableResultspLatitude: TFloatField;
    FDMemTableResultspElevation: TFloatField;
    FDMemTableResultsOriginalNo: TStringField;
    FDMemTableResultsIGSN: TStringField;
    FDMemTableResultsRecordID: TLargeintField;
    FDMemTableResultssModelSourceMuPlus: TFloatField;
    FDMemTableResultssModelSourceMuMinus: TFloatField;
    FDMemTableResultsnSamplesRegressed: TFloatField;
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
