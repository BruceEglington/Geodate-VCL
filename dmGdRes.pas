unit dmGdRes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBTables, DB;

type
  TdmGdwResults = class(TDataModule)
    IsoResults: TTable;
    dsIsoResults: TDataSource;
    Suite: TTable;
    dsSuite: TDataSource;
    Lithology: TTable;
    dsLithology: TDataSource;
    LithologyLithology: TStringField;
    Technique: TTable;
    dsTechnique: TDataSource;
    TechniqueTechAbr: TStringField;
    TechniqueTechnique: TStringField;
    SmpReg: TTable;
    dsSmpReg: TDataSource;
    SmpRegRegressionNumber: TIntegerField;
    SmpRegSampleNo: TStringField;
    SmpRegRegressed: TStringField;
    Country: TTable;
    dsCountry: TDataSource;
    CountryList: TTable;
    dsCountryList: TDataSource;
    CountryCountryAbr: TStringField;
    CountryCountry: TStringField;
    CountryListCountryAbr: TStringField;
    CountryListCountry: TStringField;
    dbResults: TDatabase;
    dsInterpretation: TDataSource;
    Interpretation: TTable;
    InterpretationInterpAbr: TStringField;
    InterpretationInterpretation: TStringField;
    IsoSys: TTable;
    dsIsoSys: TDataSource;
    IsoSysIsoSystem: TStringField;
    IsoSysIsoSysNo: TSmallintField;
    SuiteList: TTable;
    dsSuiteList: TDataSource;
    SuiteFormationName: TStringField;
    SuiteFormationRank: TStringField;
    SuiteCountryAbr: TStringField;
    SuiteListFormationName: TStringField;
    SuiteListFormationRank: TStringField;
    SuiteListCountryAbr: TStringField;
    IsoResultsCountryAbr: TStringField;
    IsoResultsSuitName: TStringField;
    IsoResultsLitholName: TStringField;
    IsoResultsIsotopeSystem: TStringField;
    IsoResultsTechAbstr: TStringField;
    IsoResultsDateString: TDateField;
    IsoResultsAge: TFloatField;
    IsoResultsRegressionNumber: TAutoIncField;
    IsoResultsAgeError: TFloatField;
    IsoResultsAgeError2: TFloatField;
    IsoResultsMSWD: TFloatField;
    IsoResultsNReg: TSmallintField;
    IsoResultsNSamples: TSmallintField;
    IsoResultsAlpha: TFloatField;
    IsoResultsN_Replicates: TSmallintField;
    IsoResultsDegFreedom: TSmallintField;
    IsoResultsFCutoff: TFloatField;
    IsoResultsProbOfFit: TFloatField;
    IsoResultsLwrIntercept: TFloatField;
    IsoResultsLwrError: TFloatField;
    IsoResultsLwrError2: TFloatField;
    IsoResultsWeighting: TStringField;
    IsoResultsLudwigp: TFloatField;
    IsoResultsXCentroid: TFloatField;
    IsoResultsYCentroid: TFloatField;
    IsoResultsConstraints: TStringField;
    IsoResultsXConstrain: TFloatField;
    IsoResultsYConstrain: TFloatField;
    IsoResultsAgeConstrain: TFloatField;
    IsoResultsInitRatio: TFloatField;
    IsoResultsInitRatioError: TFloatField;
    IsoResultsEpsilon: TFloatField;
    IsoResultsEpsilonError: TFloatField;
    IsoResultsMu: TFloatField;
    IsoResultsMuError: TFloatField;
    IsoResultsMuError2: TFloatField;
    IsoResultsMumodel: TStringField;
    IsoResultsAugmentation: TStringField;
    IsoResultsDecayConst1: TFloatField;
    IsoResultsDecayConst2: TFloatField;
    IsoResultsSigmaDC1: TFloatField;
    IsoResultsSigmaDC2: TFloatField;
    IsoResultsInterpAbr: TStringField;
    IsoResultsComment: TMemoField;
    IsoResultsProgName: TStringField;
    IsoResultsRegisteredUser: TStringField;
    IsoResultsPolygonNo: TIntegerField;
    IsoResultsPrefLevel: TSmallintField;
    procedure IsoResultsAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    RegressionNumber : integer;
  end;

var
  dmGdwResults: TdmGdwResults;

implementation

{$R *.DFM}

procedure TdmGdwResults.IsoResultsAfterPost(DataSet: TDataSet);
begin
  RegressionNumber := IsoResultsRegressionNumber.AsInteger;
end;

end.
