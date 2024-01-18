program GDW4;

uses
  Forms,
  Gd_3mn in 'Gd_3mn.pas' {GDW1_MainForm},
  GDW_varb in 'GDW_varb.pas',
  Gd_about in 'Gd_about.pas' {AboutBox},
  GDW_regp in 'GDW_regp.pas',
  GDErnOpt in 'GDErnOpt.pas' {fmGetErrorchronOption},
  Gd_cwt in 'Gd_cwt.pas' {fmConcordiaWtType},
  Gd_cnstr in 'Gd_cnstr.pas' {fmConstrain},
  Gd_file in 'Gd_file.pas',
  Gd_drv in 'Gd_drv.pas' {fmOptDir},
  Gd_edit in 'Gd_edit.pas' {fmEdit},
  Gd_MSWD in 'Gd_MSWD.pas' {fmMSWD},
  dmGdMSWD in 'dmGdMSWD.pas' {dmMSWD: TDataModule},
  dmGdtmpDB in 'dmGdtmpDB.pas' {dmGdwtmp: TDataModule},
  Gd_Evap1 in 'Gd_Evap1.pas' {fmZrEvap},
  Gd_Plat2 in 'Gd_Plat2.pas' {fmPlatAr},
  Gd_MdlAt in 'Gd_MdlAt.pas' {fmGetMdlAt},
  Gd_prec in 'Gd_prec.pas' {fmPrecision},
  Gd_New in 'Gd_New.pas' {fmNewData},
  gd_HstVl in 'gd_HstVl.pas' {fmGetHistValues},
  Gd_sht in 'Gd_sht.pas' {fmSheet},
  Gd_shtim in 'Gd_shtim.pas' {fmSheetImport},
  Gd_AxVl in 'Gd_AxVl.pas' {fmAxOpt},
  gdw_reg1 in 'gdw_reg1.pas' {fmRegressionResult},
  Gd_Ccda in 'Gd_Ccda.pas' {fmConcordiaDate},
  Gd_WtAv in 'Gd_WtAv.pas' {fmWtAv},
  GD_param in 'GD_param.pas' {fmParam},
  RegUser3 in 'RegUser3.pas' {fmRegUser3},
  Gd_PostFm in 'Gd_PostFm.pas' {fmModelPbPostFm},
  Gd_Matrix in 'Gd_Matrix.pas',
  Vcl.Themes,
  Vcl.Styles,
  Allsorts in '..\Eglington Delphi common code items\Allsorts.pas',
  ErrCodes in '..\Eglington Delphi common code items\ErrCodes.pas',
  Mathproc in '..\Eglington Delphi common code items\Mathproc.pas',
  NumRecipes in '..\Eglington Delphi common code items\NumRecipes.pas',
  NumRecipes_varb in '..\Eglington Delphi common code items\NumRecipes_varb.pas';

{$R *.RES}

begin
  Application.Initialize;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.Title := 'GDW';
  Application.CreateForm(TdmGdwtmp, dmGdwtmp);
  Application.CreateForm(TdmMSWD, dmMSWD);
  Application.CreateForm(TGDW1_MainForm, GDW1_MainForm);
  Application.Run;
end.
