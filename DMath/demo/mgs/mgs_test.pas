unit MGS_Test;

interface

uses
{$IFDEF VER80}
  WinTypes, WinProcs,
{$ELSE}
  Windows,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Grids, Buttons,
  FMath, Matrices,    {= from the TPMath library =}
  MGS_LSQ, TestData;

Type
  TFitType = (ftMGS, ftSVD, ftGaussJordan);

Const
  FitTypeSTR : array[ftMGS..ftGaussJordan] of string =
      ('Modified Gram-Schmidt (QR)', 'Singular Value Decomposition', 'Gaussian Elimination');

Type
  TForm1 = class(TForm)
    PolyDegree: TRadioGroup;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    StringGrid1: TStringGrid;
    Label2: TLabel;
    RMSLabel: TLabel;
    Panel1: TPanel;
    sbMGS: TSpeedButton;
    sbSVD: TSpeedButton;
    sbGaussJordan: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PolyDegreeClick(Sender: TObject);
    procedure sbFitTypeClick(Sender: TObject);
  Private
    { Private declarations }
    fMinDensity,
    fMaxDensity,
    fDensityRange   :  float;

    fMinAltitude,
    fMaxAltitude,
    fAltitudeRange  :  float;

    fRMSError : float;

    fPolyDegree : integer;

    fFitType : TFitType;

    Function Xpt(X : float) : integer;
    Function Ypt(Y : float) : integer;

  Public
    PointRadius,
    PointDensity : integer;

    FitData : TVector;
    FitCoefficients : TVector;

    procedure CalcRMSError;
    Procedure DoMGSFit;
    Procedure DoSVDFit;
    Procedure DoGaussJordanFit;
    procedure DrawAxes;
    procedure PlotData;
    Procedure SelectDataSet(VAR startNDX, stopNDX : integer;
                            VAR Z, rho, fit : TVector);
    procedure ShowBestFit;
    procedure UpdateStringGrid;
  END;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  regress;  {= from the TPMath library =}

Const
  MaxLSQdimension = 22;

procedure TForm1.FormCreate(Sender: TObject);
  Var
    i : integer;
    S : string;
  BEGIN
  {= associate the speed buttons with the fit algorithms =}
    sbMGS.tag := ord(ftMGS);
    sbSVD.tag := ord(ftSVD);
    sbGaussJordan.tag := ord(ftGaussJordan);

  {= get the MGS test data =}
    GetTestData;

  {= Choose the index of the first data point for the TPMath algorithms =}
    SetFirstPoint(0);

  {= select initial polynomial degree =}
    fPolyDegree := 3;
    DimVector(FitData, NumDataPTS);
    DimVector(FitCoefficients,  fPolyDegree);

  {= set up a crudimentary data plot =}
    fMinDensity := Density[NumDataPTS];
    fMaxDensity := Density[0];
    fDensityRange := fMaxDensity - fMinDensity;

    fMinAltitude := Altitude[0];
    fMaxAltitude := Altitude[NumDataPTS];
    fAltitudeRange := fMaxAltitude - fMinAltitude;

    PointRadius  := 4;
    PointDensity := 2;

    Canvas.Font.Name := 'Times New Roman';
    Canvas.Font.Size := 8;

    PaintBox1.Canvas.Brush.Style := bsSolid;
    PaintBox1.Canvas.Brush.Color := clWhite;
    PaintBox1.Canvas.Pen.Color   := clBlack;

  {= initialize the string grid to show the values of the fit coefficients =}
    with StringGrid1 do
      BEGIN
        RowCount := MaxLSQdimension+1;
        ColWidths[0] := 32;
        ColWidths[1] := Width - ColWidths[0] - 3;
        for i := 0 to RowCount do
          BEGIN
            S := ' ' + IntToStr(i);
            if (length(S) < 3) then
              S := ' ' + S;
            Cells[0, i] := S;
          END;
      END;

  {= fit the data! =}
    PolyDegreeClick(self);
  END;

Function TForm1.Xpt(X : float) : integer;
  BEGIN
    result := round(((X - fMinDensity)/ fDensityRange)*(PaintBox1.Width-1));
  END;

Function TForm1.Ypt(Y : float) : integer;
  BEGIN
    result := round(((fMaxAltitude - Y)/ fAltitudeRange)*(PaintBox1.Height-1));
  END;

Procedure TForm1.DoMGSFit;
  BEGIN
    mgsPolynomialFit (Altitude, Density, NumDataPTS, fPolyDegree, FitCoefficients);
  END;

Procedure TForm1.DoSVDFit;
  Var
    V : TMatrix;
    FitResult : integer;
  BEGIN
    DimMatrix(V, fPolyDegree, fPolyDegree);
    Try
      FitResult := PolFit(Altitude, Density, NumDataPTS, fPolyDegree, FitCoefficients, V);
      CASE FitResult of
        MAT_OK : ;  {= No error (see TPMath.Matrices.pas); do nothing =}
        MAT_NON_CONV :    {= Non-convergence =}
          MessageDlg('SVD failure: non-convergence of iterative procedure', mtError, [mbOK], 0);
        else
          MessageDlg('SVD failure: unknown error (' + IntToStr(FitResult) + ')', mtError, [mbOK], 0);
      END;  {==CASE FitResult==}
    Except on E : Exception do
      {Application.}ShowException(E, ErrorAddr);
    END;
  END;

Procedure TForm1.DoGaussJordanFit;
  Var
    V : TMatrix;
  BEGIN
    DimMatrix(V, fPolyDegree, fPolyDegree);
    PolFit(Altitude, Density, NumDataPTS, fPolyDegree, FitCoefficients, V);
  END;

Procedure TForm1.DrawAxes;
  Const
    NumXLabels = 4;
    NumYLabels = 5;
    YTicLength = 4;
    XTicLength = 6;
  Var
    XLabel  : array[1..NumXLabels] of string[10];
    XLValue : array[1..NumXLabels] of float;
    YLabel  : array[1..NumYLabels] of string[8];
    YLValue : array[1..NumYLabels] of float;
    i,
    hereX,
    hereY   : integer;
    dX, dY  : float;
  begin
    dX := fDensityRange  / (NumXLabels-1);
    for hereX := 1 to NumXLabels-1 do
      BEGIN
        XLValue[hereX] := Density[NumDataPTS] + (hereX-1)*dX;
        XLabel[hereX]  := FloatToStrF(XLValue[hereX], ffExponent, 4, 1);
      END;
    XLValue[NumXLabels] := Density[1];
    XLabel[NumXLabels] := FloatToStrF(XLValue[NumXLabels], ffExponent, 4, 1);

    dY := fAltitudeRange / (NumYLabels-1);
    for hereY := 1 to NumYLabels-1 do
      BEGIN
        YLValue[hereY] := Altitude[NumDataPTS] - (hereY-1)*dY;
        YLabel[hereY] := IntToStr(round(YLValue[hereY])) + 'km';
      END;
    YLValue[NumYLabels] := Altitude[0];
    YLabel[NumYLabels] := IntToStr(round(YLValue[NumYLabels])) + 'km';

    Canvas.Brush.Style := bsClear;
  {= draw the Y labels =}
    for i := 1 to NumYLabels do
      BEGIN
        hereX := PaintBox1.Left - 2;
        hereY := PaintBox1.Top + Ypt(YLValue[i]);
        Canvas.MoveTo(hereX, hereY);
        dec(hereX, YTicLength);
        Canvas.LineTo(hereX, hereY);
        dec(hereX, 2);
        dec(hereY, Canvas.TextHeight(YLabel[i]) div 2);
        Canvas.TextOut(hereX - Canvas.TextWidth(YLabel[i]), hereY, YLabel[i]);
      END;
  {= draw the X labels =}
    for i := 1 to NumXLabels do
      BEGIN
        hereY := PaintBox1.Top + PaintBox1.Height;
        hereX := PaintBox1.Left + Xpt(XLValue[i]);
        Canvas.MoveTo(hereX, hereY);
        inc(hereY, XTicLength);
        Canvas.LineTo(hereX, hereY);
        inc(hereY, 2);
        dec(hereX, Canvas.TextWidth(XLabel[i]) div 2);
        Canvas.TextOut(hereX, hereY, XLabel[i]);
      END;
  end;

Procedure TForm1.SelectDataSet( VAR startNDX, stopNDX : integer;
                                VAR Z, rho, fit : TVector);
  BEGIN
    startNDX := 0;
    stopNDX  := NumDataPTS;
    Z   := Altitude;
    rho := Density;
    fit := FitData;
  END;

procedure TForm1.PlotData;
  Var
    i, maxPT    : integer;
    Z, rho, fit : TVector;
  BEGIN
    SelectDataSet(i, maxPT, Z, rho, fit);
    with PaintBox1.Canvas do
      BEGIN
        Pen.Width := 1;
        Pen.Color := clBlack;
        while i <= maxPT do
          BEGIN
            Ellipse(Xpt(rho[i])-PointRadius, Ypt(Z[i])-PointRadius,
                    Xpt(rho[i])+PointRadius, Ypt(Z[i])+PointRadius);
            inc(i, PointDensity);
          END;
      END;
  end;

procedure TForm1.ShowBestFit;
  Var
    n0, nMax, n : integer;
    Z, rho, fit : TVector;
  BEGIN
    SelectDataSet(n0, nMax, Z, rho, fit);
    with PaintBox1.Canvas do
      BEGIN
        Pen.Width := 1;
        Pen.Color := clRed;
        MoveTo(Xpt(fit[n0]), Ypt(Z[n0]));
        for n := n0+1 to nMax do
          LineTo(Xpt(fit[n]), Ypt(Z[n]));
      END;
  end;

procedure TForm1.CalcRMSError;
  BEGIN
    fRMSError := RMSerror(FitData, Density, 0, NumDataPTS);
    RMSLabel.Caption := 'RMS error = ' + FloatToStrF(fRMSError, ffExponent, 5, 2);
  END;

procedure TForm1.UpdateStringGrid;
  Var
    i : integer;
    S : string;
  BEGIN
    with StringGrid1 do
      BEGIN
        for i := 0 to fPolyDegree do
          BEGIN
            S := ' ' + FloatToStrF(FitCoefficients[i], ffExponent, 6, 2);
            if (FitCoefficients[i] >= 0.00) then
              S := ' ' + S;
            Cells[1, i] := S;
          END;
        for i := fPolyDegree+1 to RowCount do
          Cells[1,i] := '';
      END;
  end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
  begin
    with PaintBox1.Canvas do
      BEGIN
        Brush.Style := bsSolid;
        Brush.Color := clWhite;
        Pen.Color   := clBlack;
        Rectangle(0, 0, PaintBox1.width, PaintBox1.height);
      END;
    DrawAxes;
    PlotData;
    ShowBestFit;
  end;

procedure TForm1.PolyDegreeClick(Sender: TObject);
  BEGIN
    fPolyDegree := 3 + PolyDegree.ItemIndex;
    DimVector(FitCoefficients,  fPolyDegree);
    CASE fFitType of
      ftMGS : DoMGSFit;
      ftSVD : DoSVDFit;
      ftGaussJordan : DoGaussJordanFit;
    END;
    CalcFitValues(0, NumDataPTS, fPolyDegree, Altitude, FitData, FitCoefficients);
    UpdateStringGrid;
    CalcRMSError;
    CASE fPolyDegree of
      3 : Caption := FitTypeSTR[fFitType] + ' :  Cubic Polynomial Fit';
      4 : Caption := FitTypeSTR[fFitType] + ' :  Quartic Polynomial Fit';
      5 : Caption := FitTypeSTR[fFitType] + ' :  Quintic Polynomial Fit';
      6..20 : Caption := Format('%s :  %dth Order Polynomial Fit', [FitTypeSTR[fFitType], fPolyDegree]);
      21 : Caption := FitTypeSTR[fFitType] + ' :  21st Order Polynomial Fit';
      22 : Caption := FitTypeSTR[fFitType] + ' :  22nd Order Polynomial Fit';
    END;  {=CASE fPolyDegree=}
    PaintBox1Paint(Sender);
  end;

procedure TForm1.sbFitTypeClick(Sender: TObject);
  BEGIN
    if (Sender IS TSpeedButton) then
      BEGIN
        with (Sender AS TSpeedButton) do
          fFitType := TFitType(tag);
      {= select TPMath regression algorithm =}
        if (fFitType = ftSVD) then
          SetRegAlgo(regress.SVD)
        else if (fFitType = ftGaussJordan) then
          SetRegAlgo(regress.GAUSS_JORDAN);
        PolyDegreeClick(self);
      END;
  END;

END.
