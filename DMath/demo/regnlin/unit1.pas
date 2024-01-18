unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, Printers;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    GroupBox2: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    GroupBox3: TGroupBox;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    LabeledEdit7: TLabeledEdit;
    LabeledEdit8: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    LabeledEdit9: TLabeledEdit;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    SpinEdit2: TSpinEdit;
    CheckBox1: TCheckBox;
    GroupBox4: TGroupBox;
    Label3: TLabel;
    ColorBox1: TColorBox;
    Label4: TLabel;
    SpinEdit3: TSpinEdit;
    RadioGroup1: TRadioGroup;
    LabeledEdit10: TLabeledEdit;
    LabeledEdit11: TLabeledEdit;
    LabeledEdit12: TLabeledEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  FMath, Matrices, Regress, Models, PaString, PlotVar, WinPlot;

var
  InFName      : String;      { Name of input file }
  OutFName     : String;      { Name of output file }
  Title        : String;      { Title of study }
  XName, YName : String;      { Names of variables }
  N            : Integer;     { Number of points }
  X, Y         : TVector;     { Point coordinates }
  RegModel     : Byte;        { Regression model }
  B            : TVector;     { Regression parameters }
  Calc         : Boolean;     { Calculation successful }
  CurvParam    : TCurvParam;  { Parameters for plotted curve }
  Npts         : Integer;     { Nb of points in plotted curve }

  function ReadInputFile(InFName                 : String;
                         var Title, XName, YName : String;
                         var N                   : Integer;
                         var X, Y                : TVector) : Integer;
{ ----------------------------------------------------------------------
  Reads an input file for linear or polynomial regression.
  The input file is an ASCII file with the following structure :

    Line 1 : Title of study
    Line 2 : Number of variables (must be 2 here !)
    Line 3 : Name of variable x
    Line 4 : Name of variable y
    Line 5 : Number of points (must be > number of fitted parameters !)

    The next lines contain the coordinates (x, y) of the points (1 point
    by line). The values of x and y must be separated by spaces or tabs.
  ---------------------------------------------------------------------- }
  var
    InF  : Text;     { Input file }
    Nvar : Integer;  { Number of variables }
    K    : Integer;  { Loop variable }
  begin
    Assign(InF, InFName);
    Reset(InF);

    ReadLn(InF, Title);
    ReadLn(InF, Nvar);

    if Nvar <> 2 then
      begin
        WriteLn('Data file must contain 2 variables !');
        ReadInputFile := - 1;
        Exit;
      end;

    ReadLn(InF, XName);
    ReadLn(InF, YName);
    ReadLn(InF, N);

    DimVector(X, N);
    DimVector(Y, N);

    for K := 1 to N do
      ReadLn(InF, X[K], Y[K]);

    Close(InF);
    ReadInputFile := 0;
  end;

  procedure UpdateAxes;
  { Update axis parameters and display them in the dialog boxes }
  begin
    AutoScale(X, 1, N, XAxis);
    AutoScale(Y, 1, N, YAxis);

    GraphTitle  := Title;
    XAxis.Title := XName;
    YAxis.Title := YName;

    with Form1 do
      begin
        LabeledEdit1.Text := FloatToStr(XAxis.Min);
        LabeledEdit2.Text := FloatToStr(XAxis.Max);
        LabeledEdit3.Text := FloatToStr(XAxis.Step);

        LabeledEdit4.Text := FloatToStr(YAxis.Min);
        LabeledEdit5.Text := FloatToStr(YAxis.Max);
        LabeledEdit6.Text := FloatToStr(YAxis.Step);

        LabeledEdit7.Text := XAxis.Title;
        LabeledEdit8.Text := YAxis.Title;
        LabeledEdit9.Text := GraphTitle;
      end;
  end;

  procedure TForm1.Button1Click(Sender: TObject);
  { Read data file }
  begin
    if OpenDialog1.Execute then
      begin
        InFName := OpenDialog1.FileName;
        if ReadInputFile(InFName, Title, XName, YName, N, X, Y) = 0 then
          UpdateAxes
        else
          MessageDlg('Error reading file ' + InFName, mtError, [mbOk], 0);
      end;
  end;

  procedure TForm1.ComboBox1Change(Sender: TObject);
  begin
    case ComboBox1.ItemIndex of
      0 : RegModel := REG_LIN;
      1 : RegModel := REG_POL;
      2 : RegModel := REG_FRAC;
      3 : RegModel := REG_EXPO;
      4 : RegModel := REG_IEXPO;
      5 : RegModel := REG_EXLIN;
      6 : RegModel := REG_POWER;
      7 : RegModel := REG_MICH;
      8 : RegModel := REG_MINT;
      9 : RegModel := REG_HILL;
     10 : RegModel := REG_LOGIS;
     11 : RegModel := REG_PKA;
    end;

    Label1.Visible := (RegModel in [REG_POL, REG_FRAC, REG_EXPO]);
    Label2.Visible := (RegModel = REG_FRAC);

    SpinEdit1.Visible := Label1.Visible;
    SpinEdit2.Visible := Label2.Visible;

    CheckBox1.Visible := (RegModel in [REG_FRAC, REG_EXPO, REG_IEXPO, REG_LOGIS]);
    CheckBox3.Visible := (RegModel = REG_LOGIS);

    RadioGroup1.Visible := (RegModel = REG_MINT);
    LabeledEdit10.Visible := RadioGroup1.Visible;
    LabeledEdit11.Visible := RadioGroup1.Visible;
    LabeledEdit12.Visible := RadioGroup1.Visible;
    CheckBox2.Visible := RadioGroup1.Visible;

    case RegModel of
      REG_POL  : Label1.Caption := 'Degree of polynomial';
      REG_FRAC : begin
                   Label1.Caption := 'Degree of numerator';
                   Label2.Caption := 'Degree of denominator';
                 end;
      REG_EXPO : Label1.Caption := 'Number of exponentials';
    end;
  end;

  procedure TForm1.RadioGroup1Click(Sender: TObject);
  var
    L1, L2 : string[2];
  begin
    case RadioGroup1.ItemIndex of
      0 : begin
            L1 := 's0';
            L2 := 'e0';
          end;
      1 : begin
            L1 := 'e0';
            L2 := 'T ';
          end;
      2 : begin
            L1 := 's0';
            L2 := 'T ';
          end;
    end;
    LabeledEdit10.EditLabel.Caption := L1;
    LabeledEdit11.EditLabel.Caption := L2;
  end;

  procedure WriteOutputFile(InFName             : String;
                            var OutFName        : String;
                            Title, XName, YName : String;
                            N                   : Integer;
                            Y, Ycalc, S, B      : TVector;
                            V                   : TMatrix;
                            Test                : TRegTest);
{ ----------------------------------------------------------------------
  Writes the result of the regression to an output file
  ---------------------------------------------------------------------- }
  var
    OutF     : Text;     { Output file }
    Line1,
    Line2    : String;   { Separating lines }
    Sr       : Float;    { Residual standard deviation }
    Delta    : Float;    { Residual }
    SB       : TVector;  { Standard deviations of parameters }
    T        : TVector;  { Student's t }
    Prob     : TVector;  { Probabilities }
    I, K     : Integer;  { Loop variables }
  begin
    DimVector(SB, LastParam);
    DimVector(T, LastParam);
    DimVector(Prob, LastParam);

    K := Pos('.', InFName);
    OutFName := Copy(InFName, 1, Pred(K)) + '.out';
    Assign(OutF, OutFName);
    Rewrite(OutF);

    Line1 := StrChar(73, '-');
    Line2 := StrChar(73, '=');

    WriteLn(OutF, Line2);
    WriteLn(OutF, 'Data file  : ', InFName);
    WriteLn(OutF, 'Study name : ', Title);
    WriteLn(OutF, 'x variable : ', XName);
    WriteLn(OutF, 'y variable : ', YName);
    WriteLn(OutF, 'Function   : ', FuncName);

    { Perform tests on parameters }
    ParamTest(B, V, N, FirstParam, LastParam, SB, T, Prob);

    WriteLn(OutF, Line1);
    WriteLn(OutF, 'Parameter    Est.value         Std.dev.        t Student       Prob(>|t|)');
    WriteLn(OutF, Line1);
    for I := FirstParam to LastParam do
      WriteLn(OutF, ParamName(I):5, B[I]:17:8, SB[I]:17:8, T[I]:17:2, Prob[I]:17:4);
    WriteLn(OutF, Line1);
    WriteLn(OutF, 'Number of observations            : n   = ', N:5);

    with Test do
      begin
        Sr := Sqrt(Vr);
        WriteLn(OutF, 'Residual error                    : s   = ', Sr:10:8);
        if (R2 >= 0.0) and (R2 <= 1.0) then
          WriteLn(OutF, 'Coefficient of determination      : r2  = ', R2:10:8);
        if (R2a >= 0.0) and (R2a <= 1.0) then
          WriteLn(OutF, 'Adjusted coeff. of determination  : r2a = ', R2a:10:8);
        Write(OutF, 'Variance ratio (explained/resid.) : F   = ', F:10:4);
        WriteLn(OutF, '    Prob(>F) = ', Prob:6:4);
      end;

    WriteLn(OutF, Line1);
    WriteLn(OutF, '  i        Y obs.       Y calc.      Residual      Std.dev.      Std.res.');
    WriteLn(OutF, Line1);
    for K := 1 to N do
      begin
        Delta := Y[K] - Ycalc[K];
        WriteLn(OutF, K:3, Y[K]:14:4, Ycalc[K]:14:4, Delta:14:4, S[K]:14:4, (Delta / S[K]):14:4);
      end;
    WriteLn(OutF, Line2);
    Close(OutF);
  end;

  procedure TForm1.Button2Click(Sender: TObject);
  { Perform fit }
  var
    U       : TMatrix;  { Matrix of independent variables (not used here) }
    Ycalc   : TVector;  { Expected Y values }
    S       : TVector;  { Standard deviations of Y values }
    CstPar  : TVector;  { Constant parameters }
    Theta   : TVector;  { Variance parameters }
    V       : TMatrix;  { Variance-covariance matrix of parameters }
    B_min,
    B_max   : TVector;  { Parameter bounds }
    RegTest : TRegTest; { Regression tests }
    ErrCode : Integer;  { Error code }
    I       : Integer;  { Loop variable }

  function Checked(CheckBox : TCheckBox) : Byte;
  { Get constant term flag }
  begin
    if CheckBox.Checked then
      Checked := 1
    else
      Checked := 0;
  end;

  begin
    { For the regression models defined in MODELS.PAS,
      the highest index of the constant parameters is 4}
    DimVector(CstPar, 4);

    { Read constant parameters if necessary.
      See the units defining the models (fitpol.pas etc) }
    case RegModel of
      REG_POL   : CstPar[0] := SpinEdit1.Value;
      REG_FRAC  : begin
                    CstPar[0] := SpinEdit1.Value;
                    CstPar[1] := SpinEdit2.Value;
                    CstPar[2] := Checked(CheckBox1);
                  end;
      REG_EXPO  : begin
                    CstPar[0] := SpinEdit1.Value;
                    CstPar[1] := Checked(CheckBox1);
                  end;
      REG_IEXPO : CstPar[0] := Checked(CheckBox1);
      REG_MINT  : begin
                    case RadioGroup1.ItemIndex of
                      0 : begin
                            CstPar[0] := StrToFloat(LabeledEdit10.Text);
                            CstPar[1] := StrToFloat(LabeledEdit11.Text);
                            CstPar[2] := 0.0;
                          end;
                      1 : begin
                            CstPar[0] := 0.0;
                            CstPar[1] := StrToFloat(LabeledEdit10.Text);
                            CstPar[2] := StrToFloat(LabeledEdit11.Text);
                          end;
                      2 : begin
                            CstPar[0] := StrToFloat(LabeledEdit10.Text);
                            CstPar[1] := 0.0;
                            CstPar[2] := StrToFloat(LabeledEdit11.Text);
                          end;
                    end;
                    CstPar[3] := StrToFloat(LabeledEdit12.Text);
                    CstPar[4] := Checked(CheckBox2);
                  end;
      REG_LOGIS : begin
                    CstPar[0] := Checked(CheckBox1);
                    CstPar[1] := Checked(CheckBox3);
                  end;
    end;

    { Initialize regression and variance models.
      Here we use a constant variance model }
    InitModel(RegModel, VAR_CONST, CstPar);

    { Dimension arrays.
      Note: the variance parameters Theta[1]..Theta[LastVarParam]
      must be supplied if we use a non-constant variance model }
    DimVector(Theta, LastVarParam);
    DimVector(B, LastParam);
    DimVector(B_min, LastParam);
    DimVector(B_max, LastParam);
    DimMatrix(V, LastParam, LastParam);
    DimVector(Ycalc, N);
    DimVector(S, N);

    { Initialize bounds }
    for I := FirstParam to LastParam do
      begin
        B_min[I] := -1.0E+6;
        B_max[I] :=  1.0E+6;
      end;

    { Perform regression. The numbers 1000 and 0.001 denote
      the maximal number of iterations and the tolerance on
      the fitted parameters }
    ErrCode := WLSFit(X, U, Y, N, True, 1000, 0.001, Theta,
                      B, B_min, B_max, V, Ycalc, S, RegTest);

    { Write results }
    case ErrCode of
      MAT_OK : begin
                 WriteOutputFile(InFName, OutFName, Title, XName, YName,
                                 N, Y, Ycalc, S, B, V, RegTest);
                 MessageDlg('Results written to file ' + OutFName,
                            mtInformation, [mbOk], 0);
               end;
      MAT_SINGUL   : MessageDlg('Singular matrix', mtError, [mbOk], 0);
      MAT_NON_CONV : MessageDlg('Non-convergence of SVD algorithm', mtError, [mbOk], 0);
    end;

    Calc := (ErrCode = 0);
  end;

  procedure TForm1.Button3Click(Sender: TObject);
  { Display results }
  begin
    Form1.Image1.Visible := False;
    Form1.RichEdit1.Visible := True;

    if OutFName <> '' then
      Form1.RichEdit1.Lines.LoadFromFile(OutFName);
  end;

  procedure ClearGraphic;
  begin
    with Form1.Image1 do
      Canvas.FillRect(Rect(0, 0, Width, Height));
  end;

  procedure GetGraphParam;
  { Read graphic parameters from dialog boxes }
  begin
    with Form1 do
      begin
        XAxis.Min  := StrToFloat(LabeledEdit1.Text);
        XAxis.Max  := StrToFloat(LabeledEdit2.Text);
        XAxis.Step := StrToFloat(LabeledEdit3.Text);

        YAxis.Min  := StrToFloat(LabeledEdit4.Text);
        YAxis.Max  := StrToFloat(LabeledEdit5.Text);
        YAxis.Step := StrToFloat(LabeledEdit6.Text);

        XAxis.Title := LabeledEdit7.Text;
        YAxis.Title := LabeledEdit8.Text;
        GraphTitle  := LabeledEdit9.Text;

        CurvParam.LineParam.Color  := ColorBox1.Selected;
        CurvParam.PointParam.Color := ColorBox1.Selected;

        Npts := SpinEdit3.Value;
      end;
  end;

  function Func(X : Float) : Float;
  { Function to be plotted }
  begin
    Func := RegFunc(X, B);
  end;

  procedure PlotGraph(Canvas : TCanvas);
  begin
    PlotXAxis(Canvas);
    PlotYAxis(Canvas);
    PlotGrid(Canvas);
    WriteTitle(Canvas);

    PlotCurve(Canvas, X, Y, 1, N, CurvParam);

    if Calc then
      PlotFunc(Canvas, Func, XAxis.Min, XAxis.Max,
               Npts, CurvParam.LineParam);
  end;

  procedure TForm1.Button4Click(Sender: TObject);
  { Plot graph }
  begin
    if N = 0 then Exit;

    Form1.Image1.Visible := True;
    Form1.RichEdit1.Visible := False;

    ClearGraphic;
    GetGraphParam;
    InitGraph(Image1.Canvas, Image1.Width, Image1.Height);
    PlotGraph(Image1.Canvas);
  end;

  procedure TForm1.Button5Click(Sender: TObject);
  { Print curve }
  begin
    if N = 0 then Exit;
    Printer.BeginDoc;
    GetGraphParam;
    InitGraph(Printer.Canvas, Printer.PageWidth, Printer.PageHeight);
    PlotGraph(Printer.Canvas);
    Printer.EndDoc;
  end;

  procedure TForm1.Button6Click(Sender: TObject);
  { Quit program }
  begin
    Form1.Close;
  end;

begin
  N := 0;
  OutFName := '';
  RegModel := REG_LIN;

  { Initialize graphic parameters }
  Xwin1 := 10;
  Xwin2 := 90;
  Ywin1 := 10;
  Ywin2 := 90;

  CurvParam.LineParam.Color   := clRed;
  CurvParam.LineParam.Style   := psSolid;
  CurvParam.LineParam.Width   := 1;
  CurvParam.PointParam.Color  := clRed;
  CurvParam.PointParam.Symbol := 1;
  CurvParam.PointParam.Size   := 2;
  CurvParam.Step              := 1;
  CurvParam.Connect           := False;
end.
