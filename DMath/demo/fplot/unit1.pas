unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin;

type
  TForm1 = class(TForm)
    Image1: TImage;
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    ColorBox1: TColorBox;
    Label1: TLabel;
    RadioGroup2: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    LabeledEdit3: TLabeledEdit;
    GroupBox2: TGroupBox;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    LabeledEdit6: TLabeledEdit;
    SpinEdit1: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox3: TGroupBox;
    LabeledEdit7: TLabeledEdit;
    LabeledEdit8: TLabeledEdit;
    LabeledEdit9: TLabeledEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  fmath, fspec, plotvar, winplot;

var
  A, B, Y : Float;  { Function parameters }

function Func(X : Float) : Float;
{ Function to be plotted }
begin
  case Form1.RadioGroup1.ItemIndex of
    0 : Func := Expo(X);
    1 : Func := Exp2(X);
    2 : Func := Exp10(X);
    3 : Func := Power(X, Y);
    4 : Func := Log(X);
    5 : Func := Log2(X);
    6 : Func := Log10(X);
    7 : Func := Sin(X);
    8 : Func := Cos(X);
    9 : Func := Tan(X);
   10 : Func := ArcSin(X);
   11 : Func := ArcCos(X);
   12 : Func := ArcTan(X);
   13 : Func := Sinh(X);
   14 : Func := Cosh(X);
   15 : Func := Tanh(X);
   16 : Func := ArcSinh(X);
   17 : Func := ArcCosh(X);
   18 : Func := ArcTanh(X);
   19 : Func := Gamma(X);
   20 : Func := IGamma(A, X);
   21 : Func := Beta(X, Y);
   22 : Func := IBeta(A, B, X);
   23 : Func := Erf(X);
  else
    Func := 0.0
  end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
{ Display parameter box if necessary }
begin
  with Form1 do
    begin
      LabeledEdit7.Visible := (RadioGroup1.ItemIndex in [3, 21]);
      LabeledEdit8.Visible := (RadioGroup1.ItemIndex in [20, 22]);
      LabeledEdit9.Visible := (RadioGroup1.ItemIndex = 22);
      LabeledEdit7.Visible := (RadioGroup1.ItemIndex in [3, 21]);
      GroupBox3.Visible := (RadioGroup1.ItemIndex in [3, 20, 21, 22]);
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
{ Plot function }
var
  X1, X2, Y1, Y2, dX, dY, Temp : Float;
  LineParam                    : TLineParam;
  Npts                         : Integer;
begin
  X1 := StrToFloat(Form1.LabeledEdit1.Text);
  X2 := StrToFloat(Form1.LabeledEdit2.Text);
  dX := StrToFloat(Form1.LabeledEdit3.Text);

  Y1 := StrToFloat(Form1.LabeledEdit4.Text);
  Y2 := StrToFloat(Form1.LabeledEdit5.Text);
  dY := StrToFloat(Form1.LabeledEdit6.Text);

  if X1 < X2 then
    begin
      XAxis.Min  := X1;
      XAxis.Max  := X2;
      XAxis.Step := dX;
    end;

  if Y1 < Y2 then
    begin
      YAxis.Min  := Y1;
      YAxis.Max  := Y2;
      YAxis.Step := dY;
    end;

  Npts := SpinEdit1.Value;

  Temp := StrToFloat(Form1.LabeledEdit7.Text); if Temp > 0.0 then Y := Temp;
  Temp := StrToFloat(Form1.LabeledEdit8.Text); if Temp > 0.0 then A := Temp;
  Temp := StrToFloat(Form1.LabeledEdit9.Text); if Temp > 0.0 then B := Temp;

  LineParam.Width := Form1.RadioGroup2.ItemIndex + 1;
  LineParam.Color := Form1.ColorBox1.Selected;
  LineParam.Style := psSolid;

  InitGraph(Image1.Canvas, Image1.Width, Image1.Height);

  PlotXAxis(Image1.Canvas);
  PlotYAxis(Image1.Canvas);
  PlotGrid(Image1.Canvas);

  PlotFunc(Image1.Canvas, Func, XAxis.Min, XAxis.Max, Npts, LineParam);
end;

procedure TForm1.Button2Click(Sender: TObject);
{ Clear graphic }
begin
  with Image1 do
    Canvas.FillRect(Rect(0, 0, Width, Height));
end;

begin
  Xwin1 := 10;
  Ywin1 := 10;
  Xwin2 := 95;
  Ywin2 := 90;
end.
