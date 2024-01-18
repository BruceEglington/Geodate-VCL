{==========================================================================

    testfft.pas  -  Don Cross <dcross@intersrv.com>

    Modified by Jean Debord <JDebord@compuserve.com> for use with Delphi.

    This program is a test/demo for the file 'fourier.pas'.
    Get the latest version of 'fourier.pas' and 'testfft.pas' at the
    following URL.

       http://www.intersrv.com/~dcross/fft.html#pascal

    NOTE:  You may need to modify the const string 'PathToBGI' to point
           to the correct drive and subdirectory for the BGI drivers on
           your computer, in order for the graphics to work.

    ---------------   What this program does -------------------------

    First, it generates a time signal consisting of a large 200 Hz sine
    wave added to a small 2000 Hz cosine wave, which is graphed on the
    screen.  (Press ENTER after you are done viewing each graph.)

    Next, it performs the FFT and graphs the resulting complex
    frequency samples.

    Then, it filters out all frequency components above 1000 Hz in
    the transformed data.

    Finally, it performs the inverse transform to get a filtered
    time signal back, and graphs the result.

    ------------------------ Revision history ------------------------

2002 April 8 [Jean Debord]
     Turned into a Delphi 6 application

1997 March 1 [Jean Debord]
     Modifications for use with the TP Math library:
    1. Added a USES clause for the TP Math units.
    2. Set real type to Float (defined in FMATH.PAS) which means Real,
       Single, Double or Extended, according to the compiler directives.
    3. Changed array types to those defined in TP Math. Modified array
       allocation, deallocation and reference accordingly.
    4. Removed compiler directives, which were no longer necessary.
    5. Modified some typographical and formatting options so that the
       code looks like the other TP Math units.
    No modification was made to the original algorithm.

1996 December 12 [Don Cross]
    Added code to test the new procedure Fourier.CalcFrequency.
    Cleaned up some comments.
    Added code to preserve the original text mode.

1996 November 17 [Don Cross]
    Wrote and debugged first version.

==========================================================================}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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
  fmath, matrices, fourier, plotvar, winplot;

const
  NumSamples   = 512;                        { Buffer size must be power of 2 }
  SamplingRate = 22050;                      { Sampling rate in Hz }
  MaxIndex     = NumSamples - 1;             { Max. array index }
  FreqUnit     = SamplingRate / NumSamples;  { Frequency unit }

var
  T, Freq            : TVector;  { Time and frequency }
  RealIn1, RealIn2   : TVector;  { Signals (raw and filtered) }
  ImagIn             : TVector;  { Imaginary part of signal (= 0) }
  RealOut1, ImagOut1 : TVector;  { FFT of raw signal }
  RealOut2, ImagOut2 : TVector;  { FFT of filtered signal }
  OutputListingFile  : Text;
  I, FreqIndex       : Integer;
  CurvParam          : TCurvParamVector;

  function F(T : Float) : Float;
  begin
    F := Sin(200 * 2 * PI * T) + 0.2 * Cos(2000 * 2 * PI * T);
  end;

  procedure Test_CalcFrequency;
  var
    Yr, Yi                           : Float;
    I                                : Integer;
    RealIn, ImagIn, RealOut, ImagOut : TVector;

  begin
    DimVector(RealIn, MaxIndex);
    DimVector(ImagIn, MaxIndex);

    DimVector(RealOut, MaxIndex);
    DimVector(ImagOut, MaxIndex);

    { Fill input buffers with random data }
    for I := 0 to MaxIndex do
      begin
        RealIn[I] := Random(10000);
        ImagIn[I] := Random(10000);
      end;

    WriteLn(OutputListingFile);
    WriteLn(OutputListingFile, '*** Testing procedure CalcFrequency ***');
    WriteLn(OutputListingFile);

    FFT(NumSamples, RealIn, ImagIn, RealOut, ImagOut);
    for I := 0 to MaxIndex do
      begin
        CalcFrequency(NumSamples, I, RealIn, ImagIn, Yr, Yi);
        WriteLn(OutputListingFile, I:4,
                RealOut[I]:15:6, Yr:15:6,
                ImagOut[I]:20:6, Yi:15:6);
      end;
  end;

  procedure ListData(RealData, ImagData : TVector; Comment : String);
  var
    I : Integer;
  begin
    WriteLn(OutputListingFile, '*** ', Comment, ' ***');
    WriteLn(OutputListingFile);
    WriteLn(OutputListingFile, 'index':20, 'real':20, 'imag':20);

    for I := 0 to MaxIndex do
      begin
        WriteLn(OutputListingFile, I:20,
                RealData[I]:20:5, ImagData[I]:20:5);
      end;

    WriteLn(OutputListingFile);
    WriteLn(OutputListingFile, '------------------------------------------------------------------------');
    WriteLn(OutputListingFile);
  end;

  procedure ClearGraphic;
  begin
    with Form1.Image1 do
      Canvas.FillRect(Rect(0, 0, Width, Height));
  end;

  procedure TForm1.Button1Click(Sender: TObject);
  { View Output File }
  begin
    Form1.Image1.Visible := False;
    Form1.RichEdit1.Visible := True;
    Form1.RichEdit1.Lines.LoadFromFile('fftout.txt');
  end;

  procedure TForm1.Button2Click(Sender: TObject);
  { Plot Raw Signal }
  begin
    Form1.Image1.Visible := True;
    Form1.RichEdit1.Visible := False;

    AutoScale(T, 0, MaxIndex, XAxis);
    AutoScale(RealIn1, 0, MaxIndex, YAxis);

    XAxis.Title := 'Time (s)';
    YAxis.Title := 'Amplitude';

    ClearGraphic;

    InitGraph(Image1.Canvas, Image1.Width, Image1.Height);

    PlotXAxis(Image1.Canvas);
    PlotYAxis(Image1.Canvas);
    PlotGrid(Image1.Canvas);

    PlotCurve(Image1.Canvas, T, RealIn1, 0, MaxIndex, CurvParam[1]);
  end;

  procedure TForm1.Button3Click(Sender: TObject);
  { Plot FFT }
  begin
    Form1.Image1.Visible := True;
    Form1.RichEdit1.Visible := False;

    AutoScale(Freq, 0, MaxIndex div 2, XAxis);
    AutoScale(ImagOut1, 0, MaxIndex, YAxis);

    XAxis.Title := 'Frequency (Hz)';
    YAxis.Title := 'FFT';

    ClearGraphic;

    InitGraph(Image1.Canvas, Image1.Width, Image1.Height);

    PlotXAxis(Image1.Canvas);
    PlotYAxis(Image1.Canvas);
    PlotGrid(Image1.Canvas);

    PlotCurve(Image1.Canvas, Freq, RealOut1, 0, MaxIndex, CurvParam[1]);
    PlotCurve(Image1.Canvas, Freq, ImagOut1, 0, MaxIndex, CurvParam[2]);

    WriteLegend(Image1.Canvas, 2, CurvParam, False, True);
  end;

  procedure TForm1.Button4Click(Sender: TObject);
  { Plot Filtered Signal }
  begin
    Form1.Image1.Visible := True;
    Form1.RichEdit1.Visible := False;

    AutoScale(T, 0, MaxIndex, XAxis);
    AutoScale(RealIn2, 0, MaxIndex, YAxis);

    XAxis.Title := 'Time (s)';
    YAxis.Title := 'Amplitude';

    ClearGraphic;

    InitGraph(Image1.Canvas, Image1.Width, Image1.Height);

    PlotXAxis(Image1.Canvas);
    PlotYAxis(Image1.Canvas);
    PlotGrid(Image1.Canvas);

    PlotCurve(Image1.Canvas, T, RealIn2, 0, MaxIndex, CurvParam[1]);
  end;

  procedure TForm1.Button5Click(Sender: TObject);
  { Quit program }
  begin
    Form1.Close;
  end;

begin
  { Dimension arrays }
  DimVector(T, MaxIndex);
  DimVector(Freq, MaxIndex);
  DimVector(RealIn1, MaxIndex);
  DimVector(RealIn2, MaxIndex);
  DimVector(ImagIn, MaxIndex);
  DimVector(RealOut1, MaxIndex);
  DimVector(ImagOut1, MaxIndex);
  DimVector(RealOut2, MaxIndex);
  DimVector(ImagOut2, MaxIndex);
  DimCurvParamVector(CurvParam, 2);

  { Initialize graphic parameters }
  Xwin1 := 10;
  Xwin2 := 80;
  Ywin1 := 10;
  Ywin2 := 90;

  for I := 1 to 2 do
    with CurvParam[I] do
      begin
        PointParam.Symbol := 0;
        Connect := True;
      end;

  CurvParam[1].Legend := 'Real';
  CurvParam[2].Legend := 'Imag.';

  { Open output file }
  Assign(OutputListingFile, 'fftout.txt');
  Rewrite(OutputListingFile);

  { Compute raw signal and frequencies }
  for I := 0 to MaxIndex do
    begin
      T[I] := I / SamplingRate;
      Freq[I] := I * FreqUnit;
      RealIn1[I] := F(T[I]);
      ImagIn[I] := 0.0;
    end;

  ListData(RealIn1, ImagIn, 'Time domain data before transform');

  { Perform FFT }
  FFT(NumSamples, RealIn1, ImagIn, RealOut1, ImagOut1);

  ListData(RealOut1, ImagOut1, 'Frequency domain data after transform');

  { Filter out everything above 1000 Hz (low-pass) }
  FreqIndex := Trunc(1000.0 / FreqUnit);
  for I := 0 to MaxIndex do
    if ((I > FreqIndex) and (I < NumSamples div 2)) or
    ((I >= NumSamples div 2) and (I < NumSamples - FreqIndex)) then
      begin
        RealOut2[I] := 0.0;
        ImagOut2[I] := 0.0;
      end
    else
      begin
        RealOut2[I] := RealOut1[I];
        ImagOut2[I] := ImagOut1[I];
      end;

  { Compute filtered signal }
  IFFT(NumSamples, RealOut2, ImagOut2, RealIn2, ImagIn);
  ListData(RealIn2, ImagIn, 'Time domain data after inverse transform');

  Test_CalcFrequency;

  Close(OutputListingFile);
end.
