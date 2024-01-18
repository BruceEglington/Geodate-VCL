object Form1: TForm1
  Left = 242
  Top = 200
  Width = 697
  Height = 485
  Caption = 'Fast Fourier Transform'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 176
    Top = 0
    Width = 513
    Height = 457
  end
  object RichEdit1: TRichEdit
    Left = 144
    Top = 0
    Width = 545
    Height = 457
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 105
    Height = 25
    Caption = 'View Output File'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 56
    Width = 105
    Height = 25
    Caption = 'Plot Raw Signal'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 96
    Width = 105
    Height = 25
    Caption = 'Plot FFT'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 16
    Top = 136
    Width = 105
    Height = 25
    Caption = 'Plot Filtered Signal'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 16
    Top = 416
    Width = 105
    Height = 25
    Caption = 'Quit'
    TabOrder = 4
    OnClick = Button5Click
  end
end
