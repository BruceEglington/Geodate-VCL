object Form1: TForm1
  Left = 205
  Top = 179
  Width = 783
  Height = 531
  Caption = 'Function plotter'
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
    Left = 264
    Top = 0
    Width = 505
    Height = 497
  end
  object Label1: TLabel
    Left = 128
    Top = 232
    Width = 56
    Height = 13
    Caption = 'Select color'
  end
  object Label2: TLabel
    Left = 128
    Top = 368
    Width = 18
    Height = 13
    Caption = 'Plot'
  end
  object Label3: TLabel
    Left = 216
    Top = 368
    Width = 28
    Height = 13
    Caption = 'points'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 105
    Height = 489
    Caption = 'Select function'
    ItemIndex = 19
    Items.Strings = (
      'Expo(X)'
      'Exp2(X)'
      'Exp10(X)'
      'Power(X, Y)'
      'Log(X)'
      'Log2(X)'
      'Log10(X)'
      'Sin(X)'
      'Cos(X)'
      'Tan(X)'
      'ArcSin(X)'
      'ArcCos(X)'
      'ArcTan(X)'
      'Sinh(X)'
      'Cosh(X)'
      'Tanh(X)'
      'ArcSinh(X)'
      'ArcCosh(X)'
      'ArcTanh(X)'
      'Gamma(X)'
      'IGamma(A, X)'
      'Beta(X, Y)'
      'IBeta(A, B, X)'
      'Erf(X)')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object GroupBox1: TGroupBox
    Left = 128
    Top = 8
    Width = 121
    Height = 105
    Caption = 'X Axis'
    TabOrder = 1
    object LabeledEdit1: TLabeledEdit
      Left = 32
      Top = 24
      Width = 73
      Height = 21
      EditLabel.Width = 17
      EditLabel.Height = 13
      EditLabel.Caption = 'Min'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 0
      Text = '0'
    end
    object LabeledEdit2: TLabeledEdit
      Left = 32
      Top = 48
      Width = 73
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Max'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 1
      Text = '5'
    end
    object LabeledEdit3: TLabeledEdit
      Left = 32
      Top = 72
      Width = 73
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Step'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 2
      Text = '1'
    end
  end
  object ColorBox1: TColorBox
    Left = 128
    Top = 248
    Width = 121
    Height = 22
    Selected = clRed
    Style = [cbStandardColors, cbPrettyNames]
    ItemHeight = 16
    TabOrder = 3
  end
  object RadioGroup2: TRadioGroup
    Left = 128
    Top = 280
    Width = 121
    Height = 73
    Caption = 'Select width'
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
    TabOrder = 4
  end
  object Button1: TButton
    Left = 128
    Top = 392
    Width = 57
    Height = 25
    Caption = 'Plot'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 192
    Top = 392
    Width = 57
    Height = 25
    Caption = 'Clear'
    TabOrder = 7
    OnClick = Button2Click
  end
  object GroupBox2: TGroupBox
    Left = 128
    Top = 120
    Width = 121
    Height = 105
    Caption = 'Y Axis'
    TabOrder = 2
    object LabeledEdit4: TLabeledEdit
      Left = 32
      Top = 24
      Width = 73
      Height = 21
      EditLabel.Width = 17
      EditLabel.Height = 13
      EditLabel.Caption = 'Min'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 0
      Text = '0'
    end
    object LabeledEdit5: TLabeledEdit
      Left = 32
      Top = 48
      Width = 73
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Max'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 1
      Text = '5'
    end
    object LabeledEdit6: TLabeledEdit
      Left = 32
      Top = 72
      Width = 73
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Step'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 2
      Text = '1'
    end
  end
  object SpinEdit1: TSpinEdit
    Left = 152
    Top = 363
    Width = 57
    Height = 22
    Increment = 100
    MaxValue = 1000
    MinValue = 100
    TabOrder = 5
    Value = 200
  end
  object GroupBox3: TGroupBox
    Left = 128
    Top = 424
    Width = 121
    Height = 73
    Caption = 'Parameters ( > 0 )'
    TabOrder = 8
    Visible = False
    object LabeledEdit7: TLabeledEdit
      Left = 16
      Top = 16
      Width = 33
      Height = 21
      EditLabel.Width = 7
      EditLabel.Height = 13
      EditLabel.Caption = 'Y'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 0
      Text = '0.5'
    end
    object LabeledEdit8: TLabeledEdit
      Left = 16
      Top = 40
      Width = 33
      Height = 21
      EditLabel.Width = 7
      EditLabel.Height = 13
      EditLabel.Caption = 'A'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 1
      Text = '0.5'
    end
    object LabeledEdit9: TLabeledEdit
      Left = 80
      Top = 40
      Width = 33
      Height = 21
      EditLabel.Width = 7
      EditLabel.Height = 13
      EditLabel.Caption = 'B'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 2
      Text = '0.5'
    end
  end
end
