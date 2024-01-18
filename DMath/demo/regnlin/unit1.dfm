object Form1: TForm1
  Left = 215
  Top = 119
  Width = 761
  Height = 580
  Caption = 'Linear / Nonlinear Regression'
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
    Left = 200
    Top = 0
    Width = 553
    Height = 553
  end
  object RichEdit1: TRichEdit
    Left = 200
    Top = 0
    Width = 553
    Height = 553
    TabStop = False
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 11
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = 'Read Data File'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 200
    Width = 89
    Height = 25
    Caption = 'Compute'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 104
    Top = 200
    Width = 89
    Height = 25
    Caption = 'View Results'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 488
    Width = 89
    Height = 25
    Caption = 'Plot Graph'
    TabOrder = 8
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 104
    Top = 488
    Width = 89
    Height = 25
    Caption = 'Print Graph'
    TabOrder = 9
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 520
    Width = 185
    Height = 25
    Caption = 'Quit'
    TabOrder = 10
    OnClick = Button6Click
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 232
    Width = 89
    Height = 129
    Caption = 'X Axis'
    TabOrder = 4
    object LabeledEdit1: TLabeledEdit
      Left = 32
      Top = 24
      Width = 49
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
      Width = 49
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
      Width = 49
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Step'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 2
      Text = '1'
    end
    object LabeledEdit7: TLabeledEdit
      Left = 32
      Top = 96
      Width = 49
      Height = 21
      EditLabel.Width = 21
      EditLabel.Height = 13
      EditLabel.Caption = 'Text'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 3
      Text = 'X'
    end
  end
  object GroupBox3: TGroupBox
    Left = 104
    Top = 232
    Width = 89
    Height = 129
    Caption = 'Y Axis'
    TabOrder = 5
    object LabeledEdit4: TLabeledEdit
      Left = 32
      Top = 24
      Width = 49
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
      Width = 49
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
      Width = 49
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Step'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 2
      Text = '1'
    end
    object LabeledEdit8: TLabeledEdit
      Left = 32
      Top = 96
      Width = 49
      Height = 21
      EditLabel.Width = 21
      EditLabel.Height = 13
      EditLabel.Caption = 'Text'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 3
      Text = 'Y'
    end
  end
  object LabeledEdit9: TLabeledEdit
    Left = 56
    Top = 368
    Width = 137
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'Graph title'
    LabelPosition = lpLeft
    LabelSpacing = 3
    TabOrder = 6
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 185
    Height = 153
    Caption = 'Regression model'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 56
      Width = 97
      Height = 13
      Caption = 'Degree of numerator'
      Visible = False
    end
    object Label2: TLabel
      Left = 8
      Top = 88
      Width = 108
      Height = 13
      Caption = 'Degree of denominator'
      Visible = False
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 20
      Width = 169
      Height = 21
      DropDownCount = 11
      ItemHeight = 13
      TabOrder = 0
      Text = 'Linear'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Linear'
        'Polynomial'
        'Rational fraction'
        'Sum of exponentials'
        'Increasing exponential'
        'Exponential + linear'
        'Power'
        'Michaelis'
        'Integrated Michaelis'
        'Hill'
        'Logistic'
        'Acid/Base titration curve')
    end
    object SpinEdit1: TSpinEdit
      Left = 128
      Top = 51
      Width = 49
      Height = 22
      MaxValue = 10
      MinValue = 1
      TabOrder = 1
      Value = 1
      Visible = False
    end
    object SpinEdit2: TSpinEdit
      Left = 128
      Top = 83
      Width = 49
      Height = 22
      MaxValue = 10
      MinValue = 1
      TabOrder = 2
      Value = 1
      Visible = False
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 120
      Width = 169
      Height = 17
      Caption = 'Constant term'
      TabOrder = 3
      Visible = False
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 48
      Width = 49
      Height = 65
      Caption = 'X var.'
      ItemIndex = 0
      Items.Strings = (
        't'
        's0'
        'e0')
      TabOrder = 4
      Visible = False
      OnClick = RadioGroup1Click
    end
    object LabeledEdit10: TLabeledEdit
      Left = 128
      Top = 56
      Width = 49
      Height = 21
      EditLabel.Width = 11
      EditLabel.Height = 13
      EditLabel.Caption = 's0'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 5
      Text = '1'
      Visible = False
    end
    object LabeledEdit11: TLabeledEdit
      Left = 128
      Top = 88
      Width = 49
      Height = 21
      EditLabel.Width = 12
      EditLabel.Height = 13
      EditLabel.Caption = 'e0'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 6
      Text = '1'
      Visible = False
    end
    object LabeledEdit12: TLabeledEdit
      Left = 128
      Top = 120
      Width = 49
      Height = 21
      EditLabel.Width = 8
      EditLabel.Height = 13
      EditLabel.Caption = 'H'
      LabelPosition = lpLeft
      LabelSpacing = 3
      TabOrder = 7
      Text = '1'
      Visible = False
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 120
      Width = 41
      Height = 17
      Caption = 'Fit H'
      TabOrder = 8
      Visible = False
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 88
      Width = 113
      Height = 17
      Caption = 'Generalized logistic'
      TabOrder = 9
      Visible = False
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 400
    Width = 185
    Height = 81
    Caption = 'Plotted curve'
    TabOrder = 7
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 54
      Height = 13
      Caption = 'Curve color'
    end
    object Label4: TLabel
      Left = 8
      Top = 52
      Width = 80
      Height = 13
      Caption = 'Number of points'
    end
    object ColorBox1: TColorBox
      Left = 96
      Top = 19
      Width = 81
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object SpinEdit3: TSpinEdit
      Left = 96
      Top = 47
      Width = 81
      Height = 22
      Increment = 100
      MaxValue = 1000
      MinValue = 100
      TabOrder = 1
      Value = 200
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dat'
    Filter = 'Data files|*.dat'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 208
    Top = 8
  end
end
