object FormSpeed: TFormSpeed
  Left = 0
  Top = 0
  Caption = 'TeeBI Speed Benchmark Tests'
  ClientHeight = 397
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 851
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 424
      Top = 13
      Width = 128
      Height = 13
      Caption = 'Results copied to clipboard'
      Visible = False
    end
    object LTotal: TLabel
      Left = 111
      Top = 13
      Width = 3
      Height = 13
    end
    object Button1: TButton
      Left = 24
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Begin Test '
      TabOrder = 0
      OnClick = Button1Click
    end
    object BExport: TButton
      Left = 336
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Export'
      Enabled = False
      TabOrder = 1
      OnClick = BExportClick
    end
    object CheckBox1: TCheckBox
      Left = 584
      Top = 12
      Width = 137
      Height = 17
      Caption = 'Use multiple CPUs'
      TabOrder = 2
      Visible = False
      OnClick = CheckBox1Click
    end
  end
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 851
    Height = 356
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
  end
end
