object BIRConsole: TBIRConsole
  Left = 518
  Top = 251
  Width = 651
  Height = 338
  Caption = 'R Console'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 223
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 264
    Width = 635
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object EStatement: TEdit
      Left = 6
      Top = 6
      Width = 536
      Height = 21
      TabOrder = 0
      OnChange = EStatementChange
      OnKeyPress = EStatementKeyPress
    end
    object Panel2: TPanel
      Left = 536
      Top = 0
      Width = 99
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object BGo: TButton
        Left = 16
        Top = 4
        Width = 75
        Height = 25
        Caption = '&Go'
        Enabled = False
        TabOrder = 0
        OnClick = BGoClick
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Clear'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
