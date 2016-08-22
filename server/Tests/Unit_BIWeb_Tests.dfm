object FormBIWebTests: TFormBIWebTests
  Left = 0
  Top = 0
  Caption = 'TeeBI Web Server - Unit Tests'
  ClientHeight = 676
  ClientWidth = 1069
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1069
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = -6
    object Label1: TLabel
      Left = 304
      Top = 14
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object LTime: TLabel
      Left = 664
      Top = 14
      Width = 3
      Height = 13
    end
    object BRun: TButton
      Left = 24
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Run !'
      TabOrder = 0
      OnClick = BRunClick
    end
    object Button1: TButton
      Left = 144
      Top = 9
      Width = 137
      Height = 25
      Caption = 'Generate HTML page...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object CBHost: TComboBox
      Left = 351
      Top = 11
      Width = 145
      Height = 21
      TabOrder = 2
      OnChange = CBHostChange
      Items.Strings = (
        'localhost:15015'
        'steema.cat:15015')
    end
  end
  object LBTest: TListBox
    Left = 0
    Top = 41
    Width = 473
    Height = 594
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBTestClick
    ExplicitHeight = 425
  end
  object BIGrid1: TBIGrid
    Left = 473
    Top = 41
    Width = 596
    Height = 594
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
    ExplicitLeft = 384
    ExplicitTop = 296
    ExplicitWidth = 320
    ExplicitHeight = 120
  end
  object Panel2: TPanel
    Left = 0
    Top = 635
    Width = 1069
    Height = 41
    Align = alBottom
    TabOrder = 3
    ExplicitLeft = 448
    ExplicitTop = 336
    ExplicitWidth = 185
    object EURL: TEdit
      Left = 160
      Top = 11
      Width = 811
      Height = 21
      TabOrder = 0
    end
    object BOpenURL: TButton
      Left = 16
      Top = 9
      Width = 129
      Height = 25
      Caption = '&Open in browser...'
      Enabled = False
      TabOrder = 1
      OnClick = BOpenURLClick
    end
  end
  object SaveDialog1: TSaveDialog
    FileName = 'index.htm'
    Filter = 'HTML files|*.htm;*.html'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'File name to generate BIWeb HTML tests'
    Left = 536
    Top = 416
  end
end
