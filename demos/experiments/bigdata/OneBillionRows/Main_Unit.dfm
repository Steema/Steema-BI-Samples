object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeBI - One Billion Row big data'
  ClientHeight = 278
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object LFileName: TLabel
    Left = 52
    Top = 108
    Width = 58
    Height = 15
    Caption = 'big_data.bi'
  end
  object LLoadTime: TLabel
    Left = 280
    Top = 72
    Width = 3
    Height = 15
  end
  object BCreate: TButton
    Left = 52
    Top = 24
    Width = 213
    Height = 25
    Caption = 'Create One Billion Row data'
    TabOrder = 0
    OnClick = BCreateClick
  end
  object BLoad: TButton
    Left = 52
    Top = 68
    Width = 213
    Height = 25
    Caption = 'Load data from disk'
    Enabled = False
    TabOrder = 1
    OnClick = BLoadClick
  end
  object Button3: TButton
    Left = 124
    Top = 212
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = Button3Click
  end
  object BView: TButton
    Left = 192
    Top = 148
    Width = 73
    Height = 25
    Caption = 'Raw Data...'
    Enabled = False
    TabOrder = 3
    OnClick = BViewClick
  end
  object BQuery: TButton
    Left = 52
    Top = 148
    Width = 124
    Height = 25
    Caption = 'Query and Visualize...'
    Enabled = False
    TabOrder = 4
  end
end
