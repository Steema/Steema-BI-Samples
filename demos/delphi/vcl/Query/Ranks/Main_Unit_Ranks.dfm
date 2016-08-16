object FormDataRank: TFormDataRank
  Left = 0
  Top = 0
  Caption = 'TeeBI - Data Rank Example'
  ClientHeight = 585
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 453
    Height = 544
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 40
    ExplicitTop = 72
    ExplicitWidth = 505
    ExplicitHeight = 481
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 453
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 185
    object CreateRank: TButton
      Left = 24
      Top = 9
      Width = 89
      Height = 25
      Caption = 'Create RANK'
      TabOrder = 0
      OnClick = CreateRankClick
    end
    object Button2: TButton
      Left = 136
      Top = 9
      Width = 169
      Height = 25
      Caption = 'Sort by Year and Happiness'
      TabOrder = 1
      OnClick = Button2Click
    end
    object CBHideYears: TCheckBox
      Left = 320
      Top = 13
      Width = 121
      Height = 17
      Caption = 'Hide duplicate Years'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CBHideYearsClick
    end
  end
end
