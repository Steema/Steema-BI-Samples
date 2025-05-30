object DataLinksEditor: TDataLinksEditor
  Left = 0
  Top = 0
  Caption = 'DataLinksEditor'
  ClientHeight = 400
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 413
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 312
    object BAdd: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Add'
      TabOrder = 0
      OnClick = BAddClick
    end
    object BDelete: TButton
      Left = 120
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 1
      OnClick = BDeleteClick
    end
  end
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 413
    Height = 256
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    OnDataChange = BIGrid1DataChange
    ExplicitTop = 39
  end
  object PanelLink: TPanel
    Left = 0
    Top = 297
    Width = 413
    Height = 103
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 196
    ExplicitWidth = 312
    object Label3: TLabel
      Left = 11
      Top = 16
      Width = 37
      Height = 13
      Caption = 'Master:'
    end
    object Label4: TLabel
      Left = 11
      Top = 48
      Width = 31
      Height = 13
      Caption = 'Detail:'
    end
    object SpeedButton1: TSpeedButton
      Left = 271
      Top = 12
      Width = 23
      Height = 22
      Caption = '...'
      Flat = True
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 271
      Top = 44
      Width = 23
      Height = 22
      Caption = '...'
      Flat = True
      OnClick = SpeedButton2Click
    end
    object EMaster: TEdit
      Left = 54
      Top = 13
      Width = 211
      Height = 21
      TabOrder = 0
      OnChange = EMasterChange
    end
    object EDetail: TEdit
      Left = 54
      Top = 45
      Width = 211
      Height = 21
      TabOrder = 1
      OnChange = EDetailChange
    end
  end
end
