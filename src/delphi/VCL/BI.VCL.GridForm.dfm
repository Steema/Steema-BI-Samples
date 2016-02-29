object BIGridForm: TBIGridForm
  Left = 0
  Top = 0
  ClientHeight = 496
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelNav: TPanel
    Left = 0
    Top = 462
    Width = 524
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      Left = 113
      Top = 0
      Width = 411
      Height = 34
      DataSource = DataSource1
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
      Align = alClient
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 113
      Height = 34
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object LRow: TLabel
        Left = 8
        Top = 11
        Width = 16
        Height = 13
        Caption = '0/0'
      end
    end
  end
  object Grid: TBIGrid
    Left = 0
    Top = 0
    Width = 524
    Height = 462
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    TabOrder = 1
    DataSource = DataSource1
  end
  object DataSource1: TDataSource
    DataSet = BIDataset1
    OnDataChange = DataSource1DataChange
    Left = 224
    Top = 264
  end
  object BIDataset1: TBIDataset
    Left = 304
    Top = 264
  end
end
