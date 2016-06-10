object FormManual: TFormManual
  Left = 0
  Top = 0
  Caption = 'TeeBI - TDataItem manual example'
  ClientHeight = 534
  ClientWidth = 935
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
  object Splitter1: TSplitter
    Left = 329
    Top = 41
    Height = 493
    ExplicitLeft = 472
    ExplicitTop = 240
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 935
    Height = 41
    Align = alTop
    Caption = 'Creating different kinds of TDataItem manually'
    TabOrder = 0
    ExplicitLeft = 248
    ExplicitTop = 264
    ExplicitWidth = 185
  end
  object LBExample: TListBox
    Left = 0
    Top = 41
    Width = 329
    Height = 493
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      'Simple column'
      'Simple table'
      'Group of tables'
      'Nested table'
      'Master Detail'
      'Master Detail Embedded')
    TabOrder = 1
    OnClick = LBExampleClick
  end
  object PageControl1: TPageControl
    Left = 332
    Top = 41
    Width = 603
    Height = 493
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Preview'
      ExplicitLeft = 6
      ExplicitTop = 16
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter2: TSplitter
        Left = 0
        Top = 173
        Width = 595
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 0
        ExplicitWidth = 345
      end
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 595
        Height = 173
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        ExplicitLeft = -1
        ExplicitHeight = 465
      end
      object BIGrid2: TBIGrid
        Left = 0
        Top = 176
        Width = 595
        Height = 289
        Align = alBottom
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
        Visible = False
      end
    end
  end
end
