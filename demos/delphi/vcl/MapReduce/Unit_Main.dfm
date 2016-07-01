object DemoForm: TDemoForm
  Left = 0
  Top = 0
  Caption = 'TeeBI - Map Reduce Example'
  ClientHeight = 677
  ClientWidth = 1085
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1085
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 54
      Height = 13
      Caption = 'Total rows:'
    end
    object LabelRows: TLabel
      Left = 76
      Top = 14
      Width = 3
      Height = 13
    end
    object LabelTime: TLabel
      Left = 528
      Top = 16
      Width = 3
      Height = 13
    end
    object CBParallel: TCheckBox
      Left = 237
      Top = 13
      Width = 97
      Height = 17
      Caption = '&Parallel'
      TabOrder = 0
      OnClick = CBParallelClick
    end
  end
  object PageControl1: TPageControl
    Left = 233
    Top = 41
    Width = 852
    Height = 636
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Output'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 844
        Height = 608
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      object BIGrid2: TBIGrid
        Left = 0
        Top = 0
        Width = 844
        Height = 608
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        Alternate.Enabled = True
      end
    end
  end
  object LBTest: TListBox
    Left = 0
    Top = 41
    Width = 233
    Height = 636
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      'Count by Year'
      'Average Rating by Year'
      'Average Votes/Length by Year'
      'Map For All'
      'Custom')
    TabOrder = 2
    OnClick = LBTestClick
  end
end
